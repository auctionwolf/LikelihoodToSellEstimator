USE [PredictiveData]
GO

/****** Object:  StoredProcedure [dbo].[DataScience_TrusteePredictionData]    Script Date: 1/29/2014 8:11:38 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





-- =============================================
-- Author:		Wolf Rendall, wrendall@auction.com
-- Create date: 2014-01-06
-- Edit:		2014-01-15
-- Last Edit:	2014-01-29
-- Description:	Final Table for Trustee Asset Prediction
-- =============================================
CREATE PROCEDURE [dbo].[DataScience_TrusteePredictionData] 

AS
BEGIN
--Declare Variables
        DeclareVariables:    
        DECLARE @5MonthsToToday DATETIME ,
            @3MonthsToToday DATETIME ,
            @Today DATETIME ,
            @ALRInceptionDate DATETIME ,
            @BPOReserveRatio DECIMAL ,
            @BPOAbsurdityLowerBound DECIMAL ,
            @ZIPPopulationDefault DECIMAL ,
            @ZIPVacancyRateDefault DECIMAL,
			@BPOReserveSurplusDefault DECIMAL

        SET @Today = {T '00:00:00'}
        SET @5MonthsToToday = DATEADD(MONTH, -5, @Today)
        SET @3MonthsToToday = DATEADD(MONTH, -3, @Today)
        SET @ALRInceptionDate = '2013-07-01'
        SET @BPOReserveRatio = 1.2
		SET @BPOReserveSurplusDefault = 0.2
        SET @BPOAbsurdityLowerBound = 0.15
        SET @ZIPPopulationDefault = 8000
        SET @ZIPVacancyRateDefault = 0.087

-- SET NOCOUNT ON added to prevent extra result sets from
-- interfering with SELECT statements.
	SET NOCOUNT ON;

--Query to pull housing data for predictive model for all states

--Date range is 3 months prior
--Start by creating temporary tables and finish with one pull at the end
        IF OBJECT_ID('tempdb..#TemporaryFactTrusteeAuction') IS NOT NULL
            BEGIN
                DROP TABLE #TemporaryFactTrusteeAuction
            END
        IF OBJECT_ID('tempdb..#BroughtToSaleHistory') IS NOT NULL
            BEGIN
                DROP TABLE #BroughtToSaleHistory;
            END
        IF OBJECT_ID('tempdb..#Favorites') IS NOT NULL
            BEGIN
                DROP TABLE #Favorites;
            END
        IF OBJECT_ID('tempdb..#ADCExecution') IS NOT NULL
            BEGIN
                DROP TABLE #ADCExecution;
            END
        IF OBJECT_ID('tempdb..#TrusteePortfolioQuality') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePortfolioQuality;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionPreOutput') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionPreOutput;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionSourceTable') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionSourceTable;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionOutput_Auto') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionOutput_Auto;
            END
        IF OBJECT_ID('tempdb..#TrusteeNegativeFactors') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteeNegativeFactors;
            END
        IF OBJECT_ID('tempdb..#TrusteePropertyFaultPreOutput') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePropertyFaultPreOutput;
            END


--Start with FactTrusteeAuction as a base, but trim down the result set and add in an index to join on
SELECT
A.*
,FORMAT(A.AuctionDate, 'yyyy-MM-01') AS ZillowJoinField
,FORMAT(A.AuctionDate, 'yyyyMM') AS USPSJoinField
INTO
#TemporaryFactTrusteeAuction
FROM
EDW.DBO.FactTrusteeAuction A
LEFT JOIN	EDW.dbo.DimAuction DA	ON( DA.AuctionId = A.AuctionId)
WHERE	A.AuctionDate >= @5MonthsToToday
AND		A.AuctionDate IS NOT NULL
AND		DA.IsTest = 0
AND		DA.IsDemo = 0
ORDER BY AuctionDate 

CREATE CLUSTERED INDEX ZillowJoinField ON #TemporaryFactTrusteeAuction (ZillowJoinField);
CREATE NONCLUSTERED INDEX PropertyId ON #TemporaryFactTrusteeAuction (PropertyId); 
CREATE NONCLUSTERED INDEX VenueId ON #TemporaryFactTrusteeAuction (VenueId); 
CREATE NONCLUSTERED INDEX AuctionId ON #TemporaryFactTrusteeAuction (AuctionId); 
CREATE NONCLUSTERED INDEX USPSJoinField ON #TemporaryFactTrusteeAuction (USPSJoinField);

        AuctionExecution:
--========================================================================================================================
-- Historical Sell Through Rate Data (Auction.com Execution)
-- We use this as a measure of which ZIP codes we are strong in, and which we are weak in
-- Assets coming to us in an area where we have an established buyer pool should do better than those we don't
--========================================================================================================================
SELECT
DP.PropertyState
,DP.PropertyZip
,AVG(CAST(FTA.IsSold AS DECIMAL)) AS AvgSTR
,count(FTA.IsAuction) AS TotalAuctioned

INTO
#ADCExecution

FROM		#TemporaryFactTrusteeAuction					FTA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FTA.PropertyId)

WHERE	FTA.AuctionDate <= CURRENT_TIMESTAMP
AND		FTA.AuctionDate IS NOT NULL
AND		FTA.IsAuction = 1

GROUP BY
DP.PropertyState
,DP.PropertyZip

HAVING
COUNT(FTA.IsAuction) >= 5

ORDER BY
DP.PropertyState
,DP.PropertyZip

        SellerPortfolioQuality:
--========================================================================================================================
-- Historical Seller (Trustee) Portfolio Quality Data
-- We use this as a measure of which Seller Product Lines (Not Auction.com Product Lines, but a particular
-- group within a seller organization managing a certain class of assets) are strong performers
-- Assets coming to us from sellers with historically good portfolios should do better than those that don't
--========================================================================================================================
select
DT.TrusteeCode
,AVG(CAST(FTA.IsSold AS DECIMAL)) AS AvgSTR
,COUNT(FTA.IsAuction) AS TotalAuctioned

INTO
#TrusteePortfolioQuality

FROM		#TemporaryFactTrusteeAuction			FTA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FTA.PropertyId)
LEFT JOIN	EDW.dbo.DimTrustee						DT	ON(DT.TrusteeId = FTA.TrusteeId)


WHERE	FTA.AuctionDate >= @5MonthsToToday
AND		FTA.AuctionDate <= CURRENT_TIMESTAMP
AND		FTA.AuctionDate IS NOT NULL
AND		FTA.IsAuction = 1

GROUP BY
DT.TrusteeCode

HAVING
COUNT(FTA.IsAuction) >= 5

ORDER BY
DT.TrusteeCode

		PropertyBroughtToSaleHistory:
--========================================================================================================================
-- Property Historical Brought To Sale Outcomes (Indicates likelihood of Add'l postponement or cancelation
-- The Model makes predictions on likelihood to be postponed and likelihood to be cancelled
-- One of the largest indicators is previous cancellations, we use the following table to capture that value
--========================================================================================================================
SELECT
PropertyId
,SUM(IsAuction) + SUM(IsRemoved) + SUM(IsCancelled) + SUM(IsPostponed) AS RunNum
,SUM(IsRemoved) Removals
,SUM(IsCancelled) Cancellations
,SUM(IsPostponed) Postponements
INTO
#BroughtToSaleHistory
FROM
EDW.DBO.FactTrusteeAuction
GROUP BY
PropertyId

        PropertyFavorites:
--========================================================================================================================
-- Property Favorites Data
-- For each run of each asset (which we take as favorites per property per auctionid -- maps to AuctionCode)
--========================================================================================================================
SELECT
PropertyId
,AuctionId
,COUNT(distinct(UserId)) AS Favorites

INTO
#Favorites

FROM
EDW.dbo.FactPropertyFavorites

GROUP BY
propertyId
,AuctionId

ORDER BY
propertyId


        PredictionDataCollectionAndUnificationTable:
--========================================================================================================================
-- Building the Base Information Set
-- And now we unite all of this and the major auction information tables into a complete predictive set
--========================================================================================================================
SELECT
--Descriptive Information
DP.GlobalPropertyId
,DA.SystemAuctionId
,FTA.VenueId
,FTA.AuctionId
,FTA.PropertyId
,FTA.REDCID
,DP.LoanNbr
,DP.PropertyState
,DP.PropertyZip
,DP.PropertyCounty
,DP.PropertyCity
,DG.MetroArea as MSAName
,DA.AuctionCode
,DT.TrusteeCode
,DT.TrusteeName
,DG.Latitude
,DG.Longitude
,FTA.AuctionDate
,DP.PropertyOccupancyStatus
,CASE WHEN(DP.Condition LIKE '' or DP.Condition LIKE 'N/A') THEN 'unknown' ELSE DP.Condition END AS Condition
,DP.PropertyType
,DP.ProductType
,CASE WHEN(DP.PropertyType LIKE 'SF%' OR DP.PropertyType LIKE 'Single%' OR DP.PropertyType LIKE '%Town%') THEN 1 ELSE 0 END AS SFR_Flag
,CASE WHEN(DP.PropertyType LIKE '%Condo%' OR DP.PropertyType LIKE '%plex%') THEN 1 ELSE 0 END AS MultiResidence_Flag
,CASE WHEN(DP.ProductType LIKE '%C2C%') THEN 1 ELSE 0 END AS C2C_Flag
,CASE WHEN(DP.ProductType LIKE '%REO%') THEN 1 ELSE 0 END AS REO_Flag
,CASE WHEN(DP.ProductType LIKE '%Short Sale%') THEN 1 ELSE 0 END AS ShortSale_Flag
,CASE WHEN(DP.ProductType LIKE '%Trustee%') THEN 1 ELSE 0 END AS Trustee_Flag

--Win Criteria
,FTA.IsSold
,CASE	WHEN(COALESCE(FTA.WinningBid, FTA.HighBidAmt) >=  COALESCE(	FTA.AuctionSellerReserve,
			FTA.MaximumCreditBidAmnt,
			FTA.BeneficiaryAmount, 
			CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,
			FTA.TotalEstDebtorNOSAmount))
		THEN 1 ELSE 0 END AS AuctionReserveSuccess

--Historical Information
,FTA.HighBidAmt AS HighBid
,FTA.WinningBid
,HIST.RunNum
,HIST.Removals
,HIST.Cancellations
,HIST.Postponements
,SIGN(HIST.RunNum + HIST.Cancellations + HIST.Postponements -1) AS HasPrevAttempts

--Reserve Logic
,COALESCE(	FTA.AuctionSellerReserve,
			FTA.MaximumCreditBidAmnt,
			FTA.BeneficiaryAmount, 
			CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,
			FTA.TotalEstDebtorNOSAmount) AS Reserve

--Valuation ('BPO') Logic
,(CASE WHEN(COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) < 10) THEN NULL ELSE COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) END) AS BPO

--Valuation and Reserve Logic Intertwined
--Numerator BPO Minus Reserve Price = BPO Surplus
,((CASE WHEN(COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) < 10) THEN NULL ELSE COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) END) - COALESCE(	FTA.AuctionSellerReserve,
			FTA.MaximumCreditBidAmnt,
			FTA.BeneficiaryAmount, 
			CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,
			FTA.TotalEstDebtorNOSAmount)) AS BPOSurplus

--BPO Surplus Over Reserve

,((	(COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG)) / 
	(CASE WHEN(COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) < 10) THEN NULL ELSE COALESCE(FTA.BPO, FTA.InitialAVMValue, FTA.IVG) END)
	) 
-1) AS BPOSurplusPct

--Condition dummies require specific cases for easy math and condensing the scant 'excellent' assets into good
,CASE WHEN(DP.Condition = 'poor') THEN 1 ELSE 0 END AS Condition_poor
,CASE WHEN(DP.Condition = 'fair') THEN 1 ELSE 0 END AS Condition_fair
,CASE WHEN(DP.Condition = 'average') THEN 1 ELSE 0 END AS Condition_average
,CASE WHEN(DP.Condition = 'good' OR DP.Condition = 'excellent') THEN 1 ELSE 0 END AS Condition_good
,CASE WHEN(DP.Condition LIKE '' OR DP.Condition LIKE 'N/A') THEN 1 ELSE 0 END AS Condition_unknown

--Occupancy dummies require cases too
,CASE WHEN(DP.PropertyOccupancyStatus LIKE 'Occupied') THEN 1 ELSE 0 END AS PropertyOccupancyStatus_Occupied
,CASE WHEN(DP.PropertyOccupancyStatus LIKE '' OR DP.PropertyOccupancyStatus LIKE 'N/A') THEN 1 ELSE 0 END AS PropertyOccupancyStatus_Unknown
,CASE WHEN(DP.PropertyOccupancyStatus LIKE 'Vacant') THEN 1 ELSE 0 END AS PropertyOccupancyStatus_Vacant

--No Financing information Available

--USPS Data
,USPS.Active AS TotalActive
,USPS.Vacancy AS Vacancy

--Price Level Within State should be represented as dummies, Ask Paul if there is a way to avoid calling this function thrice (or more if more value levels are req'd            
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY COALESCE(FTA.AuctionSellerReserve,FTA.MaximumCreditBidAmnt,FTA.BeneficiaryAmount,CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,FTA.TotalEstDebtorNOSAmount) ASC) =1) THEN 1 ELSE 0 END AS ValueLevel_Low
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY COALESCE(FTA.AuctionSellerReserve,FTA.MaximumCreditBidAmnt,FTA.BeneficiaryAmount,CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,FTA.TotalEstDebtorNOSAmount) ASC) =2) THEN 1 ELSE 0 END AS ValueLevel_Med
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY COALESCE(FTA.AuctionSellerReserve,FTA.MaximumCreditBidAmnt,FTA.BeneficiaryAmount,CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,FTA.TotalEstDebtorNOSAmount) ASC) =3) THEN 1 ELSE 0 END AS ValueLevel_High


--Web Traffic Metrics
,ISNULL(TempF.Favorites,0) AS Favorites

--Zillow-Based Metrics
,COALESCE(Appreciation, CountyAppreciation, StateAppreciation, NationalAppreciation ) AS Appreciation
,COALESCE(Turnover, CountyTurnover, StateTurnover, NationalTurnover) AS Turnover
,COALESCE(TotalHomesForSale, CountyTotalHomesForSale, StateTotalHomesForSale, NationalTotalHomesForSale) AS TotalHomesForSale
,COALESCE(PercentHomesSold_12moTrailing, CountyPercentHomesSold_12moTrailing, StatePercentHomesSold_12moTrailing, NationalPercentHomesSold_12moTrailing) AS PercentHomesSold
,COALESCE(MedianPPSQFT, CountyMedianPPSQFT, StateMedianPPSQFT, NationalMedianPPSQFT) AS MedianPPSQFT
,(COALESCE(MedianPPSQFT, CountyMedianPPSQFT, StateMedianPPSQFT, NationalMedianPPSQFT) - (COALESCE(FTA.AuctionSellerReserve,FTA.MaximumCreditBidAmnt,FTA.BeneficiaryAmount,CASE WHEN(FTA.CreditBidType = 'Total Debt') THEN FTA.TotalEstDebtorNOSAmount ELSE NULL END,FTA.TotalEstDebtorNOSAmount) / 
					(CASE WHEN(DP.HomeSquareFootage < 10) THEN AVG(DP.HomeSquareFootage) OVER(PARTITION BY DP.PropertyState) ELSE DP.HomeSquareFootage END))) AS MedianPPSQFTDeviation
,COALESCE(ForeclosedHomesPer10000, CountyForeclosedHomesPer10000, StateForeclosedHomesPer10000, NationalForeclosedHomesPer10000) AS ForeclosedPer10000
,COALESCE(PercentSoldHomesPrevForeclosures_12moTrailing, CountyPrevForeclosureSold, StatePrevForeclosureSold, NationalPrevForeclosureSold) AS PrevForeclosureSoldRate

--More House Data Just Because
,DP.HomeSquareFootage
,DP.Bedrooms
,DP.Baths
,(2013 - DP.YearBuilt) AS Age
,DP.LotSize
,FTA.OpeningBid
,COUNT(DA.AuctionCode) OVER(PARTITION BY DA.AuctionCode) AS EventSize
,COUNT(DA.AuctionCode) OVER(PARTITION BY DA.AuctionCode, DP.PropertyState) AS StateEventSize
,ISNULL(AE.AvgSTR, 0) AS ADC_Historical_Execution
,ISNULL(PQ.AvgSTR,0) AS SellerPortfolioQuality
,FTA.IsAuction
,FTA.IsRemoved
,FTA.IsCancelled
,FTA.IsPostponed


INTO
#TrusteePredictionSourceTable	

FROM		#TemporaryFactTrusteeAuction					FTA
LEFT JOIN	EDW.dbo.DimProperty								DP	ON( DP.PropertyId = FTA.PropertyId)
LEFT JOIN	EDW.dbo.DimVenue								DV	ON( DV.VenueId = FTA.VenueId)
LEFT JOIN	EDW.dbo.DimAuction								DA	ON( DA.AuctionId = FTA.AuctionId)
LEFT JOIN	EDW.dbo.DimAuctionStatus						DAS	ON(DAS.AuctionStatusId = FTA.AuctionStatusId)
LEFT JOIN	EDW.dbo.DimGeo_New									DG	ON(DG.GeoId = FTA.GeoId)
LEFT JOIN	EDW.dbo.DimTrustee								DT	ON(DT.TrusteeId = FTA.TrusteeId)
LEFT JOIN	#Favorites										TempF ON(FTA.PropertyId = TempF.PropertyId AND FTA.AuctionId = TempF.AuctionId)
LEFT JOIN	PredictiveData.dbo.InterpolatedZillowMetrics	F	ON(DP.PropertyZip = F.PropertyZip AND Date = FTA.ZillowJoinField)
LEFT JOIN	PredictiveData.dbo.USPS_Vacancy					USPS  ON(DP.PropertyZip = USPS.ZIP AND USPS.Date = FTA.USPSJoinField)
LEFT JOIN	#ADCExecution									AE ON(DP.PropertyZip = AE.PropertyZip)
LEFT JOIN	#TrusteePortfolioQuality						PQ ON(DT.TrusteeCode = PQ.TrusteeCode)
LEFT JOIN	#BroughtToSaleHistory							HIST ON(HIST.PropertyId = FTA.PropertyId)

WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FTA.AuctionDate >= CURRENT_TIMESTAMP
--AND		FTA.AuctionDate >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
--AND		DA.AuctionEndDate >= CURRENT_TIMESTAMP


CREATE NONCLUSTERED INDEX PropertyId ON #TrusteePredictionSourceTable (PropertyId);
CREATE NONCLUSTERED INDEX VenueId ON #TrusteePredictionSourceTable (VenueId);
CREATE NONCLUSTERED INDEX PropertyState ON #TrusteePredictionSourceTable (PropertyState);



        PropertyFaultSuggestor:
--========================================================================================================================
-- Create a Fault Suggestor For This Set
-- Using the weights we used to create the predictions themselves, we rank the weighted contribution of each attribute for each property
-- We sort them from smallest to largest and return the 2 smallest (should be a negative number
-- We restrict the output so that "Reserve" itself is never the problem suggested, as this is not a useful output
--========================================================================================================================
SELECT
  GlobalPropertyId
  ,PropertyId
  ,VenueId
  ,AuctionId
  ,PropertyFaultVariable
  ,PropertyFaultName
  ,[Value] *CASE	WHEN(	NEG.PropertyFaultVariable = 'ADC_Historical_Execution') THEN FA.ADC_Historical_Execution
					WHEN(	NEG.PropertyFaultVariable = 'Appreciation') THEN FA.Appreciation
					WHEN(	NEG.PropertyFaultVariable = 'BPOSurplus') THEN (COALESCE(FA.BPO, @BPOReserveRatio*FA.Reserve) - FA.Reserve)
					WHEN(	NEG.PropertyFaultVariable = 'BPOSurplusPct') THEN ((COALESCE(FA.BPO, @BPOReserveRatio*FA.Reserve) - FA.Reserve) / FA.Reserve)
					WHEN(	NEG.PropertyFaultVariable = 'C2C_Flag') THEN FA.C2C_Flag
					WHEN(	NEG.PropertyFaultVariable = 'Cancellations') THEN FA.Cancellations
					WHEN(	NEG.PropertyFaultVariable = 'Condition_average') THEN FA.Condition_average
					WHEN(	NEG.PropertyFaultVariable = 'Condition_fair') THEN FA.Condition_fair 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_good') THEN FA.Condition_good 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_poor') THEN FA.Condition_poor 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_unknown') THEN FA.Condition_unknown 
					WHEN(	NEG.PropertyFaultVariable = 'Favorites') THEN FA.Favorites 
					WHEN(	NEG.PropertyFaultVariable = 'ForeclosedPer10000') THEN FA.ForeclosedPer10000 
					WHEN(	NEG.PropertyFaultVariable = 'HasPrevAttempts') THEN FA.HasPrevAttempts
					WHEN(	NEG.PropertyFaultVariable = 'MedianPPSQFT') THEN FA.MedianPPSQFT 
					WHEN(	NEG.PropertyFaultVariable = 'MedianPPSQFTDeviation') THEN FA.MedianPPSQFTDeviation 
					WHEN(	NEG.PropertyFaultVariable = 'MultiResidence_Flag') THEN FA.MultiResidence_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'PercentHomesSold') THEN FA.PercentHomesSold 
					WHEN(	NEG.PropertyFaultVariable = 'Postponements') THEN FA.Postponements 
					WHEN(	NEG.PropertyFaultVariable = 'PrevForeclosureSoldRate') THEN FA.PrevForeclosureSoldRate 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Occupied') THEN FA.PropertyOccupancyStatus_Occupied 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Unknown') THEN FA.PropertyOccupancyStatus_Unknown 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Vacant') THEN FA.PropertyOccupancyStatus_Vacant 
					WHEN(	NEG.PropertyFaultVariable = 'REO_Flag') THEN FA.REO_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'Reserve') THEN FA.Reserve 
					WHEN(	NEG.PropertyFaultVariable = 'RunNum') THEN FA.RunNum 
					WHEN(	NEG.PropertyFaultVariable = 'SellerPortfolioQuality') THEN FA.SellerPortfolioQuality 
					WHEN(	NEG.PropertyFaultVariable = 'SFR_Flag') THEN FA.SFR_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'ShortSale_Flag') THEN FA.ShortSale_Flag  
					WHEN(	NEG.PropertyFaultVariable = 'TotalActive') THEN FA.TotalActive
					WHEN(	NEG.PropertyFaultVariable = 'Trustee_Flag') THEN FA.Trustee_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'Turnover') THEN FA.Turnover 
					WHEN(	NEG.PropertyFaultVariable = 'Vacancy') THEN FA.Vacancy 
					WHEN(	NEG.PropertyFaultVariable = 'ValueLevel_High') THEN FA.ValueLevel_High 
					WHEN(	NEG.PropertyFaultVariable = 'ValueLevel_Low') THEN FA.ValueLevel_Low ELSE 
					FA.ValueLevel_Med END AS ComponentValue
	
	INTO #TrusteePropertyFaultPreOutput

	FROM
	#TrusteePredictionSourceTable FA
	JOIN
	PredictiveData.dbo.TrusteeNegativeAttributeLookup NEG
	ON
	FA.PropertyState = NEG.PropertyState

	ORDER BY
	GlobalPropertyId,
	ComponentValue DESC

SELECT
RowId
  ,GlobalPropertyId
  ,PropertyId
  ,VenueId
  ,AuctionId
  ,PropertyFaultVariable
  ,PropertyFaultName

INTO #TrusteeNegativeFactors
FROM(  
SELECT
ROW_NUMBER() OVER( PARTITION BY PFPO.GlobalPropertyId ORDER BY PFPO.ComponentValue ASC) AS RowId
,PFPO.*
FROM
#TrusteePropertyFaultPreOutput PFPO
WHERE ComponentValue IS NOT NULL
AND PropertyFaultVariable != 'Intercept'
AND PropertyFaultVariable != 'Reserve') T1
WHERE RowId = 1 or RowId = 2

        PredictionCalculationTable:
--========================================================================================================================
-- For Each Global Property ID, perform:
-- P(AuctionDaySale) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- P(SellerAcceptedSale) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- P(ExceedReserve) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- Likely High Bid = Suggested Reserve Maximum = Beta0 + BETA_Vector*X_Matrix -- Linear
--========================================================================================================================
SELECT
--Build Descriptive Information
A.GlobalPropertyId
,A.SystemAuctionId
,A.VenueId
,A.AuctionId
,A.PropertyId
,A.REDCID
,A.LoanNbr
,A.PropertyState
,A.PropertyZip
,A.PropertyCounty
,A.PropertyCity
,A.MSAName
,A.AuctionCode
,A.TrusteeCode
,A.TrusteeName
,A.Latitude
,A.Longitude
,A.AuctionDate
,A.PropertyOccupancyStatus
,A.Condition
,A.PropertyType
,A.ProductType
,A.RunNum
,A.TotalActive
,A.Vacancy
,A.Appreciation
,A.Turnover
,A.TotalHomesForSale
,A.PercentHomesSold
,A.MedianPPSQFT
,A.MedianPPSQFTDeviation
,A.ForeclosedPer10000
,A.PrevForeclosureSoldRate
,A.HomeSquareFootage
,A.Bedrooms
,A.Baths
,A.LotSize
,A.Age
,A.OpeningBid
,A.Reserve
,A.BPO
,CASE WHEN(A.BPO < 0.1 * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPO END AS FunctionalBPO
,A.HighBid
,A.IsCancelled
,A.IsPostponed
,A.IsRemoved
,A.IsAuction
,A.IsSold

--Build Likelihood to Be Cancelled
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
B.Intercept +
A.ADC_Historical_Execution * B.ADC_Historical_Execution +
A.Appreciation * B.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPOSurplus END) * B.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveSurplusDefault ELSE A.BPOSurplusPct END) * B.BPOSurplusPct +
A.C2C_Flag * B.C2C_Flag +
A.Cancellations * B.Cancellations +
A.Condition_poor * B.Condition_poor +
A.Condition_fair * B.Condition_fair +
A.Condition_average * B.Condition_average +
A.Condition_good * B.Condition_good +
A.Condition_unknown * B.Condition_unknown +
A.Favorites * B.Favorites +
A.ForeclosedPer10000 * B.ForeclosedPer10000 +
A.HasPrevAttempts * B.HasPrevAttempts +
A.MedianPPSQFT * B.MedianPPSQFT +
A.MedianPPSQFTDeviation * B.MedianPPSQFTDeviation +
A.MultiResidence_Flag * B.MultiResidence_Flag +
A.PercentHomesSold * B.PercentHomesSold +
A.Postponements * B.Postponements +
A.PrevForeclosureSoldRate * B.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * B.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * B.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * B.PropertyOccupancyStatus_Vacant +
A.REO_Flag * B.REO_Flag +
A.Reserve * B.Reserve +
A.RunNum * B.RunNum +
A.SellerPortfolioQuality * B.SellerPortfolioQuality +
A.SFR_Flag * B.SFR_Flag +
A.ShortSale_Flag * B.ShortSale_Flag + 
ISNULL(A.TotalActive, @ZIPPopulationDefault) * B.TotalActive +
CAST(ISNULL(A.Vacancy,@ZIPVacancyRateDefault) AS Decimal) * B.Vacancy +
A.ValueLevel_Low * B.ValueLevel_Low +
A.ValueLevel_Med * B.ValueLevel_Med +
A.ValueLevel_High * B.ValueLevel_High
)))
) AS CancellationPrediction

--Build Likelihood to be Postponed
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
C.Intercept +
A.ADC_Historical_Execution * C.ADC_Historical_Execution +
A.Appreciation * C.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPOSurplus END) * C.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveSurplusDefault ELSE A.BPOSurplusPct END) * C.BPOSurplusPct +
A.C2C_Flag * C.C2C_Flag +
A.Cancellations * C.Cancellations +
A.Condition_poor * C.Condition_poor +
A.Condition_fair * C.Condition_fair +
A.Condition_average * C.Condition_average +
A.Condition_good * C.Condition_good +
A.Condition_unknown * C.Condition_unknown +
A.Favorites * C.Favorites +
A.ForeclosedPer10000 * C.ForeclosedPer10000 +
A.HasPrevAttempts * C.HasPrevAttempts +
A.MedianPPSQFT * C.MedianPPSQFT +
A.MedianPPSQFTDeviation * C.MedianPPSQFTDeviation +
A.MultiResidence_Flag * C.MultiResidence_Flag +
A.PercentHomesSold * C.PercentHomesSold +
A.Postponements * C.Postponements +
A.PrevForeclosureSoldRate * C.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * C.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * C.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * C.PropertyOccupancyStatus_Vacant +
A.REO_Flag * C.REO_Flag +
A.Reserve * C.Reserve +
A.RunNum * C.RunNum +
A.SellerPortfolioQuality * C.SellerPortfolioQuality +
A.SFR_Flag * C.SFR_Flag +
A.ShortSale_Flag * C.ShortSale_Flag + 
ISNULL(A.TotalActive, @ZIPPopulationDefault) * C.TotalActive +
CAST(ISNULL(A.Vacancy,@ZIPVacancyRateDefault) AS Decimal) * C.Vacancy +
A.ValueLevel_Low * C.ValueLevel_Low +
A.ValueLevel_Med * C.ValueLevel_Med +
A.ValueLevel_High * C.ValueLevel_High
)))
) AS PostponementPrediction

--Build Likelihood to Third Party Sale If it Makes it to Auction Day
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
D.Intercept +
A.ADC_Historical_Execution * D.ADC_Historical_Execution +
A.Appreciation * D.Appreciation +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPOSurplus END) * D.BPOSurplus +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveSurplusDefault ELSE A.BPOSurplusPct END) * D.BPOSurplusPct +
A.C2C_Flag * D.C2C_Flag +
A.Cancellations * D.Cancellations +
A.Condition_poor * D.Condition_poor +
A.Condition_fair * D.Condition_fair +
A.Condition_average * D.Condition_average +
A.Condition_good * D.Condition_good +
A.Condition_unknown * D.Condition_unknown +
A.Favorites * D.Favorites +
A.ForeclosedPer10000 * D.ForeclosedPer10000 +
A.HasPrevAttempts * D.HasPrevAttempts +
A.MedianPPSQFT * D.MedianPPSQFT +
A.MedianPPSQFTDeviation * D.MedianPPSQFTDeviation +
A.MultiResidence_Flag * D.MultiResidence_Flag +
A.PercentHomesSold * D.PercentHomesSold +
A.Postponements * D.Postponements +
A.PrevForeclosureSoldRate * D.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * D.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * D.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * D.PropertyOccupancyStatus_Vacant +
A.REO_Flag * D.REO_Flag +
A.Reserve * D.Reserve +
A.RunNum * D.RunNum +
A.SellerPortfolioQuality * D.SellerPortfolioQuality +
A.SFR_Flag * D.SFR_Flag +
A.ShortSale_Flag * D.ShortSale_Flag + 
ISNULL(A.TotalActive, @ZIPPopulationDefault) * D.TotalActive +
CAST(ISNULL(A.Vacancy,@ZIPVacancyRateDefault) AS Decimal) * D.Vacancy +
A.ValueLevel_Low * D.ValueLevel_Low +
A.ValueLevel_Med * D.ValueLevel_Med +
A.ValueLevel_High * D.ValueLevel_High
)))
) AS ThirdPartySaleIfAuctionedPrediction

--Build Likelihood to Third Party Sale Overall
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
E.Intercept +
A.ADC_Historical_Execution * E.ADC_Historical_Execution +
A.Appreciation * E.Appreciation +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPOSurplus END) * E.BPOSurplus +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveSurplusDefault ELSE A.BPOSurplusPct END) * E.BPOSurplusPct +
A.C2C_Flag * E.C2C_Flag +
A.Cancellations * E.Cancellations +
A.Condition_poor * E.Condition_poor +
A.Condition_fair * E.Condition_fair +
A.Condition_average * E.Condition_average +
A.Condition_good * E.Condition_good +
A.Condition_unknown * E.Condition_unknown +
A.Favorites * E.Favorites +
A.ForeclosedPer10000 * E.ForeclosedPer10000 +
A.HasPrevAttempts * E.HasPrevAttempts +
A.MedianPPSQFT * E.MedianPPSQFT +
A.MedianPPSQFTDeviation * E.MedianPPSQFTDeviation +
A.MultiResidence_Flag * E.MultiResidence_Flag +
A.PercentHomesSold * E.PercentHomesSold +
A.Postponements * E.Postponements +
A.PrevForeclosureSoldRate * E.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * E.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * E.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * E.PropertyOccupancyStatus_Vacant +
A.REO_Flag * E.REO_Flag +
A.Reserve * E.Reserve +
A.RunNum * E.RunNum +
A.SellerPortfolioQuality * E.SellerPortfolioQuality +
A.SFR_Flag * E.SFR_Flag +
A.ShortSale_Flag * E.ShortSale_Flag + 
ISNULL(A.TotalActive, @ZIPPopulationDefault) * E.TotalActive +
CAST(ISNULL(A.Vacancy,@ZIPVacancyRateDefault) AS Decimal) * E.Vacancy +
A.ValueLevel_Low * E.ValueLevel_Low +
A.ValueLevel_Med * E.ValueLevel_Med +
A.ValueLevel_High * E.ValueLevel_High
)))
) AS ThirdPartySaleOverallPrediction

--Build Likely High Bid, rebranded as Suggested Reserve
,(
F.Intercept +
A.ADC_Historical_Execution * F.ADC_Historical_Execution +
A.Appreciation * F.Appreciation +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPO END) * F.BPO +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveRatio*A.Reserve ELSE A.BPOSurplus END) * F.BPOSurplus +
(CASE WHEN(A.BPO < @BPOAbsurdityLowerBound * A.Reserve OR A.BPO IS NULL) THEN @BPOReserveSurplusDefault ELSE A.BPOSurplusPct END) * F.BPOSurplusPct +
A.C2C_Flag * F.C2C_Flag +
A.Cancellations * F.Cancellations +
A.Condition_poor * F.Condition_poor +
A.Condition_fair * F.Condition_fair +
A.Condition_average * F.Condition_average +
A.Condition_good * F.Condition_good +
A.Condition_unknown * F.Condition_unknown +
A.Favorites * F.Favorites +
A.ForeclosedPer10000 * F.ForeclosedPer10000 +
A.HasPrevAttempts * F.HasPrevAttempts +
A.MedianPPSQFT * F.MedianPPSQFT +
A.MedianPPSQFTDeviation * F.MedianPPSQFTDeviation +
A.MultiResidence_Flag * F.MultiResidence_Flag +
A.PercentHomesSold * F.PercentHomesSold +
A.Postponements * F.Postponements +
A.PrevForeclosureSoldRate * F.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * F.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * F.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * F.PropertyOccupancyStatus_Vacant +
A.REO_Flag * F.REO_Flag +
A.RunNum * F.RunNum +
A.SellerPortfolioQuality * F.SellerPortfolioQuality +
A.SFR_Flag * F.SFR_Flag +
A.ShortSale_Flag * F.ShortSale_Flag + 
ISNULL(A.TotalActive, @ZIPPopulationDefault) * F.TotalActive +
CAST(ISNULL(A.Vacancy,@ZIPVacancyRateDefault) AS Decimal) * F.Vacancy +
A.Trustee_Flag * F.Trustee_Flag +
A.ValueLevel_Low * F.ValueLevel_Low +
A.ValueLevel_Med * F.ValueLevel_Med +
A.ValueLevel_High * F.ValueLevel_High
) AS RecommendedMaxReservePrice

--Include Rundate for Indexing
,CONVERT (date, SYSDATETIME()) AS ModelRunDate

INTO
#TrusteePredictionPreOutput

FROM
#TrusteePredictionSourceTable A
JOIN
PredictiveData.[dbo].[TrusteeCancellationCoefficients] B ON (A.PropertyState = B.PropertyState)
JOIN
PredictiveData.[dbo].[TrusteePostponementCoefficients] C ON (A.PropertyState = C.PropertyState)
JOIN
PredictiveData.[dbo].[TrusteeThirdPartySaleIfAuctionedCoefficients] D ON (A.PropertyState = D.PropertyState)
JOIN
PredictiveData.[dbo].[TrusteeThirdPartySaleOverallCoefficients] E ON (A.PropertyState = E.PropertyState)
JOIN
PredictiveData.[dbo].[TrusteeHighBidCoefficients] F ON (A.PropertyState = F.PropertyState)

ORDER BY
AuctionDate
,AuctionCode
,GlobalPropertyId;

CREATE NONCLUSTERED INDEX GlobalPropertyId ON #TrusteePredictionPreOutput (GlobalPropertyId);

-- Now Update the output table and we're done!

SELECT
--Build Descriptive Information
A.GlobalPropertyId
,A.SystemAuctionId
,A.VenueId
,A.AuctionId
,A.PropertyId
,A.REDCID
,A.LoanNbr
,A.PropertyState
,A.PropertyZip
,A.PropertyCounty
,A.PropertyCity
,A.MSAName
,A.AuctionCode
,A.TrusteeCode
,A.TrusteeName
,A.Latitude
,A.Longitude
,A.AuctionDate
,A.PropertyOccupancyStatus
,A.Condition
,A.PropertyType
,A.ProductType
,A.RunNum
,A.TotalActive
,A.Vacancy
,A.Appreciation
,A.Turnover
,A.TotalHomesForSale
,A.PercentHomesSold
,A.MedianPPSQFT
,A.MedianPPSQFTDeviation
,A.ForeclosedPer10000
,A.PrevForeclosureSoldRate
,A.HomeSquareFootage
,A.Bedrooms
,A.Baths
,A.LotSize
,A.Age
,A.OpeningBid
,A.Reserve
,A.BPO
,A.FunctionalBPO
,A.HighBid
,A.IsCancelled
,A.IsPostponed
,A.IsRemoved
,A.IsAuction
,A.IsSold
,CASE WHEN (A.RecommendedMaxReservePrice < A.OpeningBid) THEN A.OpeningBid ELSE A.RecommendedMaxReservePrice END AS RecommendedMaxReservePrice
,A.CancellationPrediction
,A.PostponementPrediction
,CASE WHEN (A.ThirdPartySaleIfAuctionedPrediction < A.ThirdPartySaleOverallPrediction) THEN A.ThirdPartySaleOverallPrediction ELSE A.ThirdPartySaleIfAuctionedPrediction END AS ThirdPartySaleIfAuctionedPrediction
,A.ThirdPartySaleOverallPrediction
,A.ModelRunDate
,1 As MostRecentData
,B.PropertyFaultName AS Negative1
,C.PropertyFaultName AS Negative2

INTO #TrusteePredictionOutput_Auto

FROM
#TrusteePredictionPreOutput A
JOIN
#TrusteeNegativeFactors B ON ( B.RowId = 1 AND A.GlobalPropertyId = B.GlobalPropertyId) 
JOIN
#TrusteeNegativeFactors C ON ( C.RowId = 2 AND A.GlobalPropertyId = C.GlobalPropertyId)

ORDER BY
AuctionDate
,AuctionCode
,GlobalPropertyId;


        PublishingResults:
--========================================================================================================================
-- Publishing Results into their Final Tables
-- Put the Prediction Table into it's two destination tables, one with all historical info, another with only the most recent data
--========================================================================================================================

--Update existing historical record to show that previous data is now not the most recent:
UPDATE PredictiveData.dbo.TrusteePredictionOutput_Auto
SET MostRecentData = 0

--Append to historical record
INSERT INTO PredictiveData.dbo.TrusteePredictionOutput_Auto
SELECT * FROM #TrusteePredictionOutput_Auto;

--Update the "Most Recent Prediction" Table
DROP TABLE PredictiveData.dbo.MostRecentTrusteePredictionOutput_Auto;
SELECT * INTO PredictiveData.dbo.MostRecentTrusteePredictionOutput_Auto
FROM #TrusteePredictionOutput_Auto;

        DropTables:
        IF OBJECT_ID('tempdb..#TemporaryFactTrusteeAuction') IS NOT NULL
            BEGIN
                DROP TABLE #TemporaryFactTrusteeAuction
            END
        IF OBJECT_ID('tempdb..#BroughtToSaleHistory') IS NOT NULL
            BEGIN
                DROP TABLE #BroughtToSaleHistory;
            END
        IF OBJECT_ID('tempdb..#Favorites') IS NOT NULL
            BEGIN
                DROP TABLE #Favorites;
            END
        IF OBJECT_ID('tempdb..#ADCExecution') IS NOT NULL
            BEGIN
                DROP TABLE #ADCExecution;
            END
        IF OBJECT_ID('tempdb..#TrusteePortfolioQuality') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePortfolioQuality;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionPreOutput') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionPreOutput;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionSourceTable') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionSourceTable;
            END
        IF OBJECT_ID('tempdb..#TrusteePredictionOutput_Auto') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePredictionOutput_Auto;
            END
        IF OBJECT_ID('tempdb..#TrusteeNegativeFactors') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteeNegativeFactors;
            END
        IF OBJECT_ID('tempdb..#TrusteePropertyFaultPreOutput') IS NOT NULL
            BEGIN
                DROP TABLE #TrusteePropertyFaultPreOutput;
            END

END

/*
DROP PROCEDURE DataScience_TrusteePredictionData

DROP TABLE PredictiveData.dbo.TrusteePredictionOutput_Auto

SELECT * FROM PredictiveData.dbo.TrusteePredictionOutput_Auto WHERE MostRecentData = 1
*/

GO
