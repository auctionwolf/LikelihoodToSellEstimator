--Start with FactTrusteeAuction as a base, but trim down the result set and add in an index to join on
SELECT
A.*
, CAST(CONCAT(Year(A.AuctionDate), '-', Month(A.AuctionDate), '-', '01') AS DATE) AS ZillowJoinField
, Year(A.AuctionDate)*100 + Month(A.AuctionDate) AS USPSJoinField
INTO
#TemporaryFactTrusteeAuction
FROM
EDW.DBO.FactTrusteeAuction A
WHERE	A.AuctionDate >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
AND		A.AuctionDate IS NOT NULL
Order By AuctionDate 

CREATE NONCLUSTERED INDEX PropertyId ON #TemporaryFactTrusteeAuction (PropertyId); 
CREATE NONCLUSTERED INDEX VenueId ON #TemporaryFactTrusteeAuction (VenueId); 
CREATE NONCLUSTERED INDEX AuctionId ON #TemporaryFactTrusteeAuction (AuctionId); 
CREATE CLUSTERED INDEX ZillowJoinField ON #TemporaryFactTrusteeAuction (ZillowJoinField);
CREATE NONCLUSTERED INDEX USPSJoinField ON #TemporaryFactTrusteeAuction (USPSJoinField);


--Historical Sell Through Rate Data (Auction.com Execution)
SELECT
DP.PropertyState
,DP.PropertyZip
,AVG(CAST(FTA.IsSold AS DECIMAL)) AS AvgSTR
,count(FTA.IsAuction) AS TotalAuctioned

INTO
#ADCExecution

FROM		EDW.dbo.FactTrusteeAuction				FTA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FTA.PropertyId)
LEFT JOIN	EDW.dbo.DimAuction						DA	ON( DA.AuctionId = FTA.AuctionId)

WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FTA.AuctionDate >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
--AND		FTA.AuctionDate <= CURRENT_TIMESTAMP
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




--Historical Seller (Trustee) Portfolio Quality Data
select
DT.TrusteeCode
,AVG(CAST(FTA.IsSold AS DECIMAL)) AS AvgSTR
,COUNT(FTA.IsAuction) AS TotalAuctioned

INTO
#TrusteePortfolioQuality

FROM		EDW.dbo.FactTrusteeAuction				FTA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FTA.PropertyId)
LEFT JOIN	EDW.dbo.DimAuction						DA	ON( DA.AuctionId = FTA.AuctionId)
LEFT JOIN	EDW.dbo.DimTrustee						DT	ON(DT.TrusteeId = FTA.TrusteeId)


WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FTA.AuctionDate >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
--AND		FTA.AuctionDate <= CURRENT_TIMESTAMP
AND		FTA.AuctionDate IS NOT NULL
AND		FTA.IsAuction = 1

GROUP BY
DT.TrusteeCode

HAVING
COUNT(FTA.IsAuction) >= 5

ORDER BY
DT.TrusteeCode


--Property Historical Brought To Sale Outcomes (Indicates likelihood of Add'l postponement or cancelation
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



--Get Favorites
SELECT
PropertyId
,COUNT(distinct(UserId)) AS Favorites

INTO
#Favorites

FROM
EDW.dbo.FactPropertyFavorites

GROUP BY
propertyId

ORDER BY
propertyId




--And now we unite all of this and the major auction information tables into a complete predictive set



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
,ISNULL(TempF.Favorites,0) / (CASE WHEN(HIST.RunNum = 0 OR HIST.RunNum IS NULL) THEN 1 ELSE HIST.RunNum END) AS Favorites

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
--,CASE WHEN(ZSG.DeedType = 'Full Warranty') THEN 1 ELSE 0 END AS FullWarrantyDeed
--,CASE WHEN(ZSG.DeedType = 'Special Warranty') THEN 1 ELSE 0 END AS SpecialWarrantyDeed
--,CASE WHEN(ZSG.DeedType = 'Quit Claim') THEN 1 ELSE 0 END AS QuitClaimDeed
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
LEFT JOIN	#Favorites										TempF ON(FTA.PropertyId = TempF.PropertyId)
LEFT JOIN	PredictiveData.dbo.InterpolatedZillowMetrics	F	ON(DP.PropertyZip = F.PropertyZip AND Date = FTA.ZillowJoinField)
LEFT JOIN	PredictiveData.dbo.USPS_Vacancy					USPS  ON(DP.PropertyZip = USPS.ZIP AND USPS.Date = FTA.USPSJoinField)
LEFT JOIN	#ADCExecution									AE ON(DP.PropertyZip = AE.PropertyZip)
LEFT JOIN	#TrusteePortfolioQuality						PQ ON(DT.TrusteeCode = PQ.TrusteeCode)
LEFT JOIN	#BroughtToSaleHistory							HIST ON(HIST.PropertyId = FTA.PropertyId)

WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
--AND		FTA.AuctionDate >= CURRENT_TIMESTAMP
AND		FTA.AuctionDate >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
--AND		DA.AuctionEndDate >= CURRENT_TIMESTAMP


CREATE NONCLUSTERED INDEX PropertyId ON #TrusteePredictionSourceTable (PropertyId);
CREATE NONCLUSTERED INDEX VenueId ON #TrusteePredictionSourceTable (VenueId);
CREATE NONCLUSTERED INDEX PropertyState ON #TrusteePredictionSourceTable (PropertyState);

SELECT * FROM #TrusteePredictionSourceTable	