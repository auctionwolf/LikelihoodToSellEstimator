USE [PredictiveData]
GO

/****** Object:  StoredProcedure [dbo].[DataScience_NonTrusteePredictionData]    Script Date: 1/27/2014 10:34:51 AM ******/
SET ANSI_NULLS ON
GO

SET QUOTED_IDENTIFIER ON
GO





-- =============================================
-- Author:		Wolf Rendall, wrendall@auction.com
-- Create date: 2013-12-23
-- Last Edit:	2014-01-15
-- Description:	Final Table for Asset Prediction
-- =============================================
CREATE PROCEDURE [dbo].[DataScience_NonTrusteePredictionData] 

AS
BEGIN
-- SET NOCOUNT ON added to prevent extra result sets from
-- interfering with SELECT statements.
	SET NOCOUNT ON;

--Query to pull housing data for predictive model for all states

--Date range is 3 months prior
--Start by creating temporary tables and finish with one pull at the end


--Start with FactAuction as a base, but trim down the result set and add in an index to join on
SELECT
A.*
, CAST(CONCAT(Year(A.BidEndDT), '-', Month(A.BidEndDT), '-', '01') AS DATE) AS ZillowJoinField
, Year(A.BidEndDT)*100 + Month(A.BidEndDT) AS USPSJoinField
INTO
#TemporaryFactAuction
FROM
EDW.DBO.FactAuction A
WHERE	A.BidEndDT >= DATEADD(MONTH, (-3), CURRENT_TIMESTAMP)
AND		A.BidEndDT IS NOT NULL

Order By BidEndDT 

CREATE NONCLUSTERED INDEX PropertyId ON #TemporaryFactAuction (PropertyId); 
CREATE NONCLUSTERED INDEX VenueId ON #TemporaryFactAuction (VenueId); 
CREATE NONCLUSTERED INDEX AuctionId ON #TemporaryFactAuction (AuctionId); 
CREATE CLUSTERED INDEX ZillowJoinField ON #TemporaryFactAuction (ZillowJoinField);
CREATE NONCLUSTERED INDEX USPSJoinField ON #TemporaryFactAuction (USPSJoinField);


--Historical Sell Through Rate Data (Auction.com Execution)
SELECT
DP.PropertyState
,DP.PropertyZip
,AVG(CAST(FA.IsSold AS DECIMAL)) AS AvgSTR
,count(FA.IsAuctioned) AS TotalAuctioned

INTO
#ADCExecution

FROM		EDW.dbo.FactAuction						FA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FA.PropertyId)
LEFT JOIN	EDW.dbo.DimAuction						DA	ON( DA.AuctionId = FA.AuctionId)
LEFT JOIN	EDW.dbo.DimAuctionStatus				DAS	ON(DAS.AuctionStatusId = FA.AuctionStatusId)

WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FA.BidEndDT >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
AND		FA.BidEndDT <= CURRENT_TIMESTAMP
AND		FA.BidEndDT IS NOT NULL
AND		(DP.ProductType LIKE '%REO%' OR DP.ProductType LIKE '%C2C%' OR DP.ProductType LIKE 'Short Sale')
AND		DAS.AuctionStatus NOT LIKE '%Removed%'
AND		DAS.AuctionStatus NOT LIKE '%Cancelled%'

GROUP BY
DP.PropertyState
,DP.PropertyZip

HAVING
COUNT(FA.IsAuctioned) >= 10

ORDER BY
DP.PropertyState
,DP.PropertyZip




--Historical Seller Portfolio Quality Data
select
DS.SellerCode
,AVG(CAST(FA.IsSold AS DECIMAL)) AS AvgSTR
,COUNT(FA.IsAuctioned) AS TotalAuctioned

INTO
#SellerPortfolioQuality

FROM		EDW.dbo.FactAuction						FA
LEFT JOIN	EDW.dbo.DimProperty						DP	ON( DP.PropertyId = FA.PropertyId)
LEFT JOIN	EDW.dbo.DimAuction						DA	ON( DA.AuctionId = FA.AuctionId)
LEFT JOIN	EDW.dbo.DimAuctionStatus				DAS	ON(DAS.AuctionStatusId = FA.AuctionStatusId)
LEFT JOIN	EDW.dbo.DimSeller						DS	ON(DS.SellerId = FA.SellerId)


WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FA.BidEndDT >= DATEADD(MONTH, (-5), CURRENT_TIMESTAMP)
AND		FA.BidEndDT <= CURRENT_TIMESTAMP
AND		FA.BidEndDT IS NOT NULL
AND		(DP.ProductType LIKE '%REO%' OR DP.ProductType LIKE '%C2C%' OR DP.ProductType LIKE 'Short Sale')
AND		DAS.AuctionStatus NOT LIKE '%Removed%'
AND		DAS.AuctionStatus NOT LIKE '%Cancelled%'

GROUP BY
DS.SellerCode

HAVING
COUNT(FA.IsAuctioned) >= 10

ORDER BY
DS.SellerCode




-- HIGH BID DATA
SELECT
PropertyId,
VenueId,
AuctionId,
MAX(BidAmount) AS HighBid,
MAX(WinningBid) AS WinningBid,
COUNT(DISTINCT(BidderId)) AS Bidders,
COUNT(DISTINCT(BidAmount)) AS Bids

INTO
#HighBidHistory

FROM(
	SELECT 
	A.VenueId,
	A.AuctionId,
	A.PropertyId,
	A.BidAmount,
	A.AuctionDate,
	A.BidderId,
	A.BidDateTime,
	A.BidStartDate,
	DP.PropertyType,
	DP.ProductType,
	A.WinningBid,
	A.StartingBid
	FROM 
	EDW.dbo.FactBidLive A
	LEFT JOIN
	EDW.dbo.DimProperty DP ON (A.PropertyId = DP.PropertyId)
	WHERE
	A.AuctionDate >= DATEADD(MONTH, (-3), CURRENT_TIMESTAMP)
	AND
	A.BidderId != -1
) BidDetails

GROUP BY
PropertyId,
VenueId,
AuctionId;

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



-- ALR Information
-- Get the property-level history for each bidder in the timeframe specified in the WHERE clause
select
db.bidderemail, 
fbl.propertyid,
fbl.auctionid,
fbl.venueid,
dp.PropertyCounty,
max(fbl.bidamount) as MaxBid,
max(fbl.winningbid) as WinningBid,
max(fbl.iswinningbid) as Winner,
max(fbl.startingbid) as StartingBid,
max(fbl.AuctionSellerReserve) as Reserve,
max(bedrooms) as beds,
max(baths) as baths,
max(lotsize) as lotsize,
max(yearbuilt) as yearbuilt,
max(fr.BPO) as BPO,
max(parsedZip) as ZIP

into
#bidder_history_raw

from
edw.dbo.factbidlive fbl
join
edw.dbo.factresidential fr on(fbl.propertyid = fr.propertyid and fbl.auctionid = fr.auctionid and fbl.venueid = fr.venueid)
join
edw.dbo.dimproperty dp on(dp.propertyid = fbl.propertyid)
join
edw.dbo.dimbidder db on (db.bidderid = fbl.bidderid)

where
fbl.auctiondate >= '2013-07-01'
and fbl.bidderid > 0
and fbl.propertyid > 0
and fbl.auctionid > 0

group by
db.bidderemail,
fbl.propertyid,
fbl.auctionid,
fbl.venueid,
dp.PropertyCounty

order by 
fbl.propertyid,
db.bidderemail;

CREATE CLUSTERED INDEX propertyid ON #bidder_history_raw (propertyid);
CREATE NONCLUSTERED INDEX bidderemail ON #bidder_history_raw (bidderemail);

-- Condense the property level history for each email address (assume this is unique bidders) to establish budget preferences
select
bidderemail,
count(distinct(propertyid)) AS AssetsBidUpon,
avg(MaxBid) AS AvgMaxBid,
avg(cast(winner AS decimal(9,2))) As AvgWinRate,
avg(case when MaxBid > WinningBid then 1.0 else 0.0 end) AS FallOutRate,
avg(MaxBid/(case when Reserve < 1 then COALESCE(MaxBid, StartingBid) else Reserve end)) as AvgMaxBidAsShareOfReserve,
avg(StartingBid) as AvgStartingBid,
avg(Reserve) as AvgReserve,
avg(case when beds >=0 then beds else null end) as avgBeds,
avg(case when baths >=0 then baths else null end) as avgBaths,
avg(case when lotsize >=0 then lotsize else null end) as avgLotSize,
avg(case when yearbuilt > 1500 then yearbuilt else null end) as AvgYearBuilt,
avg(BPO) as AvgBPO
into
#bidder_budget_criteria
from
#bidder_history_raw
group by
bidderemail
order by 
bidderemail;

CREATE CLUSTERED INDEX bidderemail ON #bidder_budget_criteria (bidderemail);

-- For each email, join in ALR information about properties registered for and properties they actually bid on to create Attendance
select 
a.bidderemail,
b.totalattended,
a.totalregistrations,
cast(b.totalattended as decimal(9,2))/ a.totalregistrations as attendance
into
#bidder_attendance_scoring
from(
select
bidderemail,
count(distinct(fpr.propertyid)) as totalregistrations
from
edw.dbo.dimbidder db
join
edw.dbo.factpropertyregistration fpr on(db.bidderid = fpr.bidderid)
where fpr.CreateDate >= '2013-06-01'
group by
bidderemail
) a,
(
select
bidderemail,
count(distinct(fbl.propertyid)) as totalattended
from
edw.dbo.dimbidder db
join
edw.dbo.factbidlive fbl on(db.bidderid = fbl.bidderid)
where fbl.AuctionDate >= '2013-07-01'
and fbl.bidderid > 0
and fbl.propertyid > 0
and fbl.auctionid > 0
group by
db.bidderemail
) b
where a.bidderemail = b.bidderemail;

CREATE CLUSTERED INDEX bidderemail ON #bidder_attendance_scoring (bidderemail);

--Aggregate the bidders' scores by propertyid, venueid, and auctionid for merging with other data. The subquery generates the pre-aggregation scores
select
propertyid,
venueid,
auctionid,
count(distinct(bidderemail)) as ALRRegistrants,
sum(naturalscore) as IntrinsicBidderQuality,
sum(bidderscore) as TotalRegistrantScore

into
#ALRScore

from(

--This subtables finds the scores per email address and shows how they're calculated
select 
fpr.auctionid
,fpr.propertyid
,fpr.venueid
,db.bidderemail
,dp.Bedrooms
,dp.Baths
,dp.LotSize
,dp.YearBuilt
,coalesce(FA.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve, AvgReserve, 1) AS Reserve
,AvgReserve / COALESCE(	CASE WHEN(FA.AuctionSellerReserve > 0) THEN FA.AuctionSellerReserve ELSE NULL END, 
							CASE WHEN(FA.Reserve > 0) THEN FA.Reserve ELSE NULL END,
							CASE WHEN(FA.LenderReserve > 0) THEN FA.LenderReserve ELSE NULL END,
							CASE WHEN(FA.UpdatedReserve > 0) THEN FA.UpdatedReserve ELSE NULL END,
							CASE WHEN AvgReserve > 0 THEN AvgReserve ELSE NULL END,
							1) as ReserveMatch
,(attendance * AvgMaxBidAsShareOfReserve) as NaturalScore
,(case when dp.Bedrooms <= 0 then AvgBeds else dp.Bedrooms end) / (case when AvgBeds <= 0 then (case when dp.Bedrooms <= 0 then 1 else dp.Bedrooms end) else avgBeds end) as Bedmatch
,(case when dp.Baths <= 0 then AvgBaths else dp.Baths end) / (case when AvgBaths < 1 then (case when dp.Baths <= 0 then 1 else dp.Baths end) else AvgBaths end) as Bathmatch
,(case when dp.LotSize <= 0 then AvgLotSize else dp.LotSize end) / (case when AvgLotSize <= 0 then (case when dp.LotSize <= 0 then 1 else dp.LotSize end) else AvgLotSize end) as Lotmatch
,(attendance * AvgMaxBidAsShareOfReserve) * (
abs(coalesce(AvgReserve,1) / (COALESCE(	CASE WHEN(FA.AuctionSellerReserve > 0) THEN FA.AuctionSellerReserve ELSE NULL END, 
							CASE WHEN(FA.Reserve > 0) THEN FA.Reserve ELSE NULL END,
							CASE WHEN(FA.LenderReserve > 0) THEN FA.LenderReserve ELSE NULL END,
							CASE WHEN(FA.UpdatedReserve > 0) THEN FA.UpdatedReserve ELSE NULL END,
							CASE WHEN AvgReserve > 0 THEN AvgReserve ELSE NULL END,
							1) +1)
) -- Positive, want to have reserves lower than the average reserve bid on.
) as BidderScore

from 
edw.dbo.factpropertyregistration  fpr
join
edw.dbo.dimbidder db on(fpr.bidderid = db.bidderid)
join
edw.dbo.dimproperty dp on(fpr.propertyid = dp.propertyid)
join
#bidder_budget_criteria a on(db.bidderemail = a.bidderemail)
join
#bidder_attendance_scoring b on(db.bidderemail = b.bidderemail)
left join
edw.dbo.factauction fa on (fpr.auctionid = fa.auctionid and fpr.venueid = fa.venueid and fpr.propertyid = fa.propertyid)

WHERE
fpr.VenueId IS NOT NULL
AND fpr.ApprovalBucket in ('Fully Approved', 'Fully Approved, IDO', 'Needs Review - Approved w/o Validated Funds')

) Score

group by
propertyid,
venueid,
auctionid

order by
propertyid,
venueid,
auctionid

CREATE CLUSTERED INDEX propertyid ON #ALRScore (propertyid);
CREATE NONCLUSTERED INDEX venueid ON #ALRScore (venueid);



--And now we unite all of this and the major auction information tables into a complete predictive set



SELECT
--Descriptive Information
DP.GlobalPropertyId
,DA.SystemAuctionId
,FA.VenueId
,FA.AuctionId
,FA.PropertyId
,FA.REDCID
,DP.LoanNbr
,DP.PropertyState
,DP.PropertyZip
,DP.PropertyCounty
,DP.PropertyCity
,DG.MSAName
,DA.AuctionCode
,DS.SellerCode
,DS.SellerName
,DG.Latitude
,DG.Longitude
,FA.BidEndDT
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
,FA.IsSold
,CASE WHEN(TempB.HighBid >= COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve)) THEN 1 ELSE 0 END AS AuctionReserveSuccess

--Historical Information
,TempB.HighBid
,FA.RunNum
,SIGN(FA.RunNum -1) AS HasPrevAttempts

--Reserve Logic
,COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve) AS Reserve

--Valuation ('BPO') Logic
,(COALESCE(		CASE WHEN(DP.ProductType LIKE '%C2C%') THEN COALESCE(FA.AprAsIs, FR.AprAsIs) ELSE NULL END,
				CASE WHEN SellerParent = 'Bank of America' THEN COALESCE(FA.TotalOwed, FA.PropertyFeature6, FA.SellerBPOValue, DP.SellerBPOValue) ELSE NULL END,
				FR.BPO, FA.Agent90AsIs, FA.AgentValue, FA.PropertyFeature6, FA.GrcValue, 
				CASE WHEN (FA.AprAsIs > 4*COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve) AND FA.AprAsIs > 10000000) THEN NULL ELSE FA.AprAsIs END,
				CASE WHEN(DP.SellerBPOValue IS NULL OR DP.SellerBPOValue < 10) THEN NULL ELSE DP.SellerBPOValue END) )  AS BPO

--IF Valuations get weird, investigate individually here
--, FR.BPO AS BPO_FR
--, FA.Agent90AsIs AS BPO_FA_Agent90
--, FR.Agent90AsIs AS BPO_FR_Agent90
--, FA.AprAsIs AS BPO_FA_AprAsIs
--, FR.AprAsIs AS BPO_FR_AprAsIs
--, FA.AgentValue AS BPO_FA_AgentValue
--, FA.GrcValue AS BPO_FA_GrcValue
--, FA.RMVComments as FA_RMV
--, FR.RMVComments as FR_RMV

--Valuation and Reserve Logic Intertwined
--Numerator BPO Minus Reserve Price = BPO Surplus
,(COALESCE(		CASE WHEN(DP.ProductType LIKE '%C2C%') THEN COALESCE(FA.AprAsIs, FR.AprAsIs) ELSE NULL END,
				CASE WHEN SellerParent = 'Bank of America' THEN COALESCE(FA.TotalOwed, FA.PropertyFeature6, FA.SellerBPOValue, DP.SellerBPOValue) ELSE NULL END,
				FR.BPO, FA.Agent90AsIs, FA.AgentValue, FA.PropertyFeature6, FA.GrcValue, 
				CASE WHEN (FA.AprAsIs > 4*COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve) AND FA.AprAsIs > 10000000) THEN NULL ELSE FA.AprAsIs END,
				CASE WHEN(DP.SellerBPOValue IS NULL OR DP.SellerBPOValue < 10) THEN NULL ELSE DP.SellerBPOValue END) ) 
				- COALESCE(	CASE WHEN(FA.AuctionSellerReserve > 0) THEN FA.AuctionSellerReserve ELSE NULL END, 
							CASE WHEN(FR.AuctionSellerReserve > 0) THEN FR.AuctionSellerReserve ELSE NULL END,
							CASE WHEN(FA.Reserve > 0) THEN FA.Reserve ELSE NULL END,
							CASE WHEN(FA.LenderReserve > 0) THEN FA.LenderReserve ELSE NULL END,
							CASE WHEN(FA.UpdatedReserve > 0) THEN FA.UpdatedReserve ELSE NULL END) AS BPOSurplus

--BPO Surplus Over Reserve. A ratio
,(COALESCE(	CASE WHEN(DP.ProductType LIKE '%C2C%') THEN COALESCE(FA.AprAsIs, FR.AprAsIs) ELSE NULL END,
				CASE WHEN SellerParent = 'Bank of America' THEN COALESCE(FA.TotalOwed, FA.PropertyFeature6, FA.SellerBPOValue, DP.SellerBPOValue) ELSE NULL END,
				FR.BPO, FA.Agent90AsIs, FA.AgentValue, FA.PropertyFeature6, FA.GrcValue, 
				CASE WHEN (FA.AprAsIs > 4*COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve) AND FA.AprAsIs > 10000000) THEN NULL ELSE FA.AprAsIs END,
				CASE WHEN(DP.SellerBPOValue IS NULL OR DP.SellerBPOValue < 10) THEN NULL ELSE DP.SellerBPOValue END) 
				/ COALESCE(	CASE WHEN(FA.AuctionSellerReserve > 0) THEN FA.AuctionSellerReserve ELSE NULL END, 
							CASE WHEN(FR.AuctionSellerReserve > 0) THEN FR.AuctionSellerReserve ELSE NULL END,
							CASE WHEN(FA.Reserve > 0) THEN FA.Reserve ELSE NULL END,
							CASE WHEN(FA.LenderReserve > 0) THEN FA.LenderReserve ELSE NULL END,
							CASE WHEN(FA.UpdatedReserve > 0) THEN FA.UpdatedReserve ELSE NULL END)
								) -1 AS BPOSurplusPct

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

--Condense multiple options in financing table into 1 or 0 in a single flag
,CASE WHEN(PAS.FinancingAvailable LIKE '203k' OR PAS.FinancingAvailable LIKE 'Yes') THEN 1 ELSE 0 END AS FinancingAvailable

--USPS Data
,USPS.Active AS TotalActive
,USPS.Vacancy AS Vacancy

--Price Level Within State should be represented as dummies, Ask Paul if there is a way to avoid calling this function thrice (or more if more value levels are req'd            
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY (COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve)) ASC) =1) THEN 1 ELSE 0 END AS ValueLevel_Low
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY (COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve)) ASC) =2) THEN 1 ELSE 0 END AS ValueLevel_Med
,CASE WHEN(NTILE(3) OVER(PARTITION BY DP.PropertyState ORDER BY (COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve)) ASC) =3) THEN 1 ELSE 0 END AS ValueLevel_High

--ALR Score
,ISNULL(TempR.ALRRegistrants,0) AS ALRRegistrants
,ISNULL(TempR.TotalRegistrantScore,0) AS TotalRegistrantScore
,ISNULL(TempR.IntrinsicBidderQuality,0) AS IntrinsicBidderQuality

--Web Traffic Metrics
,ISNULL(TempF.Favorites,0) / CASE WHEN(FA.RunNum < 1 OR FA.RunNum is NULL) THEN 1 ELSE FA.RunNum END AS Favorites

--Zillow-Based Metrics
,COALESCE(Appreciation, CountyAppreciation, StateAppreciation, NationalAppreciation ) AS Appreciation
,COALESCE(Turnover, CountyTurnover, StateTurnover, NationalTurnover) AS Turnover
,COALESCE(TotalHomesForSale, CountyTotalHomesForSale, StateTotalHomesForSale, NationalTotalHomesForSale) AS TotalHomesForSale
,COALESCE(PercentHomesSold_12moTrailing, CountyPercentHomesSold_12moTrailing, StatePercentHomesSold_12moTrailing, NationalPercentHomesSold_12moTrailing) AS PercentHomesSold
,COALESCE(MedianPPSQFT, CountyMedianPPSQFT, StateMedianPPSQFT, NationalMedianPPSQFT) AS MedianPPSQFT
,(COALESCE(MedianPPSQFT, CountyMedianPPSQFT, StateMedianPPSQFT, NationalMedianPPSQFT) - (COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve) / 
					(CASE WHEN(DP.HomeSquareFootage > 10) THEN DP.HomeSquareFootage ELSE NULL END))) AS MedianPPSQFTDeviation
,COALESCE(ForeclosedHomesPer10000, CountyForeclosedHomesPer10000, StateForeclosedHomesPer10000, NationalForeclosedHomesPer10000) AS ForeclosedPer10000
,COALESCE(PercentSoldHomesPrevForeclosures_12moTrailing, CountyPrevForeclosureSold, StatePrevForeclosureSold, NationalPrevForeclosureSold) AS PrevForeclosureSoldRate

--More House Data Just Because
,DP.HomeSquareFootage
,DP.Bedrooms
,DP.Baths
,(2013 - DP.YearBuilt) AS Age
,DP.LotSize
,FA.StartingBid
,FR.IsSubjectToApproval
,CASE WHEN((CASE WHEN(TempB.HighBid >= COALESCE(FA.AuctionSellerReserve, FR.AuctionSellerReserve, FA.Reserve, FA.LenderReserve, FA.UpdatedReserve)) THEN 1 ELSE 0 END = 1) or (FA.IsSold = 1 and FR.IsSubjectToApproval = 1 and FA.SubjToAccepted = 1)) THEN 1 ELSE 0 END AS TrueSold
,COUNT(DA.AuctionCode) OVER(PARTITION BY DA.AuctionCode) AS EventSize
,COUNT(DA.AuctionCode) OVER(PARTITION BY DA.AuctionCode, DP.PropertyState) AS StateEventSize
,CASE WHEN(ZSG.DeedType = 'Full Warranty') THEN 1 ELSE 0 END AS FullWarrantyDeed
,CASE WHEN(ZSG.DeedType = 'Special Warranty') THEN 1 ELSE 0 END AS SpecialWarrantyDeed
,CASE WHEN(ZSG.DeedType = 'Quit Claim') THEN 1 ELSE 0 END AS QuitClaimDeed
,ISNULL(AE.AvgSTR, 0) AS ADC_Historical_Execution
,ISNULL(PQ.AvgSTR,0) AS SellerPortfolioQuality

INTO
#PredictionSourceTable	

FROM		#TemporaryFactAuction							FA
LEFT JOIN	EDW.dbo.DimProperty								DP	ON( DP.PropertyId = FA.PropertyId)
LEFT JOIN	EDW.dbo.DimAuction								DA	ON( DA.AuctionId = FA.AuctionId)
LEFT JOIN	EDW.dbo.FactResidential							FR	ON( FR.PropertyId = FA.PropertyId AND FR.AuctionId = FA.AuctionId AND FR.VenueId = FA.VenueId)
LEFT JOIN	EDW.dbo.DimAuctionStatus						DAS	ON(DAS.AuctionStatusId = FA.AuctionStatusId)
LEFT JOIN	EDW.dbo.DimPreAuctionStatus						PAS	ON(PAS.PreAuctionStatusId = FR.PreAuctionStatusId)
LEFT JOIN	EDW.dbo.DimGeo_NEW								DG	ON(DG.GeoId = FR.NewGeoId)
LEFT JOIN	EDW.dbo.DimSeller								DS	ON(DS.SellerId = FR.SellerId)
LEFT JOIN	#Favorites										TempF ON(FA.PropertyId = TempF.PropertyId)
LEFT JOIN	#ALRScore										TempR ON(FA.PropertyId = TempR.PropertyId AND FA.AuctionId = TempR.AuctionId AND FA.VenueId = TempR.VenueId)
LEFT JOIN	#HighBidHistory									TempB ON(FA.PropertyId = TempB.PropertyId AND FA.AuctionId = TempB.AuctionId AND FA.VenueID = TempB.VenueId)
LEFT JOIN	PredictiveData.dbo.InterpolatedZillowMetrics	F	ON (DP.PropertyZip = F.PropertyZip AND Date = FA.ZillowJoinField)
LEFT JOIN	PredictiveData.dbo.USPS_Vacancy					USPS  ON (DP.PropertyZip = USPS.ZIP AND USPS.Date = FA.USPSJoinField)
LEFT JOIN	EDW.dbo.DimSellerGroup							ZSG ON (DS.SellerCode = ZSG.SellerCode)	
LEFT JOIN	#ADCExecution									AE ON (DP.PropertyZip = AE.PropertyZip)
LEFT JOIN	#SellerPortfolioQuality							PQ ON (DS.SellerCode = PQ.SellerCode)

WHERE	DA.IsTest = 0
AND		DA.IsDemo = 0
AND		FA.BidEndDT >= CURRENT_TIMESTAMP
--AND		FA.BidEndDT >= DATEADD(MONTH, (-3), CURRENT_TIMESTAMP)
--AND		DA.AuctionEndDate >= CURRENT_TIMESTAMP
AND		DAS.AuctionStatus NOT LIKE '%Removed%'
AND		DAS.AuctionStatus NOT LIKE '%Cancelled%';


CREATE NONCLUSTERED INDEX PropertyId ON #PredictionSourceTable (PropertyId);
CREATE NONCLUSTERED INDEX VenueId ON #PredictionSourceTable (VenueId);
CREATE NONCLUSTERED INDEX PropertyState ON #PredictionSourceTable (PropertyState);




-- For Each Global Property ID, perform:
-- P(AuctionDaySale) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- P(SellerAcceptedSale) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- P(ExceedReserve) = 1 / (1 + EXP((-1)*(Beta0 + BETA_Vector*X_Matrix)) -- Logistic
-- Likely High Bid = Suggested Reserve Maximum = Beta0 + BETA_Vector*X_Matrix -- Linear

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
,A.SellerCode
,A.SellerName
,A.Latitude
,A.Longitude
,A.BidEndDT
,A.PropertyOccupancyStatus
,A.Condition
,A.PropertyType
,A.ProductType
,A.RunNum
,A.FinancingAvailable
,A.ALRRegistrants
,A.TotalRegistrantScore
,A.IntrinsicBidderQuality
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
,A.StartingBid
,A.Reserve
,A.BPO
,CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 1.2*A.Reserve ELSE A.BPO END AS FunctionalBPO
,A.HighBid
,A.IsSubjectToApproval AS IsSubjectToApproval
,A.IsSold AS AuctionDaySale
,A.TrueSold AS SellerAcceptedSale
,A.AuctionReserveSuccess AS ReserveExceeded

--Build Likelihood to Auction Day Sale
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
B.Intercept +
A.ADC_Historical_Execution * B.ADC_Historical_Execution +
A.Appreciation * B.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 1.2*A.Reserve ELSE A.BPOSurplus END) * B.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 0.2 ELSE A.BPOSurplusPct END) * B.BPOSurplusPct +
A.C2C_Flag * B.C2C_Flag +
A.Condition_poor * B.Condition_poor +
A.Condition_fair * B.Condition_fair +
A.Condition_average * B.Condition_average +
A.Condition_good * B.Condition_good +
A.Condition_unknown * B.Condition_unknown +
A.FinancingAvailable * B.FinancingAvailable +
A.ForeclosedPer10000 * B.ForeclosedPer10000 +
A.FullWarrantyDeed * B.FullWarrantyDeed +
A.HasPrevAttempts * B.HasPrevAttempts +
A.IsSubjectToApproval * B.IsSubjectToApproval +
A.MedianPPSQFT * B.MedianPPSQFT +
A.MedianPPSQFTDeviation * B.MedianPPSQFTDeviation +
A.MultiResidence_Flag * B.MultiResidence_Flag +
A.PrevForeclosureSoldRate * B.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * B.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * B.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * B.PropertyOccupancyStatus_Vacant +
A.QuitClaimDeed * B.QuitClaimDeed +
A.REO_Flag * B.REO_Flag +
A.Reserve * B.Reserve +
A.RunNum * B.RunNum +
A.SellerPortfolioQuality * B.SellerPortfolioQuality +
A.SFR_Flag * B.SFR_Flag +
A.ShortSale_Flag * B.ShortSale_Flag + 
A.SpecialWarrantyDeed * B.SpecialWarrantyDeed +
ISNULL(A.TotalActive, 8000) * B.TotalActive +
CAST(ISNULL(A.Vacancy,0.087) AS Decimal) * B.Vacancy +
A.ValueLevel_Low * B.ValueLevel_Low +
A.ValueLevel_Med * B.ValueLevel_Med +
A.ValueLevel_High * B.ValueLevel_High
)))
) AS AuctionDaySalePrediction

--Build Likelihood to Sell Outright OR Seller Accepts the Sale
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
C.Intercept +
A.ADC_Historical_Execution * C.ADC_Historical_Execution +
A.Appreciation * C.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 1.2*A.Reserve ELSE A.BPOSurplus END) * C.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 0.2 ELSE A.BPOSurplusPct END) * C.BPOSurplusPct +
A.C2C_Flag * C.C2C_Flag +
A.Condition_poor * C.Condition_poor +
A.Condition_fair * C.Condition_fair +
A.Condition_average * C.Condition_average +
A.Condition_good * C.Condition_good +
A.Condition_unknown * C.Condition_unknown +
A.FinancingAvailable * C.FinancingAvailable +
A.ForeclosedPer10000 * C.ForeclosedPer10000 +
A.FullWarrantyDeed * C.FullWarrantyDeed +
A.HasPrevAttempts * C.HasPrevAttempts +
A.IsSubjectToApproval * C.IsSubjectToApproval +
A.MedianPPSQFT * C.MedianPPSQFT +
A.MedianPPSQFTDeviation * C.MedianPPSQFTDeviation +
A.MultiResidence_Flag * C.MultiResidence_Flag +
A.PrevForeclosureSoldRate * C.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * C.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * C.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * C.PropertyOccupancyStatus_Vacant +
A.QuitClaimDeed * C.QuitClaimDeed +
A.REO_Flag * C.REO_Flag +
A.Reserve * C.Reserve +
A.RunNum * C.RunNum +
A.SellerPortfolioQuality * C.SellerPortfolioQuality +
A.SFR_Flag * C.SFR_Flag +
A.ShortSale_Flag * C.ShortSale_Flag + 
A.SpecialWarrantyDeed * C.SpecialWarrantyDeed +
ISNULL(A.TotalActive,8000) * C.TotalActive +
CAST(CAST(ISNULL(A.Vacancy,0.087) AS Decimal) AS Decimal) * C.Vacancy +
A.ValueLevel_Low * C.ValueLevel_Low +
A.ValueLevel_Med * C.ValueLevel_Med +
A.ValueLevel_High * C.ValueLevel_High
)))
) AS SellerAcceptedSalePrediction

--Build Likelihood to Exceed Reserve
,(CAST(1 AS DECIMAL)/ (1 + EXP((-1)*(
D.Intercept +
A.ADC_Historical_Execution * D.ADC_Historical_Execution +
A.Appreciation * D.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 1.2*A.Reserve ELSE A.BPOSurplus END) * D.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 0.2 ELSE A.BPOSurplusPct END) * D.BPOSurplusPct +
A.C2C_Flag * D.C2C_Flag +
A.Condition_poor * D.Condition_poor +
A.Condition_fair * D.Condition_fair +
A.Condition_average * D.Condition_average +
A.Condition_good * D.Condition_good +
A.Condition_unknown * D.Condition_unknown +
A.FinancingAvailable * D.FinancingAvailable +
A.ForeclosedPer10000 * D.ForeclosedPer10000 +
A.FullWarrantyDeed * D.FullWarrantyDeed +
A.HasPrevAttempts * D.HasPrevAttempts +
A.IsSubjectToApproval * D.IsSubjectToApproval +
A.MedianPPSQFT * D.MedianPPSQFT +
A.MedianPPSQFTDeviation * D.MedianPPSQFTDeviation +
A.MultiResidence_Flag * D.MultiResidence_Flag +
A.PrevForeclosureSoldRate * D.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * D.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * D.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * D.PropertyOccupancyStatus_Vacant +
A.QuitClaimDeed * D.QuitClaimDeed +
A.REO_Flag * D.REO_Flag +
A.Reserve * D.Reserve +
A.RunNum * D.RunNum +
A.SellerPortfolioQuality * D.SellerPortfolioQuality +
A.SFR_Flag * D.SFR_Flag +
A.ShortSale_Flag * D.ShortSale_Flag + 
A.SpecialWarrantyDeed * D.SpecialWarrantyDeed +
ISNULL(A.TotalActive,8000) * D.TotalActive +
CAST(CAST(ISNULL(A.Vacancy,0.087) AS Decimal) AS Decimal) * D.Vacancy +
A.ValueLevel_Low * D.ValueLevel_Low +
A.ValueLevel_Med * D.ValueLevel_Med +
A.ValueLevel_High * D.ValueLevel_High
)))
) AS ExceedReservePrediction

--Build Likely High Bid, Suggested Reserve
,(
E.Intercept +
A.ADC_Historical_Execution * E.ADC_Historical_Execution +
A.Appreciation * E.Appreciation +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 1.2*A.Reserve ELSE A.BPOSurplus END) * E.BPOSurplus +
(CASE WHEN(A.BPO < 0.15 * A.Reserve OR A.BPO IS NULL) THEN 0.2 ELSE A.BPOSurplusPct END) * E.BPOSurplusPct +
A.C2C_Flag * E.C2C_Flag +
A.Condition_poor * E.Condition_poor +
A.Condition_fair * E.Condition_fair +
A.Condition_average * E.Condition_average +
A.Condition_good * E.Condition_good +
A.Condition_unknown * E.Condition_unknown +
A.FinancingAvailable * E.FinancingAvailable +
A.ForeclosedPer10000 * E.ForeclosedPer10000 +
A.FullWarrantyDeed * E.FullWarrantyDeed +
--A.HasLikelyBidders * E.HasLikelyBidders +
A.HasPrevAttempts * E.HasPrevAttempts +
--A.HasWellMatchedBidders * E.HasWellMatchedBidders +
A.IntrinsicBidderQuality * E.IntrinsicBidderQuality +
A.IsSubjectToApproval * E.IsSubjectToApproval +
A.MedianPPSQFT * E.MedianPPSQFT +
A.MedianPPSQFTDeviation * E.MedianPPSQFTDeviation +
A.MultiResidence_Flag * E.MultiResidence_Flag +
A.PrevForeclosureSoldRate * E.PrevForeclosureSoldRate +
A.PropertyOccupancyStatus_Occupied * E.PropertyOccupancyStatus_Occupied +
A.PropertyOccupancyStatus_Unknown * E.PropertyOccupancyStatus_Unknown +
A.PropertyOccupancyStatus_Vacant * E.PropertyOccupancyStatus_Vacant +
A.QuitClaimDeed * E.QuitClaimDeed +
A.REO_Flag * E.REO_Flag +
A.Reserve * E.Reserve +
A.RunNum * E.RunNum +
A.SellerPortfolioQuality * E.SellerPortfolioQuality +
A.SFR_Flag * E.SFR_Flag +
A.ShortSale_Flag * E.ShortSale_Flag + 
A.SpecialWarrantyDeed * E.SpecialWarrantyDeed +
ISNULL(A.TotalActive,8000) * E.TotalActive +
CAST(CAST(ISNULL(A.Vacancy,0.087) AS Decimal) AS Decimal) * E.Vacancy +
A.ValueLevel_Low * E.ValueLevel_Low +
A.ValueLevel_Med * E.ValueLevel_Med +
A.ValueLevel_High * E.ValueLevel_High
) AS RecommendedMaxReservePrice

--Build Likely Valuation
,(
F.Intercept +
A.Age * F.Age +
A.Baths * F.Baths +
A.Bedrooms * F.Bedrooms +
A.Condition_average * F.Condition_average +
A.Condition_fair * F.Condition_fair +
A.Condition_poor * F.Condition_poor +
A.Condition_good * F.Condition_good +
A.Condition_unknown * F.Condition_unknown +
A.HomeSquareFootage * F.HomeSquareFootage +
(A.HomeSquareFootage * A.Condition_average) * F.[HomeSquareFootage Condition_average] +
(A.HomeSquareFootage * A.Condition_fair) * F.[HomeSquareFootage Condition_fair] +
(A.HomeSquareFootage * A.Condition_poor) * F.[HomeSquareFootage Condition_poor] +
(A.HomeSquareFootage * A.Condition_good) * F.[HomeSquareFootage Condition_good] +
(A.HomeSquareFootage * A.Condition_unknown) * F.[HomeSquareFootage Condition_unknown] +
(A.HomeSquareFootage * A.MedianPPSQFT) * F.[HomeSquareFootage MedianPPSQFT] +
A.MultiResidence_Flag * F.MultiResidence_Flag +
A.SFR_Flag * F.SFR_Flag
) AS BPO_Estimate

--Include Rundate for Indexing
,CONVERT (date, SYSDATETIME()) AS ModelRunDate

INTO
#PredictionPreOutput

FROM
#PredictionSourceTable A
JOIN
PredictiveData.dbo.AuctionDaySaleCoefficients B ON (A.PropertyState = B.PropertyState)
JOIN
PredictiveData.dbo.SellerAcceptedSaleCoefficients C ON (A.PropertyState = C.PropertyState)
JOIN
PredictiveData.dbo.ExceedReserveCoefficients D ON (A.PropertyState = D.PropertyState)
JOIN
PredictiveData.dbo.HighBidCoefficients E ON (A.PropertyState = E.PropertyState)
JOIN
PredictiveData.dbo.BPOEstimationCoefficients F ON (A.PropertyState = F.PropertyState)

ORDER BY
BidEndDT
,AuctionCode
,GlobalPropertyId;

CREATE NONCLUSTERED INDEX GlobalPropertyId ON #PredictionPreOutput (GlobalPropertyId);




--Create a Fault Suggestor For This Set
SELECT
  GlobalPropertyId
  ,PropertyId
  ,VenueId
  ,AuctionId
  ,PropertyFaultVariable
  ,PropertyFaultName
  ,[Value] *CASE	WHEN(	NEG.PropertyFaultVariable = 'ADC_Historical_Execution') THEN FA.ADC_Historical_Execution
					WHEN(	NEG.PropertyFaultVariable = 'Appreciation') THEN FA.Appreciation
					WHEN(	NEG.PropertyFaultVariable = 'BPOSurplus') THEN (COALESCE(FA.BPO, 1.2*FA.Reserve) - FA.Reserve)
					WHEN(	NEG.PropertyFaultVariable = 'BPOSurplusPct') THEN ((COALESCE(FA.BPO, 1.2*FA.Reserve) - FA.Reserve) / FA.Reserve)
					WHEN(	NEG.PropertyFaultVariable = 'C2C_Flag') THEN FA.C2C_Flag
					WHEN(	NEG.PropertyFaultVariable = 'Condition_average') THEN FA.Condition_average
					WHEN(	NEG.PropertyFaultVariable = 'Condition_fair') THEN FA.Condition_fair 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_good') THEN FA.Condition_good 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_poor') THEN FA.Condition_poor 
					WHEN(	NEG.PropertyFaultVariable = 'Condition_unknown') THEN FA.Condition_unknown 
					WHEN(	NEG.PropertyFaultVariable = 'FinancingAvailable') THEN FA.FinancingAvailable 
					WHEN(	NEG.PropertyFaultVariable = 'ForeclosedPer10000') THEN FA.ForeclosedPer10000 
					WHEN(	NEG.PropertyFaultVariable = 'FullWarrantyDeed') THEN FA.FullWarrantyDeed 
					WHEN(	NEG.PropertyFaultVariable = 'HasPrevAttempts') THEN FA.HasPrevAttempts 
					WHEN(	NEG.PropertyFaultVariable = 'IsSubjectToApproval') THEN FA.IsSubjectToApproval 
					WHEN(	NEG.PropertyFaultVariable = 'MedianPPSQFT') THEN FA.MedianPPSQFT 
					WHEN(	NEG.PropertyFaultVariable = 'MedianPPSQFTDeviation') THEN FA.MedianPPSQFTDeviation 
					WHEN(	NEG.PropertyFaultVariable = 'MultiResidence_Flag') THEN FA.MultiResidence_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'PrevForeclosureSoldRate') THEN FA.PrevForeclosureSoldRate 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Occupied') THEN FA.PropertyOccupancyStatus_Occupied 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Unknown') THEN FA.PropertyOccupancyStatus_Unknown 
					WHEN(	NEG.PropertyFaultVariable = 'PropertyOccupancyStatus_Vacant') THEN FA.PropertyOccupancyStatus_Vacant 
					WHEN(	NEG.PropertyFaultVariable = 'QuitClaimDeed') THEN FA.QuitClaimDeed 
					WHEN(	NEG.PropertyFaultVariable = 'REO_Flag') THEN FA.REO_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'Reserve') THEN FA.Reserve 
					WHEN(	NEG.PropertyFaultVariable = 'RunNum') THEN FA.RunNum 
					WHEN(	NEG.PropertyFaultVariable = 'SellerPortfolioQuality') THEN FA.SellerPortfolioQuality 
					WHEN(	NEG.PropertyFaultVariable = 'SFR_Flag') THEN FA.SFR_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'ShortSale_Flag') THEN FA.ShortSale_Flag 
					WHEN(	NEG.PropertyFaultVariable = 'SpecialWarrantyDeed') THEN FA.SpecialWarrantyDeed 
					WHEN(	NEG.PropertyFaultVariable = 'TotalActive') THEN FA.TotalActive 
					WHEN(	NEG.PropertyFaultVariable = 'Turnover') THEN FA.Turnover 
					WHEN(	NEG.PropertyFaultVariable = 'Vacancy') THEN FA.Vacancy 
					WHEN(	NEG.PropertyFaultVariable = 'ValueLevel_High') THEN FA.ValueLevel_High 
					WHEN(	NEG.PropertyFaultVariable = 'ValueLevel_Low') THEN FA.ValueLevel_Low ELSE 
					FA.ValueLevel_Med END AS ComponentValue
	
	INTO #PropertyFaultPreOutput

	FROM
	#PredictionSourceTable FA
	JOIN
	PredictiveData.dbo.NonTrusteeNegativeAttributeLookup NEG
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

INTO #NegativeFactors
FROM(  
SELECT
ROW_NUMBER() OVER( PARTITION BY PFPO.GlobalPropertyId ORDER BY PFPO.ComponentValue ASC) AS RowId
,PFPO.*
FROM
#PropertyFaultPreOutput PFPO
WHERE ComponentValue IS NOT NULL
AND PropertyFaultVariable != 'Intercept'
AND PropertyFaultVariable != 'Reserve') T1
WHERE RowId = 1 or RowId = 2



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
,A.SellerCode
,A.SellerName
,A.Latitude
,A.Longitude
,A.BidEndDT
,A.PropertyOccupancyStatus
,A.Condition
,A.PropertyType
,A.ProductType
,A.RunNum
,A.FinancingAvailable
,A.ALRRegistrants
,A.TotalRegistrantScore
,A.IntrinsicBidderQuality
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
,A.StartingBid
,A.Reserve
,A.BPO
,A.FunctionalBPO
,A.HighBid
,A.IsSubjectToApproval
,A.AuctionDaySale
,A.SellerAcceptedSale
,A.ReserveExceeded
,CASE WHEN (A.AuctionDaySalePrediction < CASE WHEN (A.SellerAcceptedSalePrediction < A.ExceedReservePrediction) THEN A.ExceedReservePrediction ELSE A.SellerAcceptedSalePrediction END)
 THEN CASE WHEN (A.SellerAcceptedSalePrediction < A.ExceedReservePrediction) THEN A.ExceedReservePrediction ELSE A.SellerAcceptedSalePrediction END 
	  ELSE A.AuctionDaySalePrediction END AS AuctionDaySalePrediction
,CASE WHEN (A.SellerAcceptedSalePrediction < A.ExceedReservePrediction) THEN A.ExceedReservePrediction ELSE A.SellerAcceptedSalePrediction END AS SellerAcceptedSalePrediction
,A.ExceedReservePrediction
,CASE WHEN (A.RecommendedMaxReservePrice < A.StartingBid) THEN A.StartingBid ELSE A.RecommendedMaxReservePrice END AS RecommendedMaxReservePrice
,ModelRunDate
,1 As MostRecentData
,B.PropertyFaultName AS Negative1
,C.PropertyFaultName AS Negative2

INTO #PredictionOutput_Auto

FROM
#PredictionPreOutput A
JOIN
#NegativeFactors B ON ( B.RowId = 1 AND A.GlobalPropertyId = B.GlobalPropertyId) 
JOIN
#NegativeFactors C ON ( C.RowId = 2 AND A.GlobalPropertyId = C.GlobalPropertyId)

ORDER BY
BidEndDT
,AuctionCode
,GlobalPropertyId;



--Put the Prediction Table into it's two destination tables, one with all historical info, another with only the most recent data

--Update existing historical record to show that previous data is now not the most recent:

UPDATE PredictiveData.dbo.PredictionOutput_Auto
SET MostRecentData = 0

--Append to historical record
INSERT INTO PredictiveData.dbo.PredictionOutput_Auto
SELECT * FROM #PredictionOutput_Auto;


--Update the "Most Recent Prediction" Table
DROP TABLE PredictiveData.dbo.MostRecentPredictionOutput_Auto;
SELECT * INTO PredictiveData.dbo.MostRecentPredictionOutput_Auto
FROM #PredictionOutput_Auto;


DROP TABLE #TemporaryFactAuction
DROP TABLE #bidder_history_raw;
DROP TABLE #bidder_budget_criteria;
DROP TABLE #bidder_attendance_scoring;
DROP TABLE #ALRScore;
DROP TABLE #HighBidHistory;
DROP TABLE #Favorites;
DROP TABLE #ADCExecution;
DROP TABLE #SellerPortfolioQuality;
DROP TABLE #PredictionPreOutput;
DROP TABLE #PredictionSourceTable;
DROP TABLE #PredictionOutput_Auto;
DROP TABLE #NegativeFactors;
DROP TABLE #PropertyFaultPreOutput;

END

/*
DROP PROCEDURE DataScience_NonTrusteePredictionData

DROP TABLE PredictiveData.dbo.PredictionOutput_Auto


SELECT * FROM PredictiveData.dbo.PredictionOutput_Auto
SELECT * FROM PredictiveData.dbo.MostRecentPredictionOutput_Auto
*/

GO


