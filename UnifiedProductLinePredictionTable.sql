
SELECT
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
,A.AuctionDate
,NULL AS SellerCode
,NULL AS SellerName
,NULL AS TrusteeCode
,NULL AS TrusteeName
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
,NULL AS IsCancelled
,NULL AS IsPostponed
,NULL AS IsRemoved
,NULL AS IsAuction
,A.AuctionDaySalePrediction
,A.SellerAcceptedSalePrediction
,A.ExceedReservePrediction
,A.RecommendedMaxReservePrice
,NULL AS PostponementPrediction
,NULL AS CancellationPrediction
,NULL AS PostponementPrediction
,NULL AS ThirdPartySaleIfAuctionedPrediction
,NULL AS ThirdPartySaleOverallPrediction
,A.ModelRunDate
FROM
PredictiveData.[dbo].[PredictionOutput_Auto] A
WHERE A.MostRecentData = 1

UNION ALL (

SELECT
B.GlobalPropertyId
,B.SystemAuctionId
,B.VenueId
,B.AuctionId
,B.PropertyId
,B.REDCID
,B.LoanNbr
,B.PropertyState
,B.PropertyZip
,B.PropertyCounty
,B.PropertyCity
,B.MSAName
,B.AuctionCode
,B.AuctionDate
,NULL AS SellerCode
,NULL AS SellerName
,B.TrusteeCode
,B.TrusteeName
,B.PropertyOccupancyStatus
,B.Condition
,B.PropertyType
,B.ProductType
,B.RunNum
,NULL AS FinancingAvailable
,NULL AS ALRRegistrants
,NULL AS TotalRegistrantScore
,NULL AS IntrinsicBidderQuality
,B.TotalActive
,B.Vacancy
,B.Appreciation
,B.Turnover
,B.TotalHomesForSale
,B.PercentHomesSold
,B.MedianPPSQFT
,B.MedianPPSQFTDeviation
,B.ForeclosedPer10000
,B.PrevForeclosureSoldRate
,B.HomeSquareFootage
,B.Bedrooms
,B.Baths
,B.LotSize
,B.Age
,B.OpeningBid AS StartingBid
,B.Reserve
,B.BPO
,B.FunctionalBPO
,B.HighBid
,NULL AS IsSubjectToApproval
,IsSold AS AuctionDaySale
,IsSold AS SellerAcceptedSale
,IsSold AS ReserveExceeded
,IsCancelled
,IsPostponed
,IsRemoved
,IsAuction
,ThirdPartySaleOverallPrediction AS AuctionDaySalePrediction
,ThirdPartySaleOverallPrediction AS SellerAcceptedSalePrediction
,ThirdPartySaleOverallPrediction AS ExceedReservePrediction
,B.RecommendedMaxReservePrice
,PostponementPrediction
,CancellationPrediction
,PostponementPrediction
,ThirdPartySaleIfAuctionedPrediction
,ThirdPartySaleOverallPrediction
,B.ModelRunDate
FROM
PredictiveData.[dbo].[TrusteePredictionOutput_Auto] B
WHERE B.MostRecentData = 1)