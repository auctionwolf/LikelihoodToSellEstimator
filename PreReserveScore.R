##Unified Multiple State, Reserve Segmented Bayesian LASSO
library(lubridate)
library(penalized)
library(glmnet)
library(stringr)

#Select the run date
rundate = format(Sys.time(), "%Y-%m-%d")
#Select the training data ending date, this is the date starting the test data
date = format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(30), "%Y-%m-%d")
HUD_FLAG = 0
Acceptance_Flag = 0


#Import Auction.com Data And set the bounds on the training data
filename = paste0("~/R/Data/UnifiedPredictionData_",rundate,".csv")
DATA = read.csv(file = filename)
DATA$AuctionDate = mdy(as.character(DATA$AuctionDate))
DATA = DATA[DATA$AuctionDate >= ymd(format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(120), "%Y-%m-%d")),]

#Remove assets without a reserve
DATA$Reserve = as.numeric(as.character(DATA$Reserve))
DATA = DATA[is.na(DATA$Reserve)==F,]

#Remove Any BPOs with a BPO Surplus PCT > 5x
DATA$BPOSurplusPct = as.numeric(as.character(DATA$BPOSurplusPct))
DATA$BPO[DATA$BPOSurplusPct >= 5.0 | DATA$BPOSurplusPct < -0.90] = NA

#Remove Commas
DATA$SellerName = gsub(x=DATA$SellerName, pattern=",", replacement="", fixed=T, )
DATA$PropertyCity = gsub(x=DATA$PropertyCity, pattern=",", replacement="", fixed=T, )

#Fix the appreciation and turnover data
DATA$Turnover = as.numeric(as.character(DATA$Turnover))
DATA$Appreciation = as.numeric(as.character(DATA$Appreciation))
DATA$MedianPPSQFT = as.numeric(as.character(DATA$MedianPPSQFT))
DATA$MedianPPSQFTDeviation = as.numeric(as.character(DATA$MedianPPSQFTDeviation))
DATA$ForeclosedPer10000 = as.numeric(as.character(DATA$ForeclosedPer10000))
DATA$PrevForeclosureSoldRate = as.numeric(as.character(DATA$PrevForeclosureSoldRate))
DATA$PercentHomesSold = as.numeric(as.character(DATA$PercentHomesSold))

#When one of the Zillow Metrics are missing, they all are generally. 
#Except The Deviation Field, that is also missing when a reserve is missing too.
MissingZillow = which(is.na(DATA$Turnover))


#Reduce the ALR metrics into flags
DATA$HasLikelyBidders = 0
DATA$HasLikelyBidders[DATA$IntrinsicBidderQuality >= .5] = 1
DATA$HasWellMatchedBidders = 0
DATA$HasWellMatchedBidders[DATA$TotalRegistrantScore >= .5] = 1

#Interpolate
iterations = length(MissingZillow)
#Give County-Level Approximations to the properties that fell outside a Zillow ZIP, usually 1.5%
for(i in 1:iterations){
  DATA$Turnover[MissingZillow[i]] = mean(DATA$Turnover[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                       & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                         ,na.rm=T) 
  DATA$Appreciation[MissingZillow[i]] = mean(DATA$Appreciation[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                               & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                             ,na.rm=T)
  DATA$MedianPPSQFT[MissingZillow[i]] = mean(DATA$MedianPPSQFT[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                               & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                             ,na.rm=T)
  DATA$MedianPPSQFTDeviation[MissingZillow[i]] = mean(DATA$MedianPPSQFTDeviation[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                                                 & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                                      ,na.rm=T)
  DATA$ForeclosedPer10000[MissingZillow[i]] = mean(DATA$ForeclosedPer10000[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                                           & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                                   ,na.rm=T)
  DATA$PrevForeclosureSoldRate[MissingZillow[i]] = mean(DATA$PrevForeclosureSoldRate[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                                                     & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                                        ,na.rm=T)
  DATA$PercentHomesSold[MissingZillow[i]] = mean(DATA$PercentHomesSold[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]
                                                                                     & DATA$PropertyCounty==DATA$PropertyCounty[MissingZillow[i]]]
                                                        ,na.rm=T) 
}
#Knocked out half of the missing...
MissingZillow = which(is.na(DATA$Turnover))
#Interpolate
iterations = length(MissingZillow)
#Give State-Level Approximations to the properties that fell outside a Zillow ZIP
if(length(iterations)>0){
  for(i in 1:iterations){
    DATA$Turnover[MissingZillow[i]] = mean(DATA$Turnover[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                           ,na.rm=T) 
    DATA$Appreciation[MissingZillow[i]] = mean(DATA$Appreciation[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                               ,na.rm=T)
    DATA$MedianPPSQFT[MissingZillow[i]] = mean(DATA$MedianPPSQFT[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                               ,na.rm=T)
    DATA$MedianPPSQFTDeviation[MissingZillow[i]] = mean(DATA$MedianPPSQFTDeviation[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                                        ,na.rm=T)
    DATA$ForeclosedPer10000[MissingZillow[i]] = mean(DATA$ForeclosedPer10000[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                                     ,na.rm=T)
    DATA$PrevForeclosureSoldRate[MissingZillow[i]] = mean(DATA$PrevForeclosureSoldRate[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                                          ,na.rm=T)
    DATA$PercentHomesSold[MissingZillow[i]] = mean(DATA$PercentHomesSold[DATA$PropertyState==DATA$PropertyState[MissingZillow[i]]]
                                                          ,na.rm=T)
  }
}

#If Still Missing, Give them median of the country
MissingZillow = which(is.na(DATA$Turnover))
#Interpolate
iterations = length(MissingZillow)
#Give State-Level Approximations to the properties that fell outside a Zillow ZIP
if(length(iterations)>0){
  for(i in 1:iterations){
    DATA$Turnover[MissingZillow[i]] = median(DATA$Turnover,na.rm=T) 
    DATA$Appreciation[MissingZillow[i]] = median(DATA$Appreciation,na.rm=T)
    DATA$MedianPPSQFT[MissingZillow[i]] = median(DATA$MedianPPSQFT,na.rm=T)
    DATA$MedianPPSQFTDeviation[MissingZillow[i]] = median(DATA$MedianPPSQFTDeviation,na.rm=T)
    DATA$ForeclosedPer10000[MissingZillow[i]] = median(DATA$ForeclosedPer10000,na.rm=T)
    DATA$PrevForeclosureSoldRate[MissingZillow[i]] = median(DATA$PrevForeclosureSoldRate,na.rm=T)
    DATA$PercentHomesSold[MissingZillow[i]] = median(DATA$PercentHomesSold,na.rm=T)
  }
}

#Get Zestimates and pair them
#Import Zestimates
names = c("date","URL","Zestimate")
a = Sys.glob("C:/Users/wrendall/Documents/Zestimates/*")
datas = lapply(a, FUN = read.csv, header = F, col.names = names)
Z = do.call("rbind",datas)

#Get all matchy
Z$GlobalPropertyId = str_extract(str_match(Z$URL, "asset/\\d+"), "\\d+")
#Z$SPID = str_extract(str_match(Z$URL, "itemID=\\d+"), "\\d+")
#Z$VID  = str_extract(str_match(Z$URL, "venueId=\\d+"), "\\d+")

EZZestimate = Z[is.na(Z$GlobalPropertyId)==F,]
EZZestimate$Zestimate = as.numeric(as.character(EZZestimate$Zestimate))
EZZestimate = EZZestimate[EZZestimate$Zestimate > 0 & is.na(EZZestimate$Zestimate) ==F,]
EZZestimate = aggregate(.~GlobalPropertyId, data = EZZestimate[,c("Zestimate","GlobalPropertyId")], mean)

DATA = merge(DATA, EZZestimate, all.x = T)
DATA$BPO = as.numeric(as.character(DATA$BPO))
DATA$IsPredictedBPO = 0
DATA$IsPredictedBPO[is.na(DATA$BPO)==T] = 1
DATA$BPO[is.na(DATA$BPO)==T] = DATA$Zestimate[is.na(DATA$BPO)==T]
DATA$Zestimate[is.na(DATA$Zestimate)==T] = 0
DATA$BPOSurplus = as.numeric(as.character(DATA$BPO)) - as.numeric(as.character(DATA$Reserve))
#DATA$ZestimateSurplus = DATA$Zestimate - DATA$Reserve
#DATA$ZestimateSurplus[is.na(DATA$ZestimateSurplus)==T] = 0
#DATA$ZestimateSurplusPct = DATA$ZestimateSurplus/DATA$Reserve

#Fix SubjectTo Flag
DATA$IsSubjectToApproval = as.numeric(as.character(DATA$IsSubjectToApproval))
DATA$IsSubjectToApproval[is.na(DATA$IsSubjectToApproval)==T] = 0






if(HUD_FLAG == 1){
#Grab New HUD for Predictions
HUD = read.csv(file = "~/MostRecentHudDataForScoring.csv")
soFun = function(x){as.numeric(as.character(x))}
HUD[,9:ncol(HUD)] = apply(HUD[,9:ncol(HUD)], 2, soFun)
HUD_Clean = HUD[is.na(HUD$Reserve)==F,]

#Interpolate
MissingZillow = which(is.na(HUD_Clean$Turnover))
#Interpolate
iterations = length(MissingZillow)
#Give State-Level Approximations to the properties that fell outside a Zillow ZIP
if(length(iterations)>0){
  for(i in 1:iterations){
    HUD_Clean$Turnover[MissingZillow[i]] = mean(HUD_Clean$Turnover[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                           ,na.rm=T) 
    HUD_Clean$Appreciation[MissingZillow[i]] = mean(HUD_Clean$Appreciation[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                               ,na.rm=T)
    HUD_Clean$MedianPPSQFT[MissingZillow[i]] = mean(HUD_Clean$MedianPPSQFT[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                               ,na.rm=T)
    HUD_Clean$ForeclosedPer10000[MissingZillow[i]] = mean(HUD_Clean$ForeclosedPer10000[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                     ,na.rm=T)
    HUD_Clean$PrevForeclosureSoldRate[MissingZillow[i]] = mean(HUD_Clean$PrevForeclosureSoldRate[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                          ,na.rm=T)
    HUD_Clean$PercentHomesSold[MissingZillow[i]] = mean(HUD_Clean$PercentHomesSold[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                   ,na.rm=T)
    HUD_Clean$TotalHomesForSale[MissingZillow[i]] = mean(HUD_Clean$TotalHomesForSale[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                        ,na.rm=T)
  }
}

MissingZillow = which(is.na(HUD_Clean$TotalHomesForSale))
#Interpolate
iterations = length(MissingZillow)
#Give State-Level Approximations to the properties that fell outside a Zillow ZIP
if(length(iterations)>0){
  for(i in 1:iterations){
    HUD_Clean$PercentHomesSold[MissingZillow[i]] = mean(HUD_Clean$PercentHomesSold[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                        ,na.rm=T)
    HUD_Clean$TotalHomesForSale[MissingZillow[i]] = mean(HUD_Clean$TotalHomesForSale[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                         ,na.rm=T)
  }
}

MissingZillow = which(is.na(HUD_Clean$TotalActive))
#Interpolate
iterations = length(MissingZillow)
#Give State-Level Approximations to the properties that fell outside a Zillow ZIP
if(length(iterations)>0){
  for(i in 1:iterations){
    HUD_Clean$TotalActive[MissingZillow[i]] = mean(HUD_Clean$TotalActive[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                        ,na.rm=T)
    HUD_Clean$Vacancy[MissingZillow[i]] = mean(HUD_Clean$Vacancy[HUD_Clean$PropertyState==HUD_Clean$PropertyState[MissingZillow[i]]]
                                                         ,na.rm=T)
  }
}


#Last Fixeroo
HUD_Clean$TrueSold = 0
HUD_Clean$HighBid = 0

}







#Loop regressions over each state in the data set. Note the lack of interaction terms
#Institute floor for observations of 50


a = 1:length(States)
for( i in 1:length(a)){
  a[i] = nrow(DATA[DATA$PropertyState == as.character(States[i]),])
}
States = as.data.frame(cbind(States,a))
names(States) = c("PropertyState","Count")
States = States[as.numeric(as.character(States$Count)) > 30,]
States = as.character(States$PropertyState)
#Create Placeholders for results
Outputlist = numeric(0)
BPOlist = numeric(0)
HBlist = numeric(0)
ADlist = numeric(0)
SAlist = numeric(0)
ERlist = numeric(0)
FullOutputlist = numeric(0)
HUD_Outputlist = numeric(0)
#Press the button!
for(j in 1:length(States)){
  
  #Subset for looping purposes
  REO = DATA[DATA$PropertyState == States[j],]

    #Stabilize the patient -- Sanitize the variables
  #Fix Numerics
  REO[,23:ncol(REO)] = apply(REO[,23:ncol(REO)],2,as.character)
  REO[,23:ncol(REO)] = apply(REO[,23:ncol(REO)],2,as.numeric)
  #Fix History
  REO$Age[REO$Age > 200] = median(REO$Age[REO$Age < 200])
  REO$LotSize[is.na(REO$LotSize)==T] = .1
  REO$Bedrooms[REO$Bedrooms < 0 | is.na(REO$Bedrooms)] = median(REO$Bedrooms[REO$Bedrooms > 0],na.rm=T)
  REO$Baths[REO$Baths < 0] = median(REO$Baths[REO$Baths > 0])
  REO$HomeSquareFootage = as.numeric(as.character(REO$HomeSquareFootage))
  REO$HomeSquareFootage[REO$HomeSquareFootage < 100 | is.na(REO$HomeSquareFootage)==T] = mean(REO$HomeSquareFootage[REO$HomeSquareFootage >=100])
  REO$Favorites = ceiling(REO$Favorites / REO$RunNum)
  #Fix BPO
  REO$BPO = as.numeric(as.character(REO$BPO))
  
  #Fix USPS
  REO$TotalActive[is.na(REO$TotalActive)==T]=median(REO$TotalActive,na.rm=T)
  REO$Vacancy[is.na(REO$Vacancy)==T]=median(REO$Vacancy,na.rm=T)
  
 
  
  
  
  
  #*****************
  
  
  
  
  
  
  
  #Must have all variables present in order to predict.
  TEST = REO[REO$AuctionDate >= ymd(date)
             & is.na(REO$C2C_Flag)==F
             & is.na(REO$REO_Flag)==F
             & is.na(REO$ShortSale_Flag)==F
             & is.na(REO$Trustee_Flag)==F
             & is.na(REO$Condition_poor)==F
             & is.na(REO$Condition_fair)==F
             & is.na(REO$Condition_average)==F
             & is.na(REO$Condition_good)==F
             & is.na(REO$Condition_unknown)==F
             & is.na(REO$PropertyOccupancyStatus_Occupied)==F
             & is.na(REO$PropertyOccupancyStatus_Unknown)==F
             & is.na(REO$PropertyOccupancyStatus_Vacant)==F
             & is.na(REO$FinancingAvailable)==F
             & is.na(REO$Favorites)==F
             & is.na(REO$TotalActive)==F
             & is.na(REO$Vacancy)==F
             & is.na(REO$Appreciation)==F
             & is.na(REO$Turnover)==F
             & is.na(REO$MedianPPSQFT)==F
             ,]
  
  
  #Must have all values to construct a model
  ReservePredictor = REO[REO$AuctionDate < ymd(date) 
                         & is.na(REO$C2C_Flag)==F
                         & is.na(REO$REO_Flag)==F
                         & is.na(REO$ShortSale_Flag)==F
                         & is.na(REO$Trustee_Flag)==F
                         & is.na(REO$Condition_poor)==F
                         & is.na(REO$Condition_fair)==F
                         & is.na(REO$Condition_average)==F
                         & is.na(REO$Condition_good)==F
                         & is.na(REO$Condition_unknown)==F
                         & is.na(REO$PropertyOccupancyStatus_Occupied)==F
                         & is.na(REO$PropertyOccupancyStatus_Unknown)==F
                         & is.na(REO$PropertyOccupancyStatus_Vacant)==F
                         & is.na(REO$FinancingAvailable)==F
                         & is.na(REO$Favorites)==F
                         & is.na(REO$TotalActive)==F
                         & is.na(REO$Vacancy)==F
                         & is.na(REO$Appreciation)==F
                         & is.na(REO$Turnover)==F
                         & is.na(REO$MedianPPSQFT)==F
                         ,]
  
  HighBidPredictor = REO[REO$AuctionDate < ymd(date)
                         & is.na(REO$C2C_Flag)==F
                         & is.na(REO$REO_Flag)==F
                         & is.na(REO$ShortSale_Flag)==F
                         & is.na(REO$Trustee_Flag)==F
                         & is.na(REO$Condition_poor)==F
                         & is.na(REO$Condition_fair)==F
                         & is.na(REO$Condition_average)==F
                         & is.na(REO$Condition_good)==F
                         & is.na(REO$Condition_unknown)==F
                         & is.na(REO$PropertyOccupancyStatus_Occupied)==F
                         & is.na(REO$PropertyOccupancyStatus_Unknown)==F
                         & is.na(REO$PropertyOccupancyStatus_Vacant)==F
                         & is.na(REO$FinancingAvailable)==F
                         & is.na(REO$Favorites)==F
                         & is.na(REO$TotalActive)==F
                         & is.na(REO$Vacancy)==F
                         & is.na(REO$Appreciation)==F
                         & is.na(REO$Turnover)==F
                         & is.na(REO$MedianPPSQFT)==F
                         ,]
  
  if(HUD_FLAG==1){
  HUD_TEST = HUD_Clean[HUD_Clean$PropertyState == States[j],]
  }
  
  Step = 'Test and Training Sets Built'
  
  #Build A regular LASSO Regression for Each Value Level
  #attempt to find good penalties, then fit, then get SEs, then predict, then append results:
  
  #Create Formulas to Tranform Data and shape predictions
  AuctionDaySaleFormula = {as.formula(IsSold
                                      ~SFR_Flag
                                      +MultiResidence_Flag
                                      +C2C_Flag
                                      +REO_Flag
                                      +ShortSale_Flag
                                      #+Trustee_Flag
                                      +Condition_poor
                                      +Condition_fair
                                      +Condition_average
                                      +Condition_good
                                      +Condition_unknown
                                      +PropertyOccupancyStatus_Occupied
                                      +PropertyOccupancyStatus_Unknown
                                      +PropertyOccupancyStatus_Vacant
                                      +FinancingAvailable
                                      +TotalActive
                                      +Vacancy
                                      +Appreciation
                                      +Turnover
                                      +MedianPPSQFT
                                      +ForeclosedPer10000
                                      +PrevForeclosureSoldRate
                                      +IsSubjectToApproval
                                      +FullWarrantyDeed
                                      +SpecialWarrantyDeed
                                      +QuitClaimDeed
                                      +PercentHomesSold
                                      +ADC_Historical_Execution
                                      +SellerPortfolioQuality
  )}
  SellerAcceptsSaleFormula = {as.formula(TrueSold
                                         ~SFR_Flag
                                         +MultiResidence_Flag
                                         +C2C_Flag
                                         +REO_Flag
                                         +ShortSale_Flag
                                         #+Trustee_Flag
                                         +Condition_poor
                                         +Condition_fair
                                         +Condition_average
                                         +Condition_good
                                         +Condition_unknown
                                         +PropertyOccupancyStatus_Occupied
                                         +PropertyOccupancyStatus_Unknown
                                         +PropertyOccupancyStatus_Vacant
                                         +FinancingAvailable
                                         +TotalActive
                                         +Vacancy
                                         +Appreciation
                                         +Turnover
                                         +MedianPPSQFT
                                         +ForeclosedPer10000
                                         +PrevForeclosureSoldRate
                                         +IsSubjectToApproval
                                         +FullWarrantyDeed
                                         +SpecialWarrantyDeed
                                         +QuitClaimDeed
                                         +PercentHomesSold
                                         +ADC_Historical_Execution
                                         +SellerPortfolioQuality
  )}
  ExceedsReserveFormula  = {as.formula(AuctionReserveSuccess
                                       ~SFR_Flag
                                       +MultiResidence_Flag
                                       +C2C_Flag
                                       +REO_Flag
                                       +ShortSale_Flag
                                       #+Trustee_Flag
                                       +Condition_poor
                                       +Condition_fair
                                       +Condition_average
                                       +Condition_good
                                       +Condition_unknown
                                       +PropertyOccupancyStatus_Occupied
                                       +PropertyOccupancyStatus_Unknown
                                       +PropertyOccupancyStatus_Vacant
                                       +FinancingAvailable
                                       +TotalActive
                                       +Vacancy
                                       +Appreciation
                                       +Turnover
                                       +MedianPPSQFT
                                       +ForeclosedPer10000
                                       +PrevForeclosureSoldRate
                                       +IsSubjectToApproval
                                       +FullWarrantyDeed
                                       +SpecialWarrantyDeed
                                       +QuitClaimDeed
                                       +PercentHomesSold
                                       +ADC_Historical_Execution
                                       +SellerPortfolioQuality
  )}
  HighBidFormula = {as.formula(HighBid
                               ~SFR_Flag
                               +MultiResidence_Flag
                               +C2C_Flag
                               +REO_Flag
                               +ShortSale_Flag
                               #+Trustee_Flag
                               +Condition_poor
                               +Condition_fair
                               +Condition_average
                               +Condition_good
                               +Condition_unknown
                               +PropertyOccupancyStatus_Occupied
                               +PropertyOccupancyStatus_Unknown
                               +PropertyOccupancyStatus_Vacant
                               +FinancingAvailable
                               +TotalActive
                               +Vacancy
                               +Appreciation
                               +Turnover
                               +MedianPPSQFT
                               +ForeclosedPer10000
                               +PrevForeclosureSoldRate
                               +IsSubjectToApproval
                               +FullWarrantyDeed
                               +SpecialWarrantyDeed
                               +QuitClaimDeed
                               +PercentHomesSold
                               +ADC_Historical_Execution
                               +SellerPortfolioQuality
  )}
 
  
  #Run Predictions start with AuctionDay Sale (Easiest Criteria)
  x = ReservePredictor
  y = x$IsSold
  x = model.matrix(object = AuctionDaySaleFormula, data = x)
  AD = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cvmr = cv.glmnet(x, y)
  
  Step = 'Auction Day Sale Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = AuctionDaySaleFormula, predx)
  AuctionDaySalePrediction = predict(AD, newx = predx, s = cvmr$lambda.min, type = "response")
  
  Step = 'Auction Day Sale Prediction Run on Training Data'
  
  if(HUD_FLAG==1){
    predx = HUD_TEST
    predx = model.matrix(object = AuctionDaySaleFormula, predx)
    HUD_AuctionDay_Sale_Prediction = predict(AD, newx = predx, s = cvmr$lambda.min, type = "response")
    
    Step = 'Auction Day Sale Prediction Run on HUD Training Data'
  }
  
  
  
  #Continue with Ultimately Accepted (Medium Difficulty Criteria)
  
  x = ReservePredictor
  y = x$TrueSold
  x = model.matrix(object = SellerAcceptsSaleFormula, data = x)
  SA = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cvsa = cv.glmnet(x, y)
  
  Step = 'Seller Accepted Sale Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = SellerAcceptsSaleFormula, predx)
  SellerAcceptedSalePrediction = predict(SA, newx = predx, s = cvsa$lambda.min, type = "response")
    
  Step = 'Seller Accepted Sale Prediction Run on Test Data'
  
  if(HUD_FLAG==1){
  predx = HUD_TEST
  predx = model.matrix(object = SellerAcceptsSaleFormula, predx)
  HUD_Accepted_Sale_Prediction = predict(SA, newx = predx, s = cvsa$lambda.min, type = "response")
  
  Step = 'HUD Seller Accepted Sale Predicted'
  }

  
  #Continue with Exceeds Seller Reserve (Hardest Criteria)
  
  x = ReservePredictor
  y = x$AuctionReserveSuccess
  if(all(unique(y))==0){
    y[which((ReservePredictor$Reserve - ReservePredictor$HighBid)/ReservePredictor$Reserve == 
              min((ReservePredictor$Reserve - ReservePredictor$HighBid)/ReservePredictor$Reserve, na.rm=T))] = 1
  }
  x = model.matrix(object = ExceedsReserveFormula, data = x)
  ER = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cver = cv.glmnet(x, y)
  
  Step = 'Exceeds Reserve Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = ExceedsReserveFormula, predx)
  ExceedsReservePrediction = ExceedsReservePrediction = predict(ER, newx = predx, s = cver$lambda.min, type = "response")
  
  Step = 'Exceeds Reserve Prediction Run on Test Data'
  
  if(HUD_FLAG==1){
    predx = HUD_TEST
    predx = model.matrix(object = ExceedsReserveFormula, predx)
    HUD_Exceeds_Reserve_Prediction = predict(ER, newx = predx, s = cver$lambda.min, type = "response")
    
    Step = 'HUD Exceeds Reserve Predicted'
  }
  
  
  
  #Continue with High Bid Prediction
  
  #x = HighBidPredictor
  #y = x$HighBid
  #x = model.matrix(object = HighBidFormula, data = x)
  #HB = glmnet(x, y, family = "gaussian", alpha = 0.005, standardize = F)
  #cvhb = cv.glmnet(x, y, lambda = HB$lambda)
  
  #Step = 'High Bid Prediction Run on Training Data'
  
  #predx = TEST
  #predx$HighBid[is.na(predx$HighBid)==T]  = 0
  #predx = model.matrix(object = HighBidFormula, predx)
  #HighBidPrediction = predict(HB, newx = predx, s = HB$lambda[length(HB$lambda)], type = "response") 
  
  #Step = "High Bid Prediction Run on Test Data"
  
  if(HUD_FLAG==1){
  predx = HUD_TEST
  predx = model.matrix(object = HighBidFormula, predx)
  HUD_HighBid_Prediction = predict(HB, newx = predx, s = HB$lambda[length(HB$lambda)], type = "response")
  
  Step = 'HUD High Bid Predicted'
  }

  
  
  
  
  
  
  
  
  
  
  #******************
  
  
  
  
  
  
  
  
  
  
  
  
  #PropertyFault Suggestor
  
  #Gather the model coefficients
  LAD1 = cbind(States[j], "AuctionDaySale", as.vector(coef(AD, s = cvmr$lambda.min))[-2])
  LSA1 = cbind(States[j], "SellerAccepted", as.vector(coef(SA, s = cvsa$lambda.min))[-2])
  LER1 = cbind(States[j], "ExceedsReserve", as.vector(coef(ER, s = cver$lambda.min))[-2])
  LHB1 = cbind(States[j], "HighBid", as.vector(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2]) #Remove the model.matrix intercept
  #Name the rows to keep things ordered
  rownames(LAD1) = rownames(coef(AD, s = cvmr$lambda.min))[-2]
  rownames(LSA1) = rownames(coef(SA, s = cvsa$lambda.min))[-2]
  rownames(LER1) = rownames(coef(ER, s = cver$lambda.min))[-2]
  rownames(LHB1) = rownames(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2]
  #Bind the list to save for export
  ADlist = rbind(ADlist,LAD1)
  SAlist = rbind(SAlist,LSA1)
  ERlist = rbind(ERlist,LER1)
  HBlist = rbind(HBlist,LHB1)
  
  
  
  #Matrix Math to make a ranked list. Fun! 
  
  #Get the names of the descriptor variables
  names(TEST)[22:ncol(TEST)]
  rownames(LAD1)
  rownames(LHB1)[2:ncol(LHB1)]
  
  #Now just the ones that match Those
  names(TEST)[22:ncol(TEST)][names(TEST)[22:ncol(TEST)] %in% rownames(LAD1)]
  names(TEST)[22:ncol(TEST)][names(TEST)[22:ncol(TEST)] %in% rownames(LHB1)]
  
  #Construct Values By employing both LAD and LHB Lists, Preferencing LAD
  LHB1 = LHB1[which(! rownames(LHB1) %in% c("HasWellMatchedBidders", "HasLikelyBidders", "IntrinsicBidderQuality", "TotalRegistrantScore")),]
  A = rownames(LAD1)
  A = A[-1]
  Identity = rep(0,nrow(LAD1))
  for(i in 1:length(Identity)){
    Identity[i] = ifelse(LAD1[i,3] == 0, as.numeric(LHB1[i,3])/max(abs(as.numeric(LHB1[,3]))), LAD1[i,3]) 
  }
  Identity = Identity[-1]
  Identity = as.numeric(Identity)
  B = Identity
  C = as.matrix(TEST[, A])

  
  #For the descriptor variables, we'd like to subtract the mean from them, or 'center' them
  #This way, even if a coefficient is positive, but the property is deficient, we can call it out
  #We also want to make sure the categorical vars' means are kept at mean zero so we don't diminish their negative effect
  myfu = function(x){all(x == 1 | x == 0)}
  BitFlags = which(apply(C, 2, myfu)==1)
  Means = apply(C,2,mean)
  Means[BitFlags] = 0
  D1 = C
  for( i in 1:nrow(D1)){
    D1[i,] = D1[i,] - Means
  }
  #Now Matrix Multiply ELEMENTWISE by utilizing as vector and recycling
  D1 = matrix(as.vector(D1) * (B), nrow = nrow(C))
  #Certain things we never want to suggest, like Product Type, and Reserve, so just make those NA, R treats NA as infinity
  D1[,c(
    1,#SFR
    2,#MFR
    3,#C2C
    4,#REO
    5,#ShortSale
    6,#Trustee
    9 #Reserve
    )] = NA
  #Now find the most negative index element
  myfun = function(x){
    minimums = which(x==min(na.exclude(x)))
    minimums = ifelse(length(minimums)!=1,length(A)+1, minimums)
  }
  E1 = apply(D1,1,myfun)
  #now find the second most negative by setting the most negative to zero and retrying
  D2 = D1
  for(i in 1:nrow(D2)){
    D2[i, ifelse( E1[i] > length(A) , 1, E1[i])]=NA #If our placeholder error message is selected, instead choose a benign
  }
  E2 = apply(D2,1,myfun)
  #Now Make a list of Legitimate Names
  A1 = c("Single Family Home in MultiFamily Preferred Region",                  #1
         "Multiple Unit Home in Single Preferred Region",                       #2
         "Product Line Performs Poorly in This State",                          #3
         "Product Line Performs Poorly in This State",                          #4
         "Product Line Performs Poorly in This State",                          #5
         "Product Line Performs Poorly in This State",                          #6
         #"Property Has Many Attempts at Auction",                               #7
         #"Property Has Previous Attempts at Auction",                           #8
         #"Reserve out of Line with Area",                                       #9
         #"Low Dollar Valuation Against Purchase Price Dampening Interest",      #10
         #"Low Appraised Value as Share of Purchase Price Dampening Interest",   #11
         "Condition Suggests Expensive Refurbishment",                          #12          
         "Condition Suggests Expensive Rehab",                                  #13
         "Condition Suggests Low Investment Potential",                         #14
         "Condition Suggests Low Investment Potential",                         #15
         "Condition Information Unknown",                                       #16
         "Occupied Properties Generally Harder to Resell",                      #17
         "Occupancy Status Unknown",                                            #18
         "Vacancy Suggests Potential Condition Problems in This Region",        #19
         "Financing Incentive Suggests Expensive Rehab",                        #20
         "Low Population in ZIP",                                               #21
         "High Vacancy in ZIP",                                                 #22
         #"Low Value Property Suggests Limited ROI",                             #23
         #"Medium Value Property Suggests Limited ROI",                          #24
         #"High Value Property Suggests Little Room for Added Value",            #25
         #"Property Not a Strong Match for Registered Bidders",                 
         #"Registered Bidders Unlikely to Participate",                         
         #"Web Traffic Pattern Indicates Interest from Bargain Buyers",
         "Price Movement in ZIP Suggests Poor Returns on Investment",           #26
         "Low Turnover in ZIP Suggests Difficult Resale",                       #27
         "Low Priced Area",                                                      #28
         #"Property Expensive per SQFT Versus ZIP",                              #29
         #"Reserve High Versus Zillow Zestimate",
         "ZIP has high incidence of foreclosure",                               #30
         "Statistically Few Foreclosures are resold in ZIP",                    #31
         "Seller Likely to Insist on Full Reserve Price",                      #32
         #"Overall Event Size Suggests Asset Will Be Overlooked",               
         #"Number of Assets in Event From This State",
         "Full Warranty Deed In Distressed-Deed Preferred Market",              #33
         "Special Warranty Deed Suggests Moderate Risk for Investors",          #34
         "Quit Claim Deed Suggests High Risk",                                  #35
         "ZIP has low percentage sales rate ",                                 
         "ADC Has Historically Poor Execution in this ZIP",                     #36
         "Seller Portfolio Historically Low-Quality Assets",                    #37
         #Insert New Faults above this comment                  
         "Cannot Suggest Fault")                                                #38
  
  #E1 and E2 Index the worst and second worst problem faced by the property. Return the Name
  Neg1 = A1[E1]
  Neg2 = A1[E2]
  
  Step = "Property Faults Described"
  
  
  
  
  
  
  
  #*******
  
  
  
  
  
  
  
  
#Previous Likelihood to Accept Model  
  
if(Acceptance_Flag == 1){  
  #See if the Seller is Likely To Accept that High Bid
  
  #Build in the acceptance decision criteria
  Accept = TEST
  Accept$HBPct = HighBidPrediction / Accept$Reserve
  Accept$HBSQ  = Accept$HBPct^2
  Accept$HBCu  = Accept$HBPct^3
  Accept$Shortfall = 1 - Accept$HBPct
  Accept$Shortfall[Accept$Shortfall < 0] = 0
  Accept$SFSQ = Accept$Shortfall^2
  Accept$SFCu = Accept$Shortfall^3
  Accept$issold_fa = 0
  Accept$TrueSold_fr = 0
  
  Step = "Subject-To Acceptance Data created"
  
  #Match each property to it's correct state and seller code combination model.
  AcceptancePrediction = 1:nrow(TEST)
  for(k in 1:nrow(TEST)){
    Data = Accept[k, c("SellerCode","PropertyState","HBPct","HBSQ","HBCu","Reserve","BPO","Shortfall",
                       "SFSQ","SFCu","IsSubjectToApproval","issold_fa","TrueSold_fr")]
    #The seller code was the first value stored in the 4-tuple
    SellerIndex = seq(from = 1, to = length(AcceptanceModels), by = 4)
    #State was second
    StatesIndex = seq(from = 2, to = length(AcceptanceModels), by = 4)
    #Find all models for this seller
    ModelsIndex = which(as.character(unlist(AcceptanceModels[SellerIndex])) == Data[1,"SellerCode"])
    #Shift it forward 1 to index for the state of that seller model
    SellerIndexesForStates = SellerIndex[ModelsIndex] + 1
    #Check to see which model indexed matches this state
    ModelsIndex = min(which(as.vector(unlist(AcceptanceModels[SellerIndexesForStates])) == as.character(Data[1,"PropertyState"])))
    #Grab the index for the matched seller-state-model
    ModelsIndex = SellerIndexesForStates[ModelsIndex]
    
    Step = "Models Indexed for state and seller"
    
    #If there was no match found for the state, give it the california seller's model
    if( is.na(ModelsIndex)==T )           {
      ModelsIndex = min(which(unlist(AcceptanceModels[SellerIndexesForStates]) == "CA"))
      ModelsIndex = SellerIndexesForStates[ModelsIndex]
    }
    #If no match was found at all, give it the "other" model, based on all data.
    if( is.na(ModelsIndex)==T )           {
      ModelsIndex = length(AcceptanceModels) -2
    }
    Step = "Acceptand Model No-Match Contingencies Completed"
  
    #Now make the prediction
    AcceptanceForm = as.formula( TrueSold_fr ~ Shortfall + SFSQ + SFCu)
    newx = model.matrix(object = AcceptanceForm, data = Data)
    AO = AcceptanceModels[[ModelsIndex + 1]]
    Acc_Opti = AcceptanceModels[[ModelsIndex + 2]]
    
    pred = predict(object = AO, newx = newx, s = Acc_Opti, type = "response")
    #pred = predict(object = AO, newdata = Data, type = "response")
    AcceptancePrediction[k] = pred
    
    Step = "Acceptance Prediction Rendered"
    
  }
  
  
}
  
  
  
  
  
  SellerAcceptedSalePrediction = ifelse( SellerAcceptedSalePrediction < ExceedsReservePrediction, ExceedsReservePrediction, SellerAcceptedSalePrediction)
  AuctionDaySalePrediction = ifelse( AuctionDaySalePrediction < SellerAcceptedSalePrediction, SellerAcceptedSalePrediction, AuctionDaySalePrediction)
  
  TEST$ValueLevel = "High"
  TEST$ValueLevel[TEST$ValueLevel_Med == 1] = "Med"
  TEST$ValueLevel[TEST$ValueLevel_Low == 1] = "Low"
  TEST$PredictionDate = rundate
  
  
  
  Output = TEST[,c("REDCID","LoanNbr","ValueLevel","ProductType","PropertyType",
                   "AuctionCode","GlobalPropertyId","AuctionId","VenueId","AuctionDate","PredictionDate","SellerCode",
                   "SellerName","PropertyCity","PropertyCounty","PropertyState","PropertyZip","MSAName",
                   "PropertyOccupancyStatus","Condition","Bedrooms",
                   "Baths","LotSize","HomeSquareFootage","RunNum","BPO","StartingBid","Reserve","HighBid",
                   "BPOSurplus","ALRRegistrants", "TotalRegistrantScore","IntrinsicBidderQuality",
                   "Appreciation","Turnover","Zestimate","Favorites","IsPredictedBPO","IsSubjectToApproval","IsSold","TrueSold","AuctionReserveSuccess")]
  Output$Neg1 = Neg1
  Output$Neg2 = Neg2
  
  
  #Create Housing Quality Grade
  Output$AuctionDaySalePrediction = AuctionDaySalePrediction
  Output$SellerAcceptedSalePrediction = SellerAcceptedSalePrediction
  Output$ExceedsReservePrediction = ExceedsReservePrediction
  Output$HighBidPrediction = HighBidPrediction[,1]
  if(Acceptance_Flag == 1){
  Output$AcceptancePrediction = AcceptancePrediction}
  
  #Sometimes, assets have a perfect storm of positive characteristics that make them fall outside normal predictions.
  Output$HighBidPrediction[Output$HighBidPrediction > 1.5*Output$Reserve & Output$HighBidPrediction > 200000] = Output$Reserve[Output$HighBidPrediction > 1.5*Output$Reserve & Output$HighBidPrediction > 200000]*1.5
  
  #Create Suggested Reserve Reduction
  Output$ReserveRedux = ifelse(Output$AuctionDaySalePrediction < .5 & Output$HighBidPrediction < Output$Reserve, 
                               Output$HighBidPrediction - Output$Reserve, 0 )
  
  Outputlist = rbind(Outputlist, Output)
  
  
  
  
  #FullOutputlist
  FullOutput = TEST
  FullOutput$Neg1 = Neg1
  FullOutput$Neg2 = Neg2
  #Create Housing Quality Grade
  FullOutput$AuctionDaySalePrediction = AuctionDaySalePrediction
  FullOutput$SellerAcceptedSalePrediction = SellerAcceptedSalePrediction
  FullOutput$ExceedsReservePrediction = ExceedsReservePrediction
  FullOutput$HighBidPrediction = HighBidPrediction[,1] 
  #Sometimes, assets have a perfect storm of positive characteristics that make predict well above reserve.
  FullOutput$HighBidPrediction[FullOutput$HighBidPrediction > 1.5*FullOutput$Reserve & FullOutput$HighBidPrediction > 200000] = FullOutput$Reserve[FullOutput$HighBidPrediction > 1.5*FullOutput$Reserve & FullOutput$HighBidPrediction > 200000]*1.5
  if(Acceptance_Flag == 1){FullOutput$LikelihoodToAccept = AcceptancePrediction}

  #Create Suggested Reserve Reduction
  FullOutput$ReserveRedux = ifelse(FullOutput$AuctionDaySalePrediction < .5 & FullOutput$HighBidPrediction < FullOutput$Reserve, 
                                   FullOutput$HighBidPrediction - FullOutput$Reserve, 0 )
  FullOutputlist = rbind(FullOutputlist, FullOutput)
  
  if(HUD_FLAG==1){
  #Create Hud-Specific Output
  HUD_Output = HUD_TEST
  HUD_Output$Neg1 = HUD_Neg1
  HUD_Output$Neg2 = HUD_Neg2
  HUD_Output$SalePrediction = HUD_Sale_Prediction
  HUD_Output$SellerAcceptedSalePrediction = HUD_Accepted_Sale_Prediction
  HUD_Output$ExceedsReservePrediction = HUD_Exceeds_Reserve_Prediction
  HUD_Output$HighBidPrediction = HUD_HighBid_Prediction
  HUD_Output$ReserveRedux = ifelse(HUD_Output$SalePrediction < .5 & HUD_Output$HighBidPrediction < HUD_Output$Reserve, 
                               HUD_Output$HighBidPrediction - HUD_Output$Reserve, 0 )
  
  HUD_Outputlist = rbind(HUD_Outputlist, HUD_Output)
  }
}

LikelihoodLevels = c(0, .1, .5, .8, 1)
Outputlist$SaleLikelihood = "Low"
Outputlist$SaleLikelihood[Outputlist$AuctionDaySalePrediction >= LikelihoodLevels[2]] = "Borderline"
Outputlist$SaleLikelihood[Outputlist$AuctionDaySalePrediction >= LikelihoodLevels[3]] = "Moderate"
Outputlist$SaleLikelihood[Outputlist$AuctionDaySalePrediction >= LikelihoodLevels[4]] = "High"

#LikelihoodLevels = quantile(FullOutputlist$AuctionDaySalePrediction, probs = seq(0,1,.25))
LikelihoodLevels = c(0, .1, .5, .8, 1)
FullOutputlist$SaleLikelihood = "Low"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[2]] = "Borderline"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[3]] = "Moderate"
FullOutputlist$SaleLikelihood[FullOutputlist$AuctionDaySalePrediction >= LikelihoodLevels[4]] = "High"


#Do some logical fixing
#Make minimum bid the starting bid
Outputlist$StartingBid[is.na(Outputlist$StartingBid)==T] = min(Outputlist$StartingBid,na.rm=T)
Outputlist$HighBidPrediction[Outputlist$HighBidPrediction < Outputlist$StartingBid] = 
  Outputlist$StartingBid[Outputlist$HighBidPrediction < Outputlist$StartingBid]
FullOutputlist$StartingBid[is.na(FullOutputlist$StartingBid)==T] = min(FullOutputlist$StartingBid,na.rm=T)
FullOutputlist$HighBidPrediction[FullOutputlist$HighBidPrediction < FullOutputlist$StartingBid] = 
  FullOutputlist$StartingBid[FullOutputlist$HighBidPrediction < FullOutputlist$StartingBid]


#And now to write the results to a file
filename = paste("GLMPredictionOutput_", rundate, ".csv", sep = "")
write.csv(Outputlist, filename, row.names = F, quote = F)
#filename = paste("GLMADCoefficientOutput_", rundate, ".csv", sep = "")
#write.csv(ADlist, filename, row.names = T, quote = F)
#filename = paste("GLSADCoefficientOutput_", rundate, ".csv", sep = "")
#write.csv(SAlist, filename, row.names = T, quote = F)
#filename = paste("GLERDCoefficientOutput_", rundate, ".csv", sep = "")
#write.csv(ERlist, filename, row.names = T, quote = F)
#filename = paste("GLMHBCoefficientOutput_", rundate, ".csv", sep = "")
#write.csv(HBlist, filename, row.names = T, quote = F)
filename = paste("GLMPredictionFullOutput_", rundate, ".csv", sep = "")
write.csv(FullOutputlist, filename, row.names = F, quote = F)

filename = paste("WWR_PredictionOutput_WeeklyUpdate_", rundate, ".csv", sep = "")
write.csv(FullOutputlist, filename, row.names = F, quote = F)

if(HUD_FLAG==1){
filename = paste("HUD_PredictionOutput_", rundate, ".csv", sep = "")
write.csv(HUD_Outputlist, filename, row.names = F)
}

#filename = "WWR_BPOEstimationCoefficients.csv"
#write.csv(BPOlist, filename, row.names = T, quote = F)









#Prediction Analytics


hist(FullOutputlist$AuctionDaySalePrediction, xlab= "Sale Likelihood Prediction", main= "Frequency of Per-Attempt Chance to Sell Predictions")
hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$AuctionDate > ymd(rundate)]
     , xlab= "Sale Likelihood Prediction", main= "Upcoming 30 Day Sale Likelihood Predictions")
hist(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$AuctionDate < ymd(rundate)]
     , xlab= "Sale Likelihood Prediction", main= "Trailing 30 Day Sale Likelihood Predictions")
mean(FullOutputlist$AuctionDaySalePrediction)*(mean(is.na(as.numeric(as.character(DATA$HighBid)))))
mean(FullOutputlist$AuctionDaySalePrediction)

hist(round(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$AuctionDate > ymd(rundate)], digits=0) - 
       FullOutputlist$IsSold[FullOutputlist$AuctionDate > ymd(rundate)],
     xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Test Set") 

#Pre-Auction Stats
PrePost = data.frame(rbind(
cbind(
mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate > ymd(rundate)]),
mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate < ymd(rundate)]),
mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate > ymd(rundate)]),
mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate < ymd(rundate)]),
mean(FullOutputlist$BPO[FullOutputlist$AuctionDate > ymd(rundate)]),
mean(FullOutputlist$BPO[FullOutputlist$AuctionDate < ymd(rundate)]),
mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate > ymd(rundate)]),
mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate < ymd(rundate)])
),
cbind(
mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$BPO[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$BPO[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")]),
mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode %in% c("BA1","BV1")])
),
cbind(
  mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate > ymd(rundate) & FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$BPO[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$BPO[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "NST"]),
  mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "NST"])
),
cbind(
  mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate > ymd(rundate) & FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$BPOSurplus[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$Reserve[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$BPO[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$BPO[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate > ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"]),
  mean(FullOutputlist$BPOSurplusPct[FullOutputlist$AuctionDate < ymd(rundate) & ! FullOutputlist$SellerCode == "FNM"])
)
))
names(PrePost) = c("PreBPOSurplus", "PostBPOSurplus", "PreReserve", "PostReserve", "PreBPO", "PostBPO", "PreBPOPct", "PostBPOPct")
rownames(PrePost) = c("All","NoHUD","NSTOnly","FNMOnly")

PrePost$BPODifference = PrePost$PreBPO - PrePost$PostBPO
PrePost$ReserveDifference = PrePost$PreReserve - PrePost$PostReserve


hist(round(FullOutputlist$AuctionDaySalePrediction[FullOutputlist$AuctionDate < ymd(rundate)], digits=0) -
FullOutputlist$IsSold[FullOutputlist$AuctionDate < ymd(rundate)],
xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Validation Set")




#Have a look at unscored Assets

TEST1 = DATA[DATA$AuctionDate > ymd(rundate),]
t = as.data.frame(table(TEST1$AuctionCode))
t = t[t$Freq >0,]
t = as.data.frame(table(TEST1$SellerCode))
t = t[t$Freq >0,]
t = t[t$Var1 %in% c("BA1","BV1","BAJ","FNM","NSO","NST","NTS","NSV"),]