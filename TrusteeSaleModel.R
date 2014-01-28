##Unified Multiple State, Reserve Segmented Bayesian LASSO
library(lubridate)
library(penalized)
library(glmnet)
library(stringr)

#Select the run date
rundate = format(Sys.time(), "%Y-%m-%d")
#Select the training data ending date, this is the date starting the test data
date = format(ymd(format(Sys.time(), "%Y-%m-%d")) - days(30), "%Y-%m-%d")


#Import Auction.com Data And set the bounds on the training data
filename = paste0("~/R/Data/UnifiedTrusteePredictionData_",rundate,".csv")
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
DATA$TrusteeName = gsub(x=DATA$TrusteeName, pattern=",", replacement="", fixed=T, )
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
DATA$HighBid = as.numeric(as.character(DATA$HighBid))
DATA$WinningBid = as.numeric(as.character(DATA$WinningBid))
DATA$HighBid[is.na(DATA$HighBid) == T] = DATA$WinningBid[is.na(DATA$HighBid) == T]
DATA$OpeningBid = as.numeric(as.character(DATA$OpeningBid))
DATA$TotalActive = as.numeric(as.character(DATA$TotalActive))
DATA$Vacancy = as.numeric(as.character(DATA$Vacancy))
#DATA$ZestimateSurplus = DATA$Zestimate - DATA$Reserve
#DATA$ZestimateSurplus[is.na(DATA$ZestimateSurplus)==T] = 0
#DATA$ZestimateSurplusPct = DATA$ZestimateSurplus/DATA$Reserve




#Loop regressions over each state in the data set. Note the lack of interaction terms
#Institute floor for observations of 50

States = levels(DATA$PropertyState)
a = 1:length(States)
for( i in 1:length(a)){
  a[i] = nrow(DATA[DATA$PropertyState == as.character(States[i]),])
}
States = as.data.frame(cbind(States,a))
names(States) = c("PropertyState","Count")
States = States[as.numeric(as.character(States$Count)) > 50,]
States = as.character(States$PropertyState)

#Create Placeholders for results
Outputlist = numeric(0)
BPOlist = numeric(0)
HBlist = numeric(0)
ADlist = numeric(0)
PPlist = numeric(0)
CPlist = numeric(0)
FullOutputlist = numeric(0)

#Press the button!
for(j in 8:length(States)){
  
  #Subset for looping purposes
  TRUST = DATA[DATA$PropertyState == States[j],]
  
  BPO = DATA[is.na(DATA$BPO) == F
             & as.character(DATA$PropertyState)== States[j]
             ,c("PropertyState",
                "SFR_Flag",
                "MultiResidence_Flag",
                "BPO",
                "OpeningBid",
                "Bedrooms",
                "Baths",
                "Age",
                "LotSize",
                "HomeSquareFootage",
                "Vacancy",
                "MedianPPSQFT",
                "Condition_fair",
                "Condition_average",
                "Condition_good",
                "Condition_poor",
                "Condition_unknown"
             )]
  
  #Introduce an "Unknown" County with median value for all other inputs
  BPO[,2:(ncol(BPO)-5)] = apply(BPO[,2:(ncol(BPO)-5)],2,as.character)
  BPO[,2:(ncol(BPO)-5)] = apply(BPO[,2:(ncol(BPO)-5)],2,as.numeric)
  UkC = apply(BPO[,2:(ncol(BPO)-5)], 2, median, na.rm = T)
  UkC = c(States[j],UkC,0,0,0,0,1)
  BPO = rbind(BPO,UkC)
  #Re-Numericize the Condition Dummies
  BPO[,2:(ncol(BPO))] = apply(BPO[,2:(ncol(BPO))],2,as.character)
  BPO[,2:(ncol(BPO))] = apply(BPO[,2:(ncol(BPO))],2,as.numeric)
  
  #Construct BPO Estimator
  #Fix Missing Data
  BPO$Bedrooms[BPO$Bedrooms < 0] = median(BPO$Bedrooms[BPO$Bedrooms > 0],na.rm=T)
  BPO$Baths[BPO$Baths < 0] = median(BPO$Baths[BPO$Baths > 0],na.rm=T)
  BPO$Age[BPO$Age >= 200] = median(BPO$Age[BPO$Age < 200],na.rm=T)
  BPO$LotSize[is.na(BPO$LotSize)==T]=0.1
  BPO$HomeSquareFootage[BPO$HomeSquareFootage < 100] = mean(BPO$HomeSquareFootage[BPO$HomeSquareFootage >=100],na.rm=T)
  
  
  #Create Valuation Model, note the interaction terms.
  lm.BPO = lm(BPO ~ 
                HomeSquareFootage*Condition_good + 
                HomeSquareFootage*Condition_average + 
                HomeSquareFootage*Condition_fair + 
                HomeSquareFootage*Condition_poor + 
                HomeSquareFootage*Condition_unknown + 
                Bedrooms*Baths + 
                Age + 
                MedianPPSQFT:HomeSquareFootage +
                SFR_Flag +
                MultiResidence_Flag, data = BPO)
  
  BPOcoef = cbind(States[j], coef(lm.BPO))
  BPOlist = rbind(BPOlist, BPOcoef)
  
  #For Debugging, use this to tell where you are in the loop
  Step = 'BPO Model Created'
  
  #Stabilize the patient -- Sanitize the variables
  #Fix Numerics
  TRUST[,23:ncol(TRUST)] = apply(TRUST[,23:ncol(TRUST)],2,as.character)
  TRUST[,23:ncol(TRUST)] = apply(TRUST[,23:ncol(TRUST)],2,as.numeric)
  #Fix History
  TRUST$Age[TRUST$Age > 200] = median(TRUST$Age[TRUST$Age < 200])
  TRUST$LotSize[is.na(TRUST$LotSize)==T] = .1
  TRUST$Bedrooms[TRUST$Bedrooms < 0 | is.na(TRUST$Bedrooms)] = median(TRUST$Bedrooms[TRUST$Bedrooms > 0],na.rm=T)
  TRUST$Baths[TRUST$Baths < 0] = median(TRUST$Baths[TRUST$Baths > 0])
  TRUST$HomeSquareFootage = as.numeric(as.character(TRUST$HomeSquareFootage))
  TRUST$HomeSquareFootage[TRUST$HomeSquareFootage < 100 | is.na(TRUST$HomeSquareFootage)==T] = mean(TRUST$HomeSquareFootage[TRUST$HomeSquareFootage >=100])
  TRUST$Favorites = ceiling(TRUST$Favorites / TRUST$RunNum)
  #Fix BPO
  TRUST$BPO = as.numeric(as.character(TRUST$BPO))
  
  #Fix USPS
  TRUST$TotalActive = as.numeric(as.character(TRUST$TotalActive))
  TRUST$Vacancy = as.numeric(as.character(TRUST$Vacancy))
  TRUST$TotalActive[is.na(TRUST$TotalActive)==T]=median(TRUST$TotalActive,na.rm=T)
  TRUST$Vacancy[is.na(TRUST$Vacancy)==T]=median(TRUST$Vacancy,na.rm=T)
  TRUST$TotalActive[is.na(TRUST$TotalActive)==T]=median(DATA$TotalActive,na.rm=T)
  TRUST$Vacancy[is.na(TRUST$Vacancy)==T]=median(DATA$Vacancy,na.rm=T)
  
  #We use AuctionDate to keep track of test and training groups.
  #Affix an extimated BPO to the test group where lacking, using the unknown county as necessary.
  BPO_Data = TRUST[is.na(TRUST$BPO)==T & TRUST$AuctionDate >= ymd(date),]  
  BPO.hat = predict(object = lm.BPO, newdata = BPO_Data, type= "response")
  
  #Fix a prediction when it's outside logical bounds.
  #Fix when BPO is below starting bid or when it is above maximum legitimate BPO
  if(any(BPO.hat <= 0 | is.na(BPO.hat)==T)){
    BPO.hat[BPO.hat <= 0 | is.na(BPO.hat)==T] = ifelse(is.na(BPO_Data$OpeningBid[BPO.hat <= 0 | is.na(BPO.hat)==T])==T,
                                                       0,
                                                       BPO_Data$OpeningBid[BPO.hat <= 0 | is.na(BPO.hat)==T])
  }
  if(any(BPO.hat >= 3*BPO_Data$Reserve)){
    BPO.hat[BPO.hat >= 3*BPO_Data$Reserve] = 3*BPO_Data$Reserve[BPO.hat >= 3*BPO_Data$Reserve]
  }
  
  #when a BPO is missing, give it the median BPO of assets in its reserve X-cile
  #This way, we can draw our BPO Estimates back into normal territory
  #Calculating the appropriate replacement value should be done outside the loop
  BPOMissing = which(is.na(TRUST$BPO)==T & TRUST$AuctionDate >= ymd(date))
  if(length(BPOMissing)>0){
    x = .2
    #A is BPOs, B is Reserves
    A = quantile(TRUST$BPO, probs = seq(0,1,x), na.rm=T)
    B = quantile(TRUST$Reserve, probs = seq(0,1,x), na.rm=T)
    ImputedBPO = 1:(length(B)-1)
    for(k in 1:(length(ImputedBPO))){
      LowerIndex = k
      UpperIndex = k+1
      LB = as.numeric(B[LowerIndex])
      UB = as.numeric(B[UpperIndex])
      ID = which(TRUST$Reserve >= LB & TRUST$Reserve <= UB)
      ImputedBPO[k] = min(TRUST$BPO[TRUST$Reserve >= LB & TRUST$Reserve <= UB],na.rm=T)
    }
    #Bias toward the High End
    ImputedBPO[length(ImputedBPO)+1] = max(TRUST$BPO,na.rm=T)
    
    #Find the rank of the property inside the Reserve range
    Rank = rep(0,length(BPOMissing))
    Index = rep(0,length(BPOMissing))
    
    for(k in 1:length(BPOMissing)){
      C = B
      D = TRUST$Reserve[BPOMissing[k]]
      if(is.na(D)==T) D = median(TRUST$Reserve,na.rm=T)
      C[length(C)+1] = D
      C = C[order(C)]
      Index[k] = median(which(C==D))
      Rank[k]=ImputedBPO[Index[k]]
    }
    
    #Mix it back in
    MissingBPOs = (4*BPO.hat + 1*Rank) / 5
    
    
    
    #When Fix a prediction when it's outside logical bounds
    if(any(MissingBPOs >= 3*TRUST$Reserve[BPOMissing])){
      MissingBPOs[MissingBPOs >= 3*TRUST$Reserve[BPOMissing]] = 3*TRUST$Reserve[MissingBPOs >= 3*TRUST$Reserve[BPOMissing]]
    }
    TRUST$BPO[BPOMissing] = MissingBPOs
    TRUST$BPOSurplus = TRUST$BPO - TRUST$Reserve
    TRUST$BPOSurplusPct = TRUST$BPOSurplus / TRUST$Reserve
  }  
  Step = 'BPO Values Imputed'
  
  #Now with BPOs, fix missing obs
  TRUST$MedianPPSQFTDeviation = as.numeric(as.character(TRUST$MedianPPSQFT)) - (as.numeric(as.character(TRUST$Reserve)) / as.numeric(as.character(TRUST$HomeSquareFootage)))
  
  Step = 'Data Sanitized'
  
  
  
  
  
  
  
  
  #*****************
  
  
  
  
  
  
  
  #Must have all variables present in order to predict.
  TEST = {TRUST[TRUST$AuctionDate >= ymd(date)
             & TRUST$IsAuction == 1
             & is.na(TRUST$Reserve)==F
             & is.na(TRUST$C2C_Flag)==F
             & is.na(TRUST$REO_Flag)==F
             & is.na(TRUST$ShortSale_Flag)==F
             & is.na(TRUST$Trustee_Flag)==F
             & is.na(TRUST$Condition_poor)==F
             & is.na(TRUST$Condition_fair)==F
             & is.na(TRUST$Condition_average)==F
             & is.na(TRUST$Condition_good)==F
             & is.na(TRUST$Condition_unknown)==F
             & is.na(TRUST$PropertyOccupancyStatus_Occupied)==F
             & is.na(TRUST$PropertyOccupancyStatus_Unknown)==F
             & is.na(TRUST$PropertyOccupancyStatus_Vacant)==F
             & is.na(TRUST$ValueLevel_Low)==F
             & is.na(TRUST$ValueLevel_Med)==F
             & is.na(TRUST$ValueLevel_High)==F
             & is.na(TRUST$RunNum)==F
             & is.na(TRUST$TotalActive)==F
             & is.na(TRUST$Vacancy)==F
             & is.na(TRUST$HasPrevAttempts)==F
             & is.na(TRUST$BPOSurplusPct)==F
             & is.na(TRUST$Appreciation)==F
             & is.na(TRUST$Turnover)==F
             & is.na(TRUST$MedianPPSQFT)==F
             & is.na(TRUST$MedianPPSQFTDeviation)==F,]}
  
  
  #Must have all values to construct a model
  ReservePredictor = {TRUST[TRUST$AuctionDate < ymd(date) 
                           #& TRUST$IsAuction == 1
                           & is.na(TRUST$Reserve)==F
                           & is.na(TRUST$C2C_Flag)==F
                           & is.na(TRUST$REO_Flag)==F
                           & is.na(TRUST$ShortSale_Flag)==F
                           & is.na(TRUST$Trustee_Flag)==F
                           & is.na(TRUST$Condition_poor)==F
                           & is.na(TRUST$Condition_fair)==F
                           & is.na(TRUST$Condition_average)==F
                           & is.na(TRUST$Condition_good)==F
                           & is.na(TRUST$Condition_unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Occupied)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Vacant)==F
                           & is.na(TRUST$ValueLevel_Low)==F
                           & is.na(TRUST$ValueLevel_Med)==F
                           & is.na(TRUST$ValueLevel_High)==F
                           & is.na(TRUST$RunNum)==F
                           & is.na(TRUST$TotalActive)==F
                           & is.na(TRUST$Vacancy)==F
                           & is.na(TRUST$HasPrevAttempts)==F
                           & is.na(TRUST$BPOSurplusPct)==F
                           & is.na(TRUST$Appreciation)==F
                           & is.na(TRUST$Turnover)==F
                           & is.na(TRUST$MedianPPSQFT)==F
                           & is.na(TRUST$MedianPPSQFTDeviation)==F,]}
  
  CancelPredictor = {TRUST[TRUST$AuctionDate < ymd(date) 
                           & is.na(TRUST$Reserve)==F
                           & is.na(TRUST$C2C_Flag)==F
                           & is.na(TRUST$REO_Flag)==F
                           & is.na(TRUST$ShortSale_Flag)==F
                           & is.na(TRUST$Trustee_Flag)==F
                           & is.na(TRUST$Condition_poor)==F
                           & is.na(TRUST$Condition_fair)==F
                           & is.na(TRUST$Condition_average)==F
                           & is.na(TRUST$Condition_good)==F
                           & is.na(TRUST$Condition_unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Occupied)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Vacant)==F
                           & is.na(TRUST$ValueLevel_Low)==F
                           & is.na(TRUST$ValueLevel_Med)==F
                           & is.na(TRUST$ValueLevel_High)==F
                           & is.na(TRUST$RunNum)==F
                           & is.na(TRUST$TotalActive)==F
                           & is.na(TRUST$Vacancy)==F
                           & is.na(TRUST$HasPrevAttempts)==F
                           & is.na(TRUST$BPOSurplusPct)==F
                           & is.na(TRUST$Appreciation)==F
                           & is.na(TRUST$Turnover)==F
                           & is.na(TRUST$MedianPPSQFT)==F
                           & is.na(TRUST$MedianPPSQFTDeviation)==F,]}
  
  HighBidPredictor = {TRUST[TRUST$AuctionDate < ymd(date)
                           & TRUST$IsAuction == 1
                           & is.na(TRUST$Reserve)==F
                           & is.na(TRUST$C2C_Flag)==F
                           & is.na(TRUST$REO_Flag)==F
                           & is.na(TRUST$ShortSale_Flag)==F
                           & is.na(TRUST$Trustee_Flag)==F
                           & is.na(TRUST$Condition_poor)==F
                           & is.na(TRUST$Condition_fair)==F
                           & is.na(TRUST$Condition_average)==F
                           & is.na(TRUST$Condition_good)==F
                           & is.na(TRUST$Condition_unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Occupied)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Unknown)==F
                           & is.na(TRUST$PropertyOccupancyStatus_Vacant)==F
                           & is.na(TRUST$ValueLevel_Low)==F
                           & is.na(TRUST$ValueLevel_Med)==F
                           & is.na(TRUST$ValueLevel_High)==F
                           & is.na(TRUST$RunNum)==F
                           & is.na(TRUST$TotalActive)==F
                           & is.na(TRUST$Vacancy)==F
                           & is.na(TRUST$HasPrevAttempts)==F
                           & is.na(TRUST$BPOSurplusPct)==F
                           & is.na(TRUST$Appreciation)==F
                           & is.na(TRUST$Turnover)==F
                           & is.na(TRUST$MedianPPSQFT)==F
                           & is.na(TRUST$MedianPPSQFTDeviation)==F
                           & is.na(TRUST$HighBid)==F
                           ,]}


  Step = 'Test and Training Sets Built'
  
  #Build A regular LASSO Regression for Each Value Level
  #attempt to find good penalties, then fit, then get SEs, then predict, then append results:
  
  #Create Formulas to Tranform Data and shape predictions
  ThirdPartySaleFormula = {as.formula(IsSold
                                      ~SFR_Flag
                                      +MultiResidence_Flag
                                      +C2C_Flag
                                      +REO_Flag
                                      +ShortSale_Flag
                                      +Trustee_Flag
                                      +RunNum
                                      +HasPrevAttempts
                                      +Reserve 
                                      +BPOSurplus
                                      +BPOSurplusPct
                                      +Condition_poor
                                      +Condition_fair
                                      +Condition_average
                                      +Condition_good
                                      +Condition_unknown
                                      +PropertyOccupancyStatus_Occupied
                                      +PropertyOccupancyStatus_Unknown
                                      +PropertyOccupancyStatus_Vacant
                                      +TotalActive
                                      +Vacancy
                                      +ValueLevel_Low
                                      +ValueLevel_Med
                                      +ValueLevel_High
                                      +Favorites
                                      +Appreciation
                                      +Turnover
                                      +MedianPPSQFT
                                      +MedianPPSQFTDeviation
                                      +ForeclosedPer10000
                                      +PrevForeclosureSoldRate
                                      +PercentHomesSold
                                      +ADC_Historical_Execution
                                      +SellerPortfolioQuality
                                      +Cancellations
                                      +Postponements
  )}
  
  PostponeFormula = {as.formula(IsPostponed
                                ~SFR_Flag
                                +MultiResidence_Flag
                                +C2C_Flag
                                +REO_Flag
                                +ShortSale_Flag
                                +Trustee_Flag
                                +RunNum
                                +HasPrevAttempts
                                +Reserve 
                                +BPOSurplus
                                +BPOSurplusPct
                                +Condition_poor
                                +Condition_fair
                                +Condition_average
                                +Condition_good
                                +Condition_unknown
                                +PropertyOccupancyStatus_Occupied
                                +PropertyOccupancyStatus_Unknown
                                +PropertyOccupancyStatus_Vacant
                                +TotalActive
                                +Vacancy
                                +ValueLevel_Low
                                +ValueLevel_Med
                                +ValueLevel_High
                                +Favorites
                                +Appreciation
                                +Turnover
                                +MedianPPSQFT
                                +MedianPPSQFTDeviation
                                +ForeclosedPer10000
                                +PrevForeclosureSoldRate
                                +PercentHomesSold
                                +ADC_Historical_Execution
                                +SellerPortfolioQuality
                                +Cancellations
                                +Postponements
  )}
  
  CancelFormula = {as.formula(IsCancelled
                               ~SFR_Flag
                               +MultiResidence_Flag
                               +C2C_Flag
                               +REO_Flag
                               +ShortSale_Flag
                               +Trustee_Flag
                               +RunNum
                               +HasPrevAttempts
                               +Reserve 
                               +BPOSurplus
                               +BPOSurplusPct
                               +Condition_poor
                               +Condition_fair
                               +Condition_average
                               +Condition_good
                               +Condition_unknown
                               +PropertyOccupancyStatus_Occupied
                               +PropertyOccupancyStatus_Unknown
                               +PropertyOccupancyStatus_Vacant
                               +TotalActive
                               +Vacancy
                               +ValueLevel_Low
                               +ValueLevel_Med
                               +ValueLevel_High
                               +Favorites
                               +Appreciation
                               +Turnover
                               +MedianPPSQFT
                               +MedianPPSQFTDeviation
                               +ForeclosedPer10000
                               +PrevForeclosureSoldRate
                               +PercentHomesSold
                               +ADC_Historical_Execution
                               +SellerPortfolioQuality
                               +Cancellations
                               +Postponements
  )}
  
  HighBidFormula = {as.formula(HighBid
                               ~SFR_Flag
                               +MultiResidence_Flag
                               +C2C_Flag
                               +REO_Flag
                               +ShortSale_Flag
                               +Trustee_Flag
                               +RunNum
                               +HasPrevAttempts
                               +BPO 
                               +BPOSurplus
                               +BPOSurplusPct
                               +Condition_poor
                               +Condition_fair
                               +Condition_average
                               +Condition_good
                               +Condition_unknown
                               +PropertyOccupancyStatus_Occupied
                               +PropertyOccupancyStatus_Unknown
                               +PropertyOccupancyStatus_Vacant
                               +TotalActive
                               +Vacancy
                               +ValueLevel_Low
                               +ValueLevel_Med
                               +ValueLevel_High
                               +Favorites
                               +Appreciation
                               +Turnover
                               +MedianPPSQFT
                               +MedianPPSQFTDeviation
                               +ForeclosedPer10000
                               +PrevForeclosureSoldRate
                               +PercentHomesSold
                               +ADC_Historical_Execution
                               +SellerPortfolioQuality
                               +Cancellations
                               +Postponements
  )}
  
  #Run Predictions start with Third Party Sale
  x = ReservePredictor
  y = x$IsSold
  x = model.matrix(object = ThirdPartySaleFormula, data = x)
  AD = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cvmr = cv.glmnet(x, y)  
  Step = 'Auction Day Sale Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = ThirdPartySaleFormula, predx)
  ThirdPartySalePrediction = predict(AD, newx = predx, s = cvmr$lambda.min, type = "response") 
  Step = 'Auction Day Sale Prediction Run on Test Data'
  
  
  
  #Run Predictions with Postponed
  if(all(CancelPredictor$IsPostponed == 0)==F & all(CancelPredictor$IsPostponed == 1) ==F){
  x = CancelPredictor
  y = x$IsPostponed
  x = model.matrix(object = PostponeFormula, data = x)
  PP = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cvpp = cv.glmnet(x, y)
  Step = 'Postponement Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = PostponeFormula, predx)
  PostponePrediction = predict(PP, newx = predx, s = cvpp$lambda.min, type = "response")
  Step = 'Postponement Prediction Run on Test Data'
} else {    x = CancelPredictor
            y = x$IsPostponed
            if(all(y == 0) == T) {y[which.max(x$Postponements)] = 1
            } else {y[which.min(x$Postponements)] = 0}
            x = model.matrix(object = PostponeFormula, data = x)
            PP = glmnet(x, y, family = "binomial", alpha = 0.00001)
            cvpp = cv.glmnet(x, y)
            Step = 'Alt Postponement Prediction Run on Training Data'
            
            predx = TEST
            predx$HighBid[is.na(predx$HighBid)==T]  = 0
            predx = model.matrix(object = PostponeFormula, predx)
            PostponePrediction = predict(PP, newx = predx, s = cvpp$lambda.min, type = "response")
            Step = 'Alt Postponement Prediction Run on Test Data' } 
  
  
  #Run Predictions  with Cancellation
  if(all(CancelPredictor$IsCancelled ==0) == F & all(CancelPredictor$IsCancelled == 1) ==F){  
  x = CancelPredictor
  y = x$IsCancelled
  x = model.matrix(object = CancelFormula, data = x)
  CP = glmnet(x, y, family = "binomial", alpha = 0.00001)
  cvcp = cv.glmnet(x, y)
  Step = 'Cancellation Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = CancelFormula, predx)
  CancelPrediction = predict(CP, newx = predx, s = cvcp$lambda.min, type = "response")
  Step = 'Cancellation Prediction Run on Test Data'
  } else {    x = CancelPredictor
              y = x$IsCancelled
              if(all(y == 0) == T) {y[which.max(x$Cancellations)] = 1
              } else {y[which.min(x$Cancellations)] = 0}
              x = model.matrix(object = CancelFormula, data = x)
              CP = glmnet(x, y, family = "binomial", alpha = 0.00001)
              cvcp = cv.glmnet(x, y)
              Step = 'Alt Cancellation Prediction Run on Training Data'
              
              predx = TEST
              predx$HighBid[is.na(predx$HighBid)==T]  = 0
              predx = model.matrix(object = CancelFormula, predx)
              PostponePrediction = predict(CP, newx = predx, s = cvcp$lambda.min, type = "response")
              Step = 'Alt Cancellation Prediction Run on Test Data' }  
  
  
  #Continue with High Bid Prediction 
  x = HighBidPredictor
  y = x$HighBid
  x = model.matrix(object = HighBidFormula, data = x)
  HB = glmnet(x, y, family = "gaussian", alpha = 0.005, standardize = F)
  cvhb = cv.glmnet(x, y, lambda = HB$lambda)
  Step = 'High Bid Prediction Run on Training Data'
  
  predx = TEST
  predx$HighBid[is.na(predx$HighBid)==T]  = 0
  predx = model.matrix(object = HighBidFormula, predx)
  HighBidPrediction = predict(HB, newx = predx, s = HB$lambda[length(HB$lambda)], type = "response") 
  Step = 'High Bid Prediction Run on Test Data'
  
  
  
  
  
  
  
  
  
  
  #******************
  #PropertyFault Suggestor
  
  
  
  
  
  
  
  
  
  
  
  #PropertyFault Suggestor
  
  #Gather the model coefficients
  LAD1 = cbind(States[j], "AuctionDaySale", as.vector(coef(AD, s = cvmr$lambda.min))[-2])
  LPP1 = cbind(States[j], "Postponement", as.vector(coef(PP, s = cvpp$lambda.min))[-2])
  LCP1 = cbind(States[j], "Cancellation", as.vector(coef(CP, s = cvcp$lambda.min))[-2])
  #Remove the model.matrix intercept and insert a zero via append to make sure the models are the same
  LHB1 = cbind(States[j], "HighBid", as.vector(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2])
  #Name the rows to keep things ordered
  rownames(LAD1) = rownames(coef(AD, s = cvmr$lambda.min))[-2]
  rownames(LPP1) = rownames(coef(PP, s = cvpp$lambda.min))[-2]
  rownames(LCP1) = rownames(coef(CP, s = cvcp$lambda.min))[-2]
  rownames(LHB1) = rownames(coef(HB, s = HB$lambda[length(HB$lambda)]))[-2]
  #Bind the list to save for export
  ADlist = rbind(ADlist,LAD1)
  PPlist = rbind(PPlist,LPP1)
  CPlist = rbind(CPlist,LCP1)
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
    4,#TRUST
    5,#ShortSale
    6,#Trustee
    9 #Reserve / #BPO
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
         "C2C Product Line Performs Poorly in This State",                      #3
         "REO Product Line Performs Poorly in This State",                      #4
         "Short Sale Product Line Performs Poorly in This State",               #5
         "Trustee Product Line Performs Poorly in This State",                  #6
         "Property Has Many Attempts at Auction",                               #7
         "Property Has Any Previous Attempts at Auction",                       #8
         "Reserve out of Line with Area / BPO Too Low",                         #9
         "Low Dollar Valuation Against Purchase Price",                         #10
         "Low Appraised Value as Share of Purchase Price",                      #11
         "Condition Suggests Expensive Refurbishment",                          #12          
         "Condition Suggests Expensive Rehab",                                  #13
         "Condition Suggests Low Investment Potential",                         #14
         "Condition Suggests Low Investment Potential",                         #15
         "Condition Information Unknown",                                       #16
         "Occupied Homes in This Region Associated with Low Performance",       #17
         "Occupancy Status Unknown",                                            #18
         "Vacant Homes in This Region Associated with Low Performance",         #19
         "Low Population in ZIP",                                               #21
         "High Vacancy in ZIP",                                                 #22
         "Low Value Property Suggests Limited ROI",                             #23
         "Medium Value Property Suggests Limited ROI",                          #24
         "High Value Property Suggests Little Room for Added Value",            #25                       
         "Web Traffic Pattern Indicates Interest from Bargain Buyers",          #26
         "Price Movement in ZIP Suggests Poor Returns on Investment",           #27
         "Low Turnover in ZIP Suggests Difficult Resale",                       #28
         "Low Value Area",                                                      #29
         "Property Expensive per SQFT Versus ZIP",                              #30
         #"Reserve High Versus Zillow Zestimate",
         "ZIP has high incidence of foreclosure",                               #31
         "Statistically Few Foreclosures are resold in ZIP",                    #32
         "Few Homes Sold in Retail Market for this Region",                     #32
         #"Overall Event Size Suggests Asset Will Be Overlooked",               
         #"Number of Assets in Event From This State",
         "ZIP has low percentage sales rate ",                                  #33
         "ADC Has Historically Poor Execution in this ZIP",                     #34
         "Seller Portfolio Historically Low-Quality Assets",                    #35
         "Asset's Cancellations Imply Low Performance",                         #36
         "Asset's Postponements Imply Low Performance",                         #37
         #Insert New Faults above this comment                  
         "Cannot Suggest Fault")                                                #38
  
  #E1 and E2 Index the worst and second worst problem faced by the property. Return the Name
  Neg1 = A1[E1]
  Neg2 = A1[E2]
  
  Step = "Property Faults Described"
  
  
  
  

  
  
  
  
  #*******
  
  
  
  
 
  
  
  
  TEST$ValueLevel = "High"
  TEST$ValueLevel[TEST$ValueLevel_Med == 1] = "Med"
  TEST$ValueLevel[TEST$ValueLevel_Low == 1] = "Low"
  TEST$PredictionDate = rundate
  
  
  
  Output = TEST[,c("REDCID","LoanNbr","ValueLevel","ProductType","PropertyType",
                   "AuctionCode","GlobalPropertyId","AuctionId","VenueId","AuctionDate","PredictionDate","TrusteeCode",
                   "TrusteeName","PropertyCity","PropertyCounty","PropertyState","PropertyZip","MSAName",
                   "PropertyOccupancyStatus","Condition","Bedrooms",
                   "Baths","LotSize","HomeSquareFootage","RunNum","BPO","OpeningBid","Reserve","HighBid",
                   "BPOSurplus","Appreciation","Turnover","Zestimate","Favorites",
                   "IsPredictedBPO","IsSold","WinningBid")]
  Output$Neg1 = Neg1
  Output$Neg2 = Neg2
  
  
  #Create Housing Quality Grade
  Output$ThirdPartySalePrediction = ThirdPartySalePrediction
  Output$CancelPrediction = CancelPrediction
  Output$PostponePrediction = PostponePrediction
  Output$HighBidPrediction = HighBidPrediction[,1]

  
  #Sometimes, assets have a perfect storm of positive characteristics that make them fall outside normal predictions.
  Output$HighBidPrediction[Output$HighBidPrediction > 1.5*Output$Reserve & Output$HighBidPrediction > 200000] = Output$Reserve[Output$HighBidPrediction > 1.5*Output$Reserve & Output$HighBidPrediction > 200000]*1.5
  
  #Create Suggested Reserve Reduction
  Output$ReserveRedux = ifelse(Output$ThirdPartySalePrediction < .5 & Output$HighBidPrediction < Output$Reserve, 
                               Output$HighBidPrediction - Output$Reserve, 0 )
  
  Outputlist = rbind(Outputlist, Output)
  
  
  
  
  #FullOutputlist
  FullOutput = TEST
  FullOutput$Neg1 = Neg1
  FullOutput$Neg2 = Neg2
  #Create Housing Quality Grade
  FullOutput$ThirdPartySalePrediction = ThirdPartySalePrediction
  FullOutput$CancelPrediction = CancelPrediction
  FullOutput$PostponePrediction = PostponePrediction
  FullOutput$HighBidPrediction = HighBidPrediction[,1] 
  #Sometimes, assets have a perfect storm of positive characteristics that make predict well above reserve.
  FullOutput$HighBidPrediction[FullOutput$HighBidPrediction > 1.5*FullOutput$Reserve & FullOutput$HighBidPrediction > 200000] = FullOutput$Reserve[FullOutput$HighBidPrediction > 1.5*FullOutput$Reserve & FullOutput$HighBidPrediction > 200000]*1.5
  
  #Create Suggested Reserve Reduction
  FullOutput$ReserveRedux = ifelse(FullOutput$ThirdPartySalePrediction < .5 & FullOutput$HighBidPrediction < FullOutput$Reserve, 
                                   FullOutput$HighBidPrediction - FullOutput$Reserve, 0 )
  FullOutputlist = rbind(FullOutputlist, FullOutput)

  }








LikelihoodLevels = c(0, .1, .5, .8, 1)
Outputlist$SaleLikelihood = "Low"
Outputlist$SaleLikelihood[Outputlist$ThirdPartySalePrediction >= LikelihoodLevels[2]] = "Borderline"
Outputlist$SaleLikelihood[Outputlist$ThirdPartySalePrediction >= LikelihoodLevels[3]] = "Moderate"
Outputlist$SaleLikelihood[Outputlist$ThirdPartySalePrediction >= LikelihoodLevels[4]] = "High"

#LikelihoodLevels = quantile(FullOutputlist$ThirdPartySalePrediction, probs = seq(0,1,.25))
LikelihoodLevels = c(0, .1, .5, .8, 1)
FullOutputlist$SaleLikelihood = "Low"
FullOutputlist$SaleLikelihood[FullOutputlist$ThirdPartySalePrediction >= LikelihoodLevels[2]] = "Borderline"
FullOutputlist$SaleLikelihood[FullOutputlist$ThirdPartySalePrediction >= LikelihoodLevels[3]] = "Moderate"
FullOutputlist$SaleLikelihood[FullOutputlist$ThirdPartySalePrediction >= LikelihoodLevels[4]] = "High"


#Do some logical fixing
#Make minimum bid the starting bid
Outputlist$OpeningBid[is.na(Outputlist$OpeningBid)==T] = min(Outputlist$OpeningBid,na.rm=T)
Outputlist$HighBidPrediction[Outputlist$HighBidPrediction < Outputlist$OpeningBid] = 
  Outputlist$OpeningBid[Outputlist$HighBidPrediction < Outputlist$OpeningBid]
FullOutputlist$OpeningBid[is.na(FullOutputlist$OpeningBid)==T] = min(FullOutputlist$OpeningBid,na.rm=T)
FullOutputlist$HighBidPrediction[FullOutputlist$HighBidPrediction < FullOutputlist$OpeningBid] = 
  FullOutputlist$OpeningBid[FullOutputlist$HighBidPrediction < FullOutputlist$OpeningBid]


#And now to write the results to a file
filename = paste("TRUSTEE_GLMPredictionOutput_", rundate, ".csv", sep = "")
write.csv(Outputlist, filename, row.names = F, quote = F)
filename = paste("TRUSTEE_GLMADCoefficientOutput_", rundate, ".csv", sep = "")
write.csv(ADlist, filename, row.names = T, quote = F)
filename = paste("TRUSTEE_GLMCPCoefficientOutput_", rundate, ".csv", sep = "")
write.csv(CPlist, filename, row.names = T, quote = F)
filename = paste("TRUSTEE_GLMPPCoefficientOutput_", rundate, ".csv", sep = "")
write.csv(PPlist, filename, row.names = T, quote = F)
filename = paste("TRUSTEE_GLMHBCoefficientOutput_", rundate, ".csv", sep = "")
write.csv(HBlist, filename, row.names = T, quote = F)
filename = paste("TRUSTEE_GLMPredictionFullOutput_", rundate, ".csv", sep = "")
write.csv(FullOutputlist, filename, row.names = F, quote = F)


filename = paste("TRUSTEE_PredictionOutput_WeeklyUpdate_", rundate, ".csv", sep = "")
write.csv(FullOutputlist, filename, row.names = F, quote = F)


filename = "TRUSTEE_BPOEstimationCoefficients.csv"
write.csv(BPOlist, filename, row.names = T, quote = F)





if( 1 == 0){



#Prediction Analytics


hist(FullOutputlist$ThirdPartySalePrediction, xlab= "Sale Likelihood Prediction", main= "Frequency of Per-Attempt Chance to Sell Predictions")
hist(FullOutputlist$ThirdPartySalePrediction[FullOutputlist$AuctionDate > ymd(rundate)]
     , xlab= "Sale Likelihood Prediction", main= "Upcoming 30 Day Sale Likelihood Predictions")
hist(FullOutputlist$ThirdPartySalePrediction[FullOutputlist$AuctionDate < ymd(rundate)]
     , xlab= "Sale Likelihood Prediction", main= "Trailing 30 Day Sale Likelihood Predictions")
mean(FullOutputlist$ThirdPartySalePrediction)*(mean(is.na(as.numeric(as.character(DATA$HighBid)))))
mean(FullOutputlist$ThirdPartySalePrediction)

hist(round(FullOutputlist$ThirdPartySalePrediction[FullOutputlist$AuctionDate > ymd(rundate)], digits=0) - 
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


hist(round(FullOutputlist$ThirdPartySalePrediction[FullOutputlist$AuctionDate < ymd(rundate) & FullOutputlist$IsAuction == 1], digits=0) -
       FullOutputlist$IsSold[FullOutputlist$AuctionDate < ymd(rundate) & FullOutputlist$IsAuction == 1],
     xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Validation Set")

hist(round(FullOutputlist$ThirdPartySalePrediction[FullOutputlist$AuctionDate < ymd(rundate) & FullOutputlist$IsAuction == 1], digits=0) -
       FullOutputlist$IsSold[FullOutputlist$AuctionDate < ymd(rundate) & FullOutputlist$IsAuction == 1],
     xlab = "Error Type ( 0 = Correct, 1 = False Positive, -1 = False Negative)", main = "Frequency of Errors in Validation Set, Auctioned Only")


#Have a look at unscored Assets

TEST1 = DATA[DATA$AuctionDate > ymd(rundate),]
t = as.data.frame(table(TEST1$AuctionCode))
t = t[t$Freq >0,]
t = as.data.frame(table(TEST1$SellerCode))
t = t[t$Freq >0,]
t = t[t$Var1 %in% c("BA1","BV1","BAJ","FNM","NSO","NST","NTS","NSV"),]

}