#####################
#
#####################

#---
# Install Necessary Resources
#---
source('Functions.R')  
requiredPackages <- c('ZillowR',
                      'readr',
                      'tidyverse',
                      'RSQLite',
                      'DBI',
                      'XML',
                      'lubridate',
                      'corrplot',
                      'stringr',
                      'car',
                      'olsrr',
                      'Hmisc',
                      'psych')
loadLibraries(requiredPackages)
rm(requiredPackages)

source('Functions.R')
source('zillowAPIcredentials.R')
# Remove source zillow credentials and insert your ZWSID below
# set_zillow_web_service_id(ZWSID)
#   rm(ZWSID)

#---
# Create a local database to store data
#---
mydb2 <- dbConnect(RSQLite::SQLite(),"my-db.sqlite")

#---
# Create list of data for median house price per month
#---
# Median Sales Price for New Houses Sold in the United States (MSPNHSUS)
# https://fred.stlouisfed.org/series/MSPNHSUS
medianHousePrice.df <- read_csv("MSPNHSUS.csv")
medianHousePrice.df$DATE <- as.character(medianHousePrice.df$DATE)
dbWriteTable(mydb2,"medianHousePrice",medianHousePrice.df)
# medianHousePrice.df <- dbGetQuery(mydb,"select * from medianHousePrice")
medianHousePrice.df$DATE <- ymd(medianHousePrice.df$DATE)

#---
# Download Zillow data from API
#---
  # Use 1420 Christy Ave, 40204 for the starting home
  if(length(dbGetQuery(mydb2,"SELECT * FROM comps")[1])>0 )
    comps <- comp_finder("1420 Christy Ave","40204")
    updatedDetails <- multipleUpdatedPropertyDetails(comps$zpid)
    
    dbWriteTable(mydb2,"comps",comps,append=FALSE)
    dbWriteTable(mydb2,"details",updatedDetails,append=FALSE)
  
#---
# Combine median house prices, zillow deep comps data and zillow update data, then clean results
#---
  zillow.data <- left_join(comps,updatedDetails) #by = all variables which have the same name
  
  zillow.data <- zillow.data %>% 
                  rowwise() %>% mutate(medianUSpriceAssessmentYear = getYearAvgFromMonthly(medianHousePrice.df,taxAssessmentYear)) %>%
                  rowwise() %>% mutate(medianUSpriceLastSoldDate = getMedianFromDate(medianHousePrice.df,mdy(lastSoldDate)))
  # median house price from the FED lags by a few months, so change those values to NA.
  zillow.data$medianUSpriceLastSoldDate[zillow.data$medianUSpriceLastSoldDate == 0] <- NA
  
  for(col in colnames(zillow.data)) {
    zillow.data[,col] <- unlist(zillow.data[,col])
  }
  
#---
# Cleanup data so it is usable
#---
  zillow.data <- zillow.data %>% select(zpid,
                                        street,
                                        zipcode,
                                        taxAssessmentYear,
                                        taxAssessment,
                                        lotSizeSqFt,
                                        finishedSqFt,
                                        bathrooms,
                                        bedrooms,
                                        totalRooms,
                                        lastSoldDate,
                                        lastSoldPrice,
                                        zest = amount,
                                        zestlow = low,
                                        zesthigh = high,
                                        percentile,
                                        zindexValue,
                                        numFloors,
                                        roof:coolingSystem,
                                        floorCovering,
                                        basement,
                                        coveredParkingSpaces,
                                        medianUSpriceAssessmentYear,
                                        medianUSpriceLastSoldDate)
  
  # zIndexValue can't be numeric while it has a comma
  zillow.data$zindexValue <- str_replace(zillow.data$zindexValue,",","")
  
  # Need to determine data type for each column
  categorical <- c("zipcode",
                   "roof",
                   "exteriorMaterial",
                   "parkingType",
                   "heatingSources",
                   "heatingSystem",
                   "coolingSystem",
                   "floorCovering",
                   "basement")
  for(col in categorical) {
    zillow.data[,col] <- unlist(lapply(zillow.data[,col],as.character))
  }
  rm(categorical)
  
  numeric <- c("taxAssessment",
               "lotSizeSqFt",
               "finishedSqFt",
               "bathrooms",       
               "bedrooms",
               "totalRooms",
               "numFloors",
               "lastSoldPrice",
               "zest",
               "zestlow",
               "zesthigh",
               "percentile",
               "zindexValue",
               "coveredParkingSpaces",
               "medianUSpriceAssessmentYear",
               "medianUSpriceLastSoldDate")
  for(col in numeric) {
    zillow.data[,col] <- unlist(lapply(zillow.data[,col],as.numeric))
  }
  rm(numeric)
  
  zillow.data <- zillow.data %>% rowwise %>% mutate(lastSoldDate = mdy(lastSoldDate))
  
  # NA values present problems for regression
  # Options are to leave it as is, remove the variable/column, remove the offending row, or to fill with alternative data

  
#---
# Review (and fix) assumptions regarding data
#---
  summary(zillow.data)
  
  pairs.panels(select(zillow.data,taxAssessmentYear:zest,numFloors:medianUSpriceLastSoldDate),col="red")
      
      # bathrooms, bedrooms, totalRooms, numFloors, coveredParkingSpaces are not normal, and not continuous ... conver to categorical
      categorical <- c("bathrooms",
                   "bedrooms",
                   "totalRooms",
                   "numFloors",
                   "coveredParkingSpaces")
      for(col in categorical) {
        zillow.data[,col] <- unlist(lapply(zillow.data[,col],as.character))
      }
      rm(categorical)
  
      # Some of the variables appear to have right skew...let's try log transformation
      zillow.data.adj <- zillow.data %>% mutate(logLotSizeSqFt = log(lotSizeSqFt),
                                                logFinishedSqFt = log(finishedSqFt),
                                                logLastSoldPrice = log(lastSoldPrice),
                                                logZest = log(zest))
      pairs.panels(select(zillow.data.adj,taxAssessmentYear:taxAssessment,logLotSizeSqFt:logFinishedSqFt,bathrooms:lastSoldDate,logLastSoldPrice:logZest,numFloors:medianUSpriceLastSoldDate),col="red")
      # This cleaned up finishedSqFt, lastSoldPrice, and zest, but there are some missing values on the high end of lotSizeSqFt to pay attention to.
      
#---
# Create training and test data
#---
  # Select columns to try to start
  zillow.data.adj <- zillow.data.adj %>% select(lastSoldPrice,zest,logLastSoldPrice,logZest,logLotSizeSqFt,logFinishedSqFt,
                                                bathrooms,bedrooms,totalRooms,numFloors,roof,exteriorMaterial,
                                                parkingType,heatingSources,heatingSystem,coolingSystem,
                                                floorCovering,basement,coveredParkingSpaces)
      
  set.seed(42)        
  zillow.train <- zillow.data.adj %>% sample_n(round(length(zillow.data.adj[[1]])*0.8))
  zillow.test <- anti_join(zillow.data.adj,zillow.train)
    
#---
# Create regression model
#---
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + 
                  bathrooms + bedrooms + totalRooms + numFloors + roof + exteriorMaterial + 
                  parkingType + heatingSources + heatingSystem + coolingSystem + 
                  floorCovering + basement,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm)
  
  # There are too many NAs to deal with...let's remove those variables
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + 
                    bathrooms + bedrooms + totalRooms + numFloors,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.866
  vif(zillow.lm) #produces an error
  alias(zillow.lm) #shows perfect colinearity between bathrooms and totalRooms and numFloors3
  # The columns causing problems also have a lot of NA's, so we may need to remove them.
  
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + 
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.562
  # removing too many variables dropped adj R squared too much
  
  # Now, review this data
  # plot(zillow.lm)
  outlierTest(zillow.lm)
  crPlots(zillow.lm)
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  
  # Found that 4, 44,58,82, 16 are extreme
  zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(4,44,58,82,16)),]
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + 
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.62
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  # Found that 5,45,54,74,89 are extreme
  zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(5,45,54,74,89)),]
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + 
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.65
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  # Check for colinearity
  vif(zillow.lm)
  # VIF shows that bathrooms is 7.28 which is large and may be a problem
  
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.648
  # pvalue for bedrooms is too high
  
  zillow.lm <- lm(logZest ~ logLotSizeSqFt + logFinishedSqFt,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.634

#---
# Evaluate the model
#---
  train.pred <- predict(zillow.lm, newdata = select(zillow.train,logLotSizeSqFt,logFinishedSqFt))
  test.pred <- predict(zillow.lm, newdata = select(zillow.test,logLotSizeSqFt,logFinishedSqFt))
  
  #corelation training data
    cor(zillow.train$logZest[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE],train.pred[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE])
    #0.802
  #RMSE training data
    sqrt(mean((exp(train.pred[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE]) - zillow.train$zest[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE])^2))
    #29,086
  #MAE training data
    mean(abs(exp(train.pred[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE])-zillow.train$zest[is.na(zillow.train$logZest)==FALSE & is.na(train.pred)==FALSE]))
    #22,802
    
  #corelation test data
    cor(zillow.test$logZest[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE],
        test.pred[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE])
    # 0.596
  #RMSE test data
    sqrt(mean((exp(test.pred[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE]) - 
                 zillow.test$zest[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE])^2))
    # 48,193
  #MAE test data
    mean(abs(exp(test.pred[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE]) -
               zillow.test$zest[is.na(zillow.test$logZest)==FALSE & is.na(test.pred)==FALSE]))
    # 36,654