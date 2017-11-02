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
                  rowwise() %>% mutate(medianUSpriceLastSoldDate = getMedianFromDate(medianHousePrice.df,mdy(lastSoldDate))) %>%
                  rowwise() %>% mutate(medianUSpriceThisYear = getMedianFromDate(medianHousePrice.df,ymd(medianHousePrice.df[[length(medianHousePrice.df$DATE),1]])))
  # median house price from the FED lags by a few months, so change those values to NA.
  zillow.data$medianUSpriceLastSoldDate[zillow.data$medianUSpriceLastSoldDate == 0] <- NA
  zillow.data$medianUSpriceAssessmentYear[zillow.data$medianUSpriceAssessmentYear == 0] <- NA
  
  zillow.data <- zillow.data %>% 
    rowwise() %>% mutate(adjLastSoldPrice = as.numeric(lastSoldPrice)*(as.numeric(medianUSpriceThisYear)/as.numeric(medianUSpriceLastSoldDate))) %>%
    rowwise() %>% mutate(adjTaxAssessment = as.numeric(taxAssessment)*(as.numeric(medianUSpriceThisYear)/as.numeric(medianUSpriceAssessmentYear)))   
  
  for(col in colnames(zillow.data)) {
    zillow.data[,col] <- unlist(zillow.data[,col])
  }
  
  # zillow.data <- read_csv("zillow.data.csv")
  
#---
# Cleanup data so it is usable
#---
  zillow.data <- zillow.data %>% select(zpid,
                                        street,
                                        zipcode,
                                        taxAssessmentYear,
                                        taxAssessment,
                                        yearBuilt,
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
                                        architecture,
                                        basement,
                                        coveredParkingSpaces,
                                        adjLastSoldPrice,
                                        adjTaxAssessment,
                                        medianUSpriceAssessmentYear,
                                        medianUSpriceLastSoldDate,
                                        medianUSpriceThisYear)
  
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
               "adjLastSoldPrice",
               "adjTaxAssessment")
  for(col in numeric) {
    zillow.data[,col] <- unlist(lapply(zillow.data[,col],as.numeric))
  }
  rm(numeric)
  
  zillow.data <- zillow.data %>% rowwise %>% mutate(lastSoldDate = mdy(lastSoldDate))
  
  str(zillow.data)
  # write_csv(zillow.data,"zillow.data.csv")
  
#---
# Review (and fix) assumptions regarding data
#---
  summary(zillow.data)
    # totalRooms doesn't make any sense. Some of them show house only have bedrooms and bathrooms, but kitchens are mandated by code...so some errors here.
  
  pairs.panels(select(zillow.data,taxAssessmentYear:zest,numFloors:coveredParkingSpaces),col="red")
      
      # bathrooms, bedrooms, totalRooms, numFloors, coveredParkingSpaces are not normal, and not continuous ... convert to categorical
      categorical <- c("bathrooms",
                   "bedrooms",
                   "totalRooms",
                   "numFloors",
                   "coveredParkingSpaces")
      for(col in categorical) {
        zillow.data[,col] <- unlist(lapply(zillow.data[,col],as.character))
      }
      rm(categorical)
  
        # lotSizeSqFt may partially correlate to taxAssessment
        # finishedSqFt may partially correlate to taxAssessment, possibly slightly correlate to lotSizeSqFt
        # bathrooms may partially correlate to finishedSqFt
        # bedrooms may partially correlate to bathrooms and finishedSqFt
        # totalRooms may partially correlate to bedrooms, bathrooms, finishedSqFt, and taxAssessment
        # lastSoldPrice may partially correlate to bedrooms, finishedSqFt, and does correlate to taxAssessment
        # zest definitely correlates to lastSoldPrice
      
      # Some of the variables appear to have right skew...let's try log transformation
      zillow.data.adj <- zillow.data %>% mutate(logTaxAssessment = log(taxAssessment),
                                                logLotSizeSqFt = log(lotSizeSqFt),
                                                logFinishedSqFt = log(finishedSqFt),
                                                logLastSoldPrice = log(lastSoldPrice),
                                                logZest = log(zest),
                                                logAdjLastSoldPrice = log(adjLastSoldPrice),
                                                logAdjTaxAssessment = log(adjTaxAssessment))
      pairs.panels(select(zillow.data.adj,taxAssessmentYear,logTaxAssessment,logLotSizeSqFt:logFinishedSqFt,bathrooms:lastSoldDate,logLastSoldPrice:logZest,numFloors:coveredParkingSpaces),col="red")
      # This cleaned up finishedSqFt, lastSoldPrice, and zest, but there are some missing values on the high end of lotSizeSqFt to pay attention to.
      # Also, taxAssessment went from slight right skew to slight left skew. may not be the best transformation.
      
      zillow.data.adj <- zillow.data.adj %>% mutate(SqrtTaxAssessment = sqrt(taxAssessment))
      pairs.panels(select(zillow.data.adj,taxAssessmentYear,SqrtTaxAssessment,logLotSizeSqFt:logFinishedSqFt,bathrooms:lastSoldDate,logLastSoldPrice:logZest,numFloors:coveredParkingSpaces),col="red")
      
      # There are many NA values. We can either remove the rows, remove the columns, remove the single datapoint only, or attempt to impute missing values
        # totalRooms must be at least bathrooms + bedrooms. In reality it must also have at least a kitchen. And may have a living room. Some chance of a dining room but not guaranteed.
        # Therefore, let's use the following to impute values: if totalRooms > bathrooms + bedrooms then no change, otherwise totalRooms = bathrooms + bedrooms + 2.
        imputeRooms <- function(bedrooms,bathrooms,totalRooms) {
          bedrooms <- as.numeric(bedrooms)
          bathrooms <- as.numeric(bathrooms)
          totalRooms <- as.numeric(totalRooms)
          if(is.na(totalRooms)) { totalRooms <- 0 }
          if(is.na(bedrooms)) { bedrooms <- 0 }
          if(is.na(bathrooms)) { bathrooms <- 0 }
          if(totalRooms < bathrooms + bedrooms + 2) {
            totalRooms <- bathrooms + bedrooms + 2
          }
          return(round(totalRooms))
        }
        zillow.data.adj <- zillow.data.adj %>% mutate(adjTotalRooms = imputeRooms(bedrooms,bathrooms,totalRooms))
        rm(imputeRooms)
        
        # numFloors is more likely to be high the greater the number or rooms.
        imputeNumFloors <- function(totalRooms,numFloors) {
          totalRooms <- as.numeric(totalRooms)
          numFloors <- as.numeric(numFloors)
          if(is.na(numFloors) == FALSE) {
            numFloors <- numFloors
          } else if(totalRooms < 6) {
            numFloors <- 1
          } else if(totalRooms < 9) {
            numFloors <- 2
          } else {
            numFloors <- 3
          }
          return(round(numFloors))
        }
        zillow.data.adj <- zillow.data.adj %>% mutate(adjNumFloors = imputeNumFloors(adjTotalRooms,numFloors))
        rm(imputeNumFloors)
#---
# Create training and test data
#---
  set.seed(42)        
  zillow.train <- zillow.data.adj %>% sample_n(round(length(zillow.data.adj[[1]])*0.8))
  zillow.test <- anti_join(zillow.data.adj,zillow.train,by='zpid')
    
#---
# Create regression model, this time assuming we want to calculate zest even if we don't have lastSoldPrice
#---
  summary(zillow.train)
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                  bathrooms + bedrooms + adjTotalRooms + adjNumFloors + roof + exteriorMaterial + 
                  parkingType + heatingSources + heatingSystem + coolingSystem + 
                  floorCovering + architecture + basement,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj r2 = NA
  
  # There are too many NAs to deal with...let's remove those variables
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bathrooms + bedrooms + adjTotalRooms + adjNumFloors,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.689
  # vif(zillow.lm) #colinearity high with adjTotalRooms and bedrooms
  
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bedrooms + bathrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj r2 = 0.696
  # vif(zillow.lm)
  
  # Now, review this data
  # plot(zillow.lm)
  outlierTest(zillow.lm)
  crPlots(zillow.lm)
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  
  # Found that 6,44,61,78,82 are extreme
  zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(6,44,61,78,82)),]
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.678
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  # Found that 46 are extreme
  zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(46)),]
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.675
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  
  # Found that 4,51 are extreme
  zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(4,51)),]
  zillow.lm <- lm(zest ~ SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.716
  cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
  plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
  plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
  # And now we have points that are almost within cook's distance
  
  # Check for colinearity
  vif(zillow.lm)
  # VIF shows that bathrooms are slightly high, but not too bad.

  summary(zillow.lm)
    # sqrttaxassessment has high pvalue
  
  zillow.lm <- lm(zest ~ logLotSizeSqFt + logFinishedSqFt + zindexValue +
                    bathrooms + bedrooms,
                  data=zillow.train,na.action = na.omit)
  summary(zillow.lm) #adj Rsquared = 0.715
    # bathrooms have high pvalue
  
#---
# Evaluate the model, without lastSoldPrice
#---
  train.pred <- predict(zillow.lm, newdata = select(zillow.train,logLotSizeSqFt,logFinishedSqFt,zindexValue,bathrooms,bedrooms))
  test.pred <- predict(zillow.lm, newdata = select(zillow.test,logLotSizeSqFt,logFinishedSqFt,zindexValue,bathrooms,bedrooms))
  
  #corelation training data
    cor(zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE],train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE])
    #0.873
  #RMSE training data
    sqrt(mean((train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE] - zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE])^2))
    #21769
  #MAE training data
    mean(abs(train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE]-zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE]))
    #17619
    
  #corelation test data
    cor(zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE],
        test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE])
    # .739
  #RMSE test data
    sqrt(mean((test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE] - 
                 zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE])^2))
    # 39263
  #MAE test data
    mean(abs(test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE] -
               zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE]))
    # 29913

    # let's see how this performed against the house we chose.
    zillow.train[33,]$zest
    train.pred[33]
    
#---
# Create regression model for zest, this time assuming we want to calculate zest using lastSoldPrice
#---
    #---
    # Create training and test data
    #---
    set.seed(42)        
    zillow.train <- zillow.data.adj %>% sample_n(round(length(zillow.data.adj[[1]])*0.8))
    zillow.test <- anti_join(zillow.data.adj,zillow.train,by='zpid')
    
    #Perform regression
    # There are too many NAs to deal with...let's remove those variables
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + bedrooms + adjTotalRooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.868
    vif(zillow.lm) #colinearity high with adjTotalRooms and bedrooms
    
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bedrooms + bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj r2 = 0.871
    vif(zillow.lm) #high on bedroom
    
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj r2 = 0.875
    vif(zillow.lm) # all under 5
    
    # Now, review this data
    # plot(zillow.lm)
    outlierTest(zillow.lm)
    crPlots(zillow.lm)
    cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
    plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
    plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
    
    
    # Found that 61,70,78 are extreme
    zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(61,70,78)),]
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.96
    cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
    plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
    plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
    
    # Found that 61,70,78 are extreme
    zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(61,70,78)),]
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.962
    cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
    plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
    plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
    
    # Found that 11,13,48,71,84 are extreme
    zillow.train <- zillow.train[-which(rownames(zillow.train) %in% c(11,13,48,71,84)),]
    zillow.lm <- lm(zest ~ lastSoldPrice + SqrtTaxAssessment + logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.975
    cooksCutoff <- 4/((nrow(zillow.train)-length(zillow.lm$coefficients)-2))
    plot(zillow.lm, which = 4, cook.levels = cooksCutoff, id.n=5)
    plot(zillow.lm, which = 5, cook.levels = cooksCutoff, id.n=5)
    # And now we have points that are almost within cook's distance
    
    # Check for colinearity
    vif(zillow.lm)
    # VIF shows all are less than 5...so not great but ok.
    
    summary(zillow.lm)
    # sqrttaxassessment has high pvalue
    
    zillow.lm <- lm(zest ~ lastSoldPrice+ logLotSizeSqFt + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.976
    # logLotSizeSqFt has high pvalue 
    
    zillow.lm <- lm(zest ~ lastSoldPrice + logFinishedSqFt + zindexValue +
                      bathrooms + adjNumFloors,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.976
    # adjuNumFloors and bathrooms has high p-value...
    
    zillow.lm <- lm(zest ~ lastSoldPrice + logFinishedSqFt + zindexValue,
                    data=zillow.train,na.action = na.omit)
    summary(zillow.lm) #adj Rsquared = 0.976
    
#---
# Evaluate the model, with lastSoldPrice
#---
    train.pred <- predict(zillow.lm, newdata = select(zillow.train,lastSoldPrice,logFinishedSqFt,zindexValue))
    test.pred <- predict(zillow.lm, newdata = select(zillow.test,lastSoldPrice,logFinishedSqFt,zindexValue))
    
    #corelation training data
    cor(zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE],train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE])
    # 0.987
    #RMSE training data
    sqrt(mean((train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE] - zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE])^2))
    # 9664
    #MAE training data
    mean(abs(train.pred[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE]-zillow.train$zest[is.na(zillow.train$zest)==FALSE & is.na(train.pred)==FALSE]))
    # 6714
    
    #corelation test data
    cor(zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE],
        test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE])
    # 0.963
    #RMSE test data
    sqrt(mean((test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE] - 
                 zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE])^2))
    # 15685
    #MAE test data
    mean(abs(test.pred[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE] -
               zillow.test$zest[is.na(zillow.test$zest)==FALSE & is.na(test.pred)==FALSE]))
    # 10746
    
    # let's see how this performed against the house we chose.
    zillow.train[33,]$zest
    train.pred[33]
    