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
                        'olsrr')
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
  mydb <- dbConnect(RSQLite::SQLite(),"my-db.sqlite")
  
    
#---
# Create a list of addresses from Jefferson County Public Data
#---
  # http://data.lojic.org/
  # http://data.lojic.org/datasets/jefferson-county-ky-address-points
  # http://data.lojic.org/datasets/jefferson-county-ky-zoning
  jcky.addresses <- createResidentialAddressList('Jefferson_County_KY_Zoning.csv',
                                                 'Jefferson_County_KY_Address_Points.csv')
  
  # Selecting a random list of 6k addresses. Given an API limit of 1k per day, this
  # should cover the needs of an analysis due in 6 days.
    jcky.tolookup <- jcky.addresses %>% sample_n(6000, replace=FALSE)
    dbWriteTable(mydb, "jckytolookup", jcky.tolookup)
    rm(jcky.addresses)
    
  # After the first time, can skip steps above and process from database.
  jcky.tolookup <- dbGetQuery(mydb,"Select * from jckytolookup")
  
#---
# Create list of data for median house price per month
#---
  # Median Sales Price for New Houses Sold in the United States (MSPNHSUS)
  # https://fred.stlouisfed.org/series/MSPNHSUS
  medianHousePrice.df <- read_csv("MSPNHSUS.csv")
    medianHousePrice.df$DATE <- as.character(medianHousePrice.df$DATE)
    dbWriteTable(mydb,"medianHousePrice",medianHousePrice.df)
  medianHousePrice.df <- dbGetQuery(mydb,"select * from medianHousePrice")
    medianHousePrice.df$DATE <- ymd(medianHousePrice.df$DATE)
  
#---
# Download Zillow data from API
#---
  # Get 500 addresses from list of addresses and do a deep search on zillow
  zillowSearch.df <- multipleDeepSearchZillow(jcky.tolookup,500:550)
    dbWriteTable(mydb, "zillowDeepSearch",zillowSearch.df,append = TRUE)
  zillowSearch.df <- dbGetQuery(mydb,"select * from zillowDeepSearch")
  
  # Unfortunately, the deep search doesn't include enough information.
  # Also, many of these randomly selected properties are not in the Updated data 
  # because they have not been sold recently enought to be in Zillow database.
  # However, most comps have been updated with full details, and comps have a bit
  # more information themselves...so next step is to get comps, we should be able
  # to get 25 per zpid and per API call...so let's do it.
  zillowComps.df <- multipleDeepCompsZillow(zillowSearch.df$zpid,25)
    # if amount is empty then there isn't a zillow estimate for the property
    zillowComps.df <- zillowComps.df %>% filter(is.na(amount)==FALSE)
    # It is possible these comps are duplicates. Need to fix that.
    zillowComps.df <- zillowComps.df[duplicated(zillowComps.df$zpid)==FALSE,]
    dbWriteTable(mydb, "zillowDeepComps",zillowComps.df,append=TRUE)
  zillowComps.df <- dbGetQuery(mydb,"select * from zillowDeepComps")
    
  # Finally, what we really want is the Updated data because it is quite rich. API calls
  # are limited so do not call more than 1000 of these.
  # zillowUpdates <- multipleUpdatedPropertyDetails(zillowComps.df$zpid[1:500])
  #   dbWriteTable(mydb, "zillowUpdatedDetails",zillowUpdates,append=TRUE)
#---
# Combine median house prices, zillow deep comps data and zillow update data, then clean results for analysis
#---
  zillowComps.keep.df <- zillowComps.df %>% select(zpid, 
                                                   zipcode,
                                                   taxAssessmentYear, 
                                                   taxAssessment,
                                                   yearBuilt,
                                                   lotSizeSqFt,
                                                   finishedSqFt,
                                                   bathrooms,
                                                   bedrooms,
                                                   #totalRooms,  ## Too many of the total rooms are missing to be usable.
                                                   lastSoldDate,
                                                   lastSoldPrice,
                                                   zEstAmount = amount,
                                                   `last-updated`,
                                                   valueChange,
                                                   low,
                                                   high,
                                                   percentile,
                                                   zindexValue)
  # zillowUpdates.keep.df <- zillowUpdates %>% select(zpid,
  #                                                   street,
  #                                                   zipcode,
  #                                                   city,
  #                                                   state,
  #                                                   numRooms,
  #                                                   bedrooms,
  #                                                   bathrooms,
  #                                                   finishedSqFt,
  #                                                   lotSizeSqFt,
  #                                                   yearBuilt,
  #                                                   coveredParkingSpaces,
  #                                                   roof,
  #                                                   exteriorMaterial,
  #                                                   heatingSources,
  #                                                   heatingSystem,
  #                                                   coolingSystem,
  #                                                   floorCovering,
  #                                                   architecture,
  #                                                   numFloors,
  #                                                   basement,
  #                                                   parkingType,
  #                                                   coveredParkingSpaces)
  # zillowMerged.df <- left_join(zillowUpdates.keep.df,zillowComps.keep.df,by="zpid")
  zillowMerged.df <- zillowComps.keep.df
    rm(zillowComps.keep.df)
    # rm(zillowUpdates.keep.df)
    zillowMerged.df$lastSoldDate <- mdy(zillowMerged.df$lastSoldDate)
    zillowMerged.df$`last-updated` <- mdy(zillowMerged.df$`last-updated`)  
    
    # can't estimate based on last assesment or last sold price if we don't have an assessment or last sold price...
    zillowMerged.df <- zillowMerged.df[is.na(zillowMerged.df$taxAssessmentYear)==FALSE,]
    zillowMerged.df <- zillowMerged.df[is.na(zillowMerged.df$lastSoldDate)==FALSE,]
    
  zillowMerged.df <- zillowMerged.df %>% rowwise() %>% mutate(natAvgAssessYear = getYearAvgFromMonthly(medianHousePrice.df,taxAssessmentYear))
  zillowMerged.df <- ungroup(zillowMerged.df)
  zillowMerged.df <- zillowMerged.df %>% rowwise() %>% mutate(natAvgLastSoldYear = getMedianFromDate(medianHousePrice.df,lastSoldDate))
    zillowMerged.df <- zillowMerged.df[zillowMerged.df$natAvgLastSoldYear != 0,]                                  
    zillowMerged.df <- ungroup(zillowMerged.df)
  zillowMerged.df <- zillowMerged.df %>% rowwise() %>% mutate(adjTaxAssessment = as.numeric(taxAssessment) * 
                                                  (getYearAvgFromMonthly(medianHousePrice.df,2017)/
                                                     getYearAvgFromMonthly(medianHousePrice.df,taxAssessmentYear)))
  zillowMerged.df <- zillowMerged.df %>% rowwise() %>% mutate(adjLastSoldPrice = as.numeric(lastSoldPrice) * 
                                                                (getYearAvgFromMonthly(medianHousePrice.df,2017)/
                                                                   getYearAvgFromMonthly(medianHousePrice.df,year(lastSoldDate))))
#---
# Create regression model
#---
  #-
  # Create temp variable to protect raw data, cleanup temp
  #-
    zillowMerged.int.df <- zillowMerged.df
    # remove , from zindexValue number
    zillowMerged.int.df$zindexValue <- str_replace(zillowMerged.int.df$zindexValue,",","")
    
    # convert numeric columns to numeric
    for(i in c(4:9,11,12,14:20)) {
      zillowMerged.int.df[,i] <- as.data.frame(lapply(zillowMerged.int.df[,i],as.numeric),stringsAsFactors = FALSE)
    }
    
    zillowMerged.train.df <- na.omit(zillowMerged.int.df[1:588,]) 
    zillowMerged.test.df <- na.omit(zillowMerged.int.df[589:888,])
      rm(zillowMerged.int.df)
  #-
  # Review Data to determine good variables
  #-
    zillow.cor <- cor(na.omit(zillowMerged.train.df[,c(4:9,11,12,14:20)])) 
    corrplot(zillow.cor)
    # scatterplot(zillowMerged.train.df$lotSizeSqFt, zillowMerged.train.df$zEstAmount)
    
    
    
    
    # zillow.model <- lm(zindexValue ~ finishedSqFt + bedrooms + bathrooms + lotSizeSqFt + zipcode + taxAssessment, data=zillowMerged.train.df)
    # summary(zillow.model)
    # confint(zillow.model)
    # residualPlot(zillow.model)
    # influenceIndexPlot(zillow.model)
    # #check for colinearity problems > 10
    # vif(zillow.model) 
   
  #-
  # Scale Data
  #-
    
  #-
  # Transforming Data
  #-
    
  #-
  # Using stepAIC to predict which variables to use in model
  #-
    AIC <- ols_stepaic_forward(zillow.model,details=TRUE)
    AIC
  
  #-
  # Try an adjusted model based on AIC
  #-
    zillow.model <- lm(zindexValue ~ bedrooms + bathrooms + lotSizeSqFt + zipcode, data=zillowMerged.int.df)
    summary(zillow.model)
    confint(zillow.model)
    residualPlot(zillow.model)
    influenceIndexPlot(zillow.model)
    #check for colinearity problems > 10
    vif(zillow.model)
    