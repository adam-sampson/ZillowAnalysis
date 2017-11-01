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
mydb <- dbConnect(RSQLite::SQLite(),"my-db.sqlite")

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
  # Use 1420 Christy Ave, 40204 for the starting home
  principal.address <- GetDeepSearchResults(address = "1420 Christy Ave",citystatezip = "40204",zws_id = get_zillow_web_service_id())
  principal.address <- singleListExtract(principal.address$response)

  comps <- multipleDeepCompsZillow(principal.address$zpid,25)
  for(comp in comps) {
    temp <- multipleDeepCompsZillow(comp[1],25)
    comps <- bind_rows(comps,temp)
    rm(temp)
  }
  comps <- comps[duplicated(comps$street)==FALSE,]
  for(comp in comps[26:28,]) {
    temp <- multipleDeepCompsZillow(comp[1],25)
    comps <- bind_rows(comps,temp)
    rm(temp)
  }
  comps <- comps[duplicated(comps$street)==FALSE,]
  for(comp in comps[29:45,]) {
    temp <- multipleDeepCompsZillow(comp[1],25)
    comps <- bind_rows(comps,temp)
    rm(temp)
  }
  comps <- comps[duplicated(comps$street)==FALSE,]
  for(comp in comps[46:54,]) {
    temp <- multipleDeepCompsZillow(comp[1],25)
    comps <- bind_rows(comps,temp)
    rm(temp)
  }
  comps <- comps[duplicated(comps$street)==FALSE,]
  for(comp in comps[55:68,]) {
    temp <- multipleDeepCompsZillow(comp[1],25)
    comps <- bind_rows(comps,temp)
    rm(temp)
  }
  comps <- comps[duplicated(comps$street)==FALSE,]
  
  updatedDetails <- multipleUpdatedPropertyDetails(comps$zpid)
  
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
      
  
  
#---
# Create regression model
#---

