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
                        'XML')
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
  
  jcky.addresses <- createResidentialAddressList('Jefferson_County_KY_Zoning.csv',
                                                 'Jefferson_County_KY_Address_Points.csv')
  # dbWriteTable(mydb, "jckyaddresses", jcky.addresses)  
  
  # Selecting a random list of 6k addresses. Given an API limit of 1k per day, this
  # should cover the needs of an analysis due in 6 days.
  jcky.tolookup <- jcky.addresses %>% sample_n(6000, replace=FALSE)
  dbWriteTable(mydb, "jckytolookup", jcky.tolookup)
  rm(jcky.addresses)

#---
# Create list of data for median house price per month
#---
  medianHousePrice.df <- read_csv("MSPNHSUS.csv")
   dbWriteTable(mydb,"medianHousePrice",medianHousePrice.df)
#---
# Download Zillow data from API
#---
  # Get 500 addresses from list of addresses and do a deep search on zillow
  zillowSearch.df <- multipleDeepSearchZillow(jcky.tolookup,1000:1500)
    dbWriteTable(mydb, "zillowDeepSearch",zillowSearch.df)
  
  # Unfortunately, the deep search doesn't include enough information.
  # Also, many of these randomly selected properties are not in the Updated data 
  # because they have not been sold recently enought to be in Zillow database.
  # However, most comps have been updated with full details, and comps have a bit
  # more information themselves...so next step is to get comps, we should be able
  # to get 25 per zpid and per API call...so let's do it.
  zillowComps.df <- multipleDeepCompsZillow(zillowSearch.df$zpid,25)
    # if total rooms is empty there is less likely to be Updated data...so remove those
    zillowComps.df <- zillowComps.df %>% filter(is.na(totalRooms)==FALSE)
    
  
  # It is possible these comps are duplicates. Need to fix that.
  zillowComps.df <- zillowComps.df[duplicated(zillowComps.df$zpid)==FALSE,]
    dbWriteTable(mydb, "zillowDeepComps",zillowComps.df)
    
  # Finally, what we really want is the Updated data because it is quite rich. API calls
  # are limited so 500 of these should be a good number.
  zillowUpdates <- multipleUpdatedPropertyDetails(zillowComps.df$zpid[1:500])
    
#---
# Combine median house prices, zillow deep comps data and zillow update data, then clean results for analysis
#---
  

