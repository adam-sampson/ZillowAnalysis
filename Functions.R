## Load libraries or ask to install them
loadLibraries <- function(requiredPackages = NULL) {
  for(pack in requiredPackages) {
    # If statement will either load the library or give user the option to auto-install if not available
    if(!require(pack,character.only = TRUE)) {
      # ask the user for permission to install
      if(menu(c('yes','no'),title=paste0('Package ',pack,' not found. Would you like to install?'))==1) {
        install.packages(pack)
        delay(1000)
        require(pack)
      }
    }
  }
}

## Load Jefferson County data for addresses and zoning and output single residence addresses
createResidentialAddressList <- function(jcky.zoning, jcky.addresses) {
  
  if(is.character(jcky.zoning)==TRUE && file.exists(jcky.zoning)) {
    jcky.zoning <- read_csv(jcky.zoning)
  } else if(is.data.frame(jcky.zoning)==FALSE) {
    stop("Expecting file name string or dataframe as input")
  }
  
  if(is.character(jcky.addresses)==TRUE && file.exists(jcky.addresses)) {
    jcky.addresses <- read_csv(jcky.addresses)
  } else if(is.data.frame(jcky.addresses)==FALSE) {
    stop("Expecting file name string or dataframe as input")
  }
    
  
  jcky.zoning <- jcky.zoning %>% select(ZONING_COD,ZONING_NAM,ZONING_TYP) %>%
                 distinct()
  names(jcky.zoning)[1] <- "ZONING_CODE"
  
  
  jcky.addresses <- jcky.addresses %>% select(FULL_ADDRESS,APT,MUNI_NAME,ZIPCODE,ZONING_CODE)
  jcky.addresses <- jcky.addresses %>% filter(is.na(APT)==TRUE) %>% select(-APT)
  
  jcky.addresses <- left_join(jcky.addresses,jcky.zoning)
  jcky.addresses <- jcky.addresses %>% filter(ZONING_TYP == "RESIDENTIAL") %>%
                    filter(ZONING_NAM == "RES SINGLE FAMILY" | ZONING_NAM == "RES SINGLE FAMILY OVERLAY" | 
                           ZONING_NAM == "RURAL RESIDENTIAL" | ZONING_NAM == "RURAL RES OVERLAY")
}

recursiveListExtract <- function(xml.obj) {
  ## We can take advantage of the fact that only objects without children have values
  
}

flattenZillowList <- function(in.list) {
  # flatten the list 
  in.list <- unlist(in.list)
  
  # cleanup the output to usable format
  # each actual name/value is in format name = in.list[n] value = in.list[n+2] so check for this
  out.mat <- NULL
  for(cnt in 1:(length(in.list)-2)) {
    if(endsWith(attributes(in.list[cnt])$names,"name") == TRUE && 
       endsWith(attributes(in.list[cnt+2])$names,"value") == TRUE) {
      temp <- matrix(data=in.list[[cnt+2]],dimnames = list(NULL,in.list[[cnt]]))
      out.mat <- cbind(out.mat,temp)
    }
  }
  out.mat <- as.data.frame(out.mat,stringsAsFactors=FALSE)
  return(out.mat)
}

multipleDeepSearchZillow <- function(in.df, posVector) {
  zillowSearchOut <- NULL
  for(i in posVector) {
    zillowTemp <- GetDeepSearchResults(address = in.df$FULL_ADDRESS[i],citystatezip = as.character(in.df$ZIPCODE[i]),zws_id = get_zillow_web_service_id())
    zillowTemp <- flattenZillowList(zillowTemp)
    zillowSearchOut <- bind_rows(zillowSearchOut,zillowTemp)
  }
  return(zillowSearchOut)
}

multipleDeepCompsZillow <- function(zpidVector,count) {
  zillowUpdatedOut <- NULL
  for(i in 1:length(zpidVector)) {
    zillowTemp <- GetDeepComps(zpid = as.character(zpidVector[i]),count=count,zws_id = get_zillow_web_service_id())
    if(startsWith(zillowTemp[[2]]$text,"Error")==FALSE) {
      for(j in 1:length(zillowTemp[[3]][[1]][[2]])) {
        zillowTemp2 <- flattenZillowList(zillowTemp[[3]][[1]][[2]][j])
        zillowUpdatedOut <- bind_rows(zillowUpdatedOut,zillowTemp2)
      }
    }
  }
  return(zillowUpdatedOut)
}

multipleUpdatedPropertyDetails <- function(zpidVector) {
  zillowUpdatedOut <- NULL
  for(i in 1:length(zpidVector)) {
    zillowTemp <- GetUpdatedPropertyDetails(zpid = as.character(zpidVector[i]),zws_id = get_zillow_web_service_id())
    if(startsWith(zillowTemp[[2]]$text,"Error")==FALSE) {
      zillowTemp <- flattenZillowList(zillowTemp)
      zillowUpdatedOut <- bind_rows(zillowUpdatedOut,zillowTemp)
    }
  }
  return(zillowUpdatedOut)
}

getYearAvgFromMonthly <- function(in.df,year) {
  in.df <- in.df[year(in.df$DATE)==year,]
  out <- in.df %>% group_by(year(DATE)) %>% summarise(mean = trunc(mean(MSPNHSUS)))
  return(out$mean)
}

getMedianFromDate <- function(in.df,in.date){
  in.df <- in.df[year(in.df$DATE) == year(in.date),]
  in.df <- in.df[month(in.df$DATE) == month(in.date),]
  out <- in.df$MSPNHSUS
  if(length(out) == 0) { out <- 0 }
  return(out)
}
