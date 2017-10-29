## Load libraries or ask to install them
loadLibraries <- function(requiredPackages = NULL) {
  for(pack in requiredPackages) {
    # If statement will either load the library or give user the option to auto-install if not available
    if(!require(pack,character.only = TRUE)) {
      # ask the user for permission to install
      if(menu(c('yes','no'),title=paste0('Package ',pack,' not found. Would you like to install?'))==1) {
        install.packages(pack)
        Sys.sleep(1)
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

recursiveListExtract <- function(input.list) {
  if(("children" %in% attributes(input.list)$names) & !("value" %in% attributes(input.list$children$text)$names)) {
    print("Debug - Detected children in list")
    #recursively call the function
    for(each.list in input.list$children){
      temp <- recursiveListExtract(each.list)
      if(length(temp)>0) {
        if(exists('out')==TRUE) {
          out <- cbind(out,temp)
        } else {
          out <- temp
        }
      }
    }
  } 
  if(("value" %in% attributes(input.list$children$text)$names)) {
    print("Values detected - convert") 
    print(input.list$name)
    print(input.list[[1]])
    print(paste0(unlist(input.list[[1]]$value)))
    out <- data.frame(paste0(input.list$children$text$value))
    print(out)
    colnames(out) <- input.list$name
    print(out)
  }
  # if(exists("out") == FALSE) {
  #   out <- NULL
  # }
  if(exists("out")==FALSE) {out <- data.frame()}
  return(out)
}

multipleListExtract <- function(input.list) {
  for(each.list in input.list$children) {
    tempList <- recursiveListExtract(each.list)
    if(exists('outList')==TRUE) {
      outList <- bind_rows(outList,tempList)
    } else {
      outList <- tempList
    }
  }
  return(outList)
}

singleListExtract <- function(input.list) {
  tempList <- recursiveListExtract(input.list)
  outList <- tempList
  return(outList)
}

# flattenZillowList <- function(in.list) {
#   # flatten the list 
#   in.list <- unlist(in.list)
#   
#   # cleanup the output to usable format
#   # each actual name/value is in format name = in.list[n] value = in.list[n+2] so check for this
#   out.mat <- NULL
#   for(cnt in 1:(length(in.list)-2)) {
#     # Either there will be name followed by value 2 later or
#     # there may be name followed by currency then value 2 later
#     ## Error: Rule Fails.
#     if(endsWith(attributes(in.list[cnt])$names,"name") == TRUE && 
#        endsWith(attributes(in.list[cnt+2])$names,"value") == TRUE) {
#       temp <- matrix(data=in.list[[cnt+2]],dimnames = list(NULL,in.list[[cnt]]))
#       out.mat <- cbind(out.mat,temp)
#     }
#   }
#   out.mat <- as.data.frame(out.mat,stringsAsFactors=FALSE)
#   return(out.mat)
# }

multipleDeepSearchZillow <- function(in.df, posVector) {
  zillowSearchOut <- NULL
  for(i in posVector) {
    zillowTemp <- GetDeepSearchResults(address = in.df$FULL_ADDRESS[i],citystatezip = as.character(in.df$ZIPCODE[i]),zws_id = get_zillow_web_service_id())
    # zillowTemp <- flattenZillowList(zillowTemp)
    zillowTemp <- multipleListExtract(zillowTemp$response)
    zillowSearchOut <- bind_rows(zillowSearchOut,zillowTemp)
  }
  return(zillowSearchOut)
}

multipleDeepCompsZillow <- function(zpidVector,count) {
  zillowUpdatedOut <- NULL
  for(i in 1:length(zpidVector)) {
    zillowTemp <- GetDeepComps(zpid = as.character(zpidVector[i]),count=count,zws_id = get_zillow_web_service_id())
    if(startsWith(zillowTemp$message$text,"Error")==FALSE) {
      zillowTemp2 <- singleListExtract(zillowTemp$response$children$properties$children$principal)
      zillowUpdatedOut <- bind_rows(zillowUpdatedOut,zillowTemp2)
      for(j in 1:length(zillowTemp$response$children$properties$children$comparables)) {
        zillowTemp2 <- multipleListExtract(zillowTemp$response$children$properties$children$comparables)
        zillowUpdatedOut <- bind_rows(zillowUpdatedOut,zillowTemp2)
      }
    } else {
      warning("Error detected in zillow API call.")
      warning(zillowTemp$message)
    }
  }
  return(zillowUpdatedOut)
}

multipleUpdatedPropertyDetails <- function(zpidVector) {
  zillowUpdatedOut <- NULL
  for(i in 1:length(zpidVector)) {
    zillowTemp <- GetUpdatedPropertyDetails(zpid = as.character(zpidVector[i]),zws_id = get_zillow_web_service_id())
    if(startsWith(zillowTemp[[2]]$text,"Error")==FALSE) {
      # zillowTemp <- flattenZillowList(zillowTemp)
      zillowTemp <- singleListExtract(zillowTemp$response)
      zillowUpdatedOut <- bind_rows(zillowUpdatedOut,zillowTemp)
    } else {
      warning("Error detected in zillow API call.")
      warning(zillowTemp$message)
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
