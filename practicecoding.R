zillowTemp <- GetDeepComps(zpid = as.character(zillowSearch.df$zpid[1]),
                           count=1,
                           zws_id = get_zillow_web_service_id())

badflatten <- flattenZillowList(zillowTemp)

zillowTemp[[3]][[1]][[1]] #principal
zillowTemp[[3]][[1]][[2]] #comparables
zillowTemp[[3]][[1]][[2]][[1]] #comp 1

str(zillowTemp[[3]])
zillowTemp[[3]][[1]][[2]][[1]][[2]]$children
# > attributes(zillowTemp[[3]][[1]][[2]][[1]][[2]])
# $names
# [1] "name"                 "attributes"           "children"             "namespace"           
# [5] "namespaceDefinitions"
# 
# $class
# [1] "XMLNode"          "RXMLAbstractNode" "XMLAbstractNode"  "oldClass"   

zillowTemp$response$children
zillowTemp$response$children$properties$children$comparablesatta

textflatten <- flatten(zillowTemp[[3]][[1]][[2]][[1]])
textflatten <- flatten(textflatten)

testTranspose <- zillowTemp[[3]][[1]][[2]][[1]] %>% transpose()

zillowTemp$response

testToDF1 <- data.frame(matrix(zillowTemp[[3]][[1]][[2]][[1]], nrow=1, byrow=T))
testToDF2 <- data.frame(matrix(zillowTemp[[3]][[1]][[2]][[1]], nrow=1, byrow=T),stringsAsFactors=FALSE)

recursiveListExtract <- function(input.list) {
  out <- data.frame()
  if("children" %in% attributes(input.list)$names) {
    print("Debug - Detected children in list")
    #recursively call the function
    for(each.list in input.list$children){
      temp <- recursiveListExtract(each.list)
      out <- cbind(out,temp)
    }
  } else {
    print("Debug - No children detected in list")
    # There are no children so we must be at the end of the branch
    for(i in 1:length(input.list)) {
      tempdf <- data.frame(input.list[[i]])
      names(tempdf) <- attributes(input.list[i])[[1]]
      out <- cbind(out,tempdf)
    }
  }
  return(out)
}

testdf <- recursiveListExtract(zillowTemp$response)

zillowTemp.xml <- zillowTemp
zillowTemp.xml <- xmlParse(zillowTemp.xml)

