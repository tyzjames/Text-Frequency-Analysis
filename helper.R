
#Return a list of words and their position in the list
findwords <- function(input) {
  
  tempList <- list()
  
  for (i in 1:length(input)){
    word <- input[[i]]
    tempList[[word]] <- c(tempList[[word]], i)
  }
  
  return (tempList)
  
}

#Returns a list of words that are split by spaces. Removes empty vectors. Possible to use reg exp to remove instead.
SplitStringToGroups <- function(input) {
  a<-input
  a<-strsplit(a, split=' ')[[1]]
  a<-a[a!='']
  return(a)
}


#Returns a list of lists. Sublist contains words that are alone, joined to the next word, and joined to the next two words.
getMatchingString <- function (container) {
  cSingle <- c()
  cDouble <- c()
  cTriple <- c()
  z <- list()
  for (i in 1:nrow(container)){
    tempWrd <- toupper(SplitStringToGroups(as.character(container[i,1])))

    cnt <- length(tempWrd)
    
    for (j in 1:cnt) {
      if (!is.na(tempWrd[j+1])) {
        cSingle <- c(cSingle, paste0('%',tempWrd[j], '%')) 
      }
      if (j != cnt && !is.na(tempWrd[j+1])) {
        cDouble <- c(cDouble,paste0('%',paste(tempWrd[j], tempWrd[j+1], sep='%'),'%'))
      }
      
      if (j + 1 != cnt && !is.na(tempWrd[j+1]) && !is.na(tempWrd[j+2])) {
        cTriple <- c(cTriple,paste0('%',paste(tempWrd[j], tempWrd[j+1], tempWrd[j+2], sep='%'),'%'))
      }
      
    }
    
    z[["cSingle"]] <- c(z[["cSingle"]],cSingle)
    z[["cDouble"]] <- c(z[["cDouble"]],cDouble)
    z[["cTriple"]] <- c(z[["cTriple"]],cTriple)
    
  }
  
  return(z)
}