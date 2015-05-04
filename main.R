source("helper.R")

inputData <- data.frame(read.csv(file="data.csv", header=T, sep=','), stringsAsFactors=F)

#Remove all digits and signs from iName
inputData$cleanName <- gsub("[[:punct:]+[:digit:]+]", " ", inputData[, "INAME"], perl = T)

#Split names by spaces
nameSplit <- strsplit(inputData$cleanName, split=" ")

#Join words back together
for (i in 1:length(nameSplit)) {
  wrd <- NULL
  x<-nameSplit[[i]]
  nameSplit[[i]] <- subset(x, nchar(x)>2)
  for (j in 1:length(nameSplit[[i]])) {
    wrd <- paste(wrd, nameSplit[[i]][j])
  }
  nameSplit[[i]]<-wrd
}

inputData$cleanName <- nameSplit

input <- data.frame(inputData[, c(3)], stringsAsFactors=FALSE)

productFam <- toupper(as.character(unique(inputData$PRODUCTFAM)))
productFam <- productFam[productFam != "OTHERS"]
markets <- toupper(as.character(unique(inputData$MARKET)))


itemListSingle <- list()
itemListDouble <- list()
itemListTriple <- list()


for (i in 1:length(productFam)) {
  
  clean <- data.frame(as.character(inputData[inputData$PRODUCTFAM == productFam[i], "cleanName"]))
  z<- getMatchingString(clean)
  cSingle <- findwords(z$cSingle)
  cDouble <- findwords(z$cDouble)
  cTriple <- findwords(z$cTriple)
  itemListSingle[[productFam[i]]] <- names(tail(cSingle[order(sapply(cSingle, length))], n=10))
  itemListDouble[[productFam[i]]] <- names(tail(cDouble[order(sapply(cDouble, length))], n=10))
  itemListTriple[[productFam[i]]] <- names(tail(cTriple[order(sapply(cTriple, length))], n=10))
  
}


print(itemListSingle)
print(itemListDouble)
print(itemListTriple)


