matrixForTFIDF <- function(sectors,rfmData){
  rfmData <- merge(eval(parse(text = sectors))[,c("ID", "PD_M_NM")],
                   rfmData[,c(1,2)],
                   by = "ID",
                   all.x = T)
  rfmData <- rfmData[, c("ID","PD_M_NM")]
  rfmData <- na.omit(rfmData)
  rfmData <-  cbind(rfmData[,c("ID", "PD_M_NM")],
                    dummy(rfmData$PD_M_NM, sep = ""))
  cols <- c()
  for( i in colnames(rfmData)){
    if(nchar(i) > 7){
      i <- substr(i, 8, nchar(i))
    }
    cols <- c(cols,i)
  }
  colnames(rfmData) <- cols
  rfmData <- rfmData[, -c(2)]
  rfmData <- aggregate(. ~ ID, 
                       data = rfmData,
                       FUN = sum)
  rfmData <- rfmData[, -c(1)] #ID  DATE 제거
  
  rfmData <- rfmData[rowSums(rfmData) > 0,]  
  rfmData <- as.data.frame(t(rfmData))
  return(rfmData)
}

#for(i in 1:length(rfmData$`2`)){
#  tf = sum(rfmData[i, ])
#}
#rowSums(rfmData)