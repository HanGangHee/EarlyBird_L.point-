matrixForTFIDF <- function(sectors,rfmData){
  rfmData <- merge(eval(parse(text = sectors))[,c("ID", "PD_M_NM")],
                   rfmData[,c(1,2)],
                   by = "ID",
                   all.x = T)
  rfmData <- rfmData[, c("ID","PD_M_NM")]
  rfmData <- na.omit(rfmData)
  rfmData <-  cbind(rfmData[,c("ID", "PD_M_NM")],
                    dummy(rfmData$PD_M_NM, sep = ","))
  
  cols <- c()
  for( i in colnames(rfmData)){
    if((i != "ID") & (i != "PD_M_NM")){
      i <- strsplit(i, ",")
      i <- i[[1]][2]
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
  return(rfmData)
}

# margin 문제 해결
par("mar")
par(mai=c(1,2,1,1))
temp <- matrixForTFIDF(A05, shpA05.rec1.freq1.mone2)
barplot(head(sort(colSums(temp), decreasing = T), 20), horiz = T, las = 1)
