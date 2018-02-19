matrixForTFIDF <- function(sectors,rfmData){
  rfmData <- merge(eval(parse(text = sectors))[,c("ID", "PD_H_NM")],
                   rfmData[,c(1,2)],
                   by = "ID",
                   all.x = T)
  rfmData <- na.omit(rfmData)
  rfmData <- rfmData[, c("ID","PD_H_NM")]
  rfmData <- cbind(rfmData[,c("ID", "PD_H_NM")],
                    dummy(rfmData$PD_H_NM, sep = "-"))
  
  cols <- c()
  for( i in colnames(rfmData)){
    if((i != "ID") & (i != "PD_H_NM")){
      i <- strsplit(i, "-")
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
temp <- shp.rec1.freq1.mone1.preference
barplot(head(sort(colSums(temp), decreasing = T), 20), horiz = T, las = 1)
