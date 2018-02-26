matrixForTFIDF <- function(sectors,rfmData, name){
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
  png(filename = paste(name, " 구매내역.png", sep = ""))
  par("mar")
  par(mai=c(1,2,1,1))
  barplot(head(sort(colSums(rfmData), decreasing = T), 20), horiz = T, las = 1, main = paste(name, "구매내역"))
  dev.off()
}



# margin 문제 해결
par("mar")
par(mai=c(1,2,1,1))
temp <- shp.rec1.freq1.mone1.preference

matrixForTFIDF(A02, shpA02.mon9.rfm.data.1, "A02 그룹 1")
matrixForTFIDF(A02, shpA02.mon9.rfm.data.2, "A02 그룹 2")
matrixForTFIDF(A02, shpA02.mon9.rfm.data.3, "A02 그룹 3")
matrixForTFIDF(A02, shpA02.mon9.rfm.data.4, "A02 그룹 4")

matrixForTFIDF(A03, shpA03.mon9.rfm.data.1, "A03 그룹 1")
matrixForTFIDF(A03, shpA03.mon9.rfm.data.2, "A03 그룹 2")
matrixForTFIDF(A03, shpA03.mon9.rfm.data.3, "A03 그룹 3")
matrixForTFIDF(A03, shpA03.mon9.rfm.data.4, "A03 그룹 4")

matrixForTFIDF(A04, shpA04.mon9.rfm.data.1, "A04 그룹 1")
matrixForTFIDF(A04, shpA04.mon9.rfm.data.2, "A04 그룹 2")
matrixForTFIDF(A04, shpA04.mon9.rfm.data.3, "A04 그룹 3")
matrixForTFIDF(A04, shpA04.mon9.rfm.data.4, "A04 그룹 4")
