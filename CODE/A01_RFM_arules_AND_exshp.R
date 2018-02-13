#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/total_shp.RDa")

library(arulesViz)


# arules를 위한 transaction matrix 생성
createTransaction <- function(rfmData){
  rfmData <- merge(total_shp[,c("ID", "PD_M_NM")],
                rfmData[,c(1,2)],
                by = "ID",
                all.x = T)
  rfmData <- na.omit(rfmData)
  rfmData <- rfmData[, c("ID","PD_M_NM")]
  rfmData[, "PD_M_NM"] <- as.factor(rfmData[, "PD_M_NM"])
  rfmData <-  as.data.frame.matrix(table(rfmData$ID, rfmData$PD_M_NM))
  rfmData <- rfmData[rowSums(rfmData) > 0,]  
  rfmData[rfmData > 1] <- 1 
  rfmData <- as.(as.matrix(rfmData), "transactions")
  return(rfmData)
}

#분석 예제
shpA01.rfm.rec1.freq1.mona1.transaction <- createTransaction(shpA01.rfm.rec1.freq1.mona1)
class(shpA01.rfm.rec1.freq1.mona1.transaction)
summary(shpA01.rfm.rec1.freq1.mona1.transaction)
inspect(shpA01.rfm.rec1.freq1.mona1.transaction[1:10])
itemFrequency(shpA01.rfm.rec1.freq1.mona1.transaction, type = 'absolute')



#exShpInfo
rfmWithExShp <- function(rfmData){
  rfmData <- na.omit(merge(exShpInfo[, c("ID", "BIZ_UNIT")],
                   rfmData[, c(1,2)],
                   by = "ID",
                   all.x = T))
  rfmData <- rfmData[, c("ID", "BIZ_UNIT")]
  return(rfmData)
}

# margin 문제 해결
#(B01:호텔, B02:여행사, B03:면세점,C01:영화관, C02:테마파크, C03:야구관람, D01:패스트푸드, D02:패밀리레스토랑, D03:카페)

par("mar")
par(mar = c(1, 1, 1, 1))

#table()
#barplot(,names = c("B01", "B02", "B03", "C01", "C02", "c03", "D01", "D02", "D03"))
