#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/total_shp.RDa")

library(arulesViz)

str(shpInfo.A01.rfm.data.rec1.freq1.monet1)
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge <- merge(total_shp,
                                                      shpInfo.A01.rfm.data.rec1.freq1.monet1,
                                                      by = "ID",
                                                      all.x = T)
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge <- na.omit(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge)
str(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge)

shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp <- shpInfo.A01.rfm.data.rec1.freq1.monet1.merge[, c("ID", "PD_M_NM")]
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$PD_M_NM <- as.factor(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$PD_M_NM)


table(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$ID, shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$PD_M_NM)
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp <- as.data.frame.matrix(table(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$ID, shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp$PD_M_NM)
)


shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp <- shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp[rowSums(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp) > 0,]
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp[shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp > 1] <- 1
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp <- as.matrix(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp)
shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp <- as(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp, "transactions")



class(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp)
summary(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp)
inspect(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp[1:10])
itemFrequency(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp, type = 'absolute')

# margin 문제 해결
par("mar")
par(mar = c(1, 1, 1, 1))

itemFrequencyPlot(shpInfo.A01.rfm.data.rec1.freq1.monet1.merge.temp, topN = 10)




shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp <- merge(exShpInfo, 
                                                      shpInfo.A01.rfm.data.rec1.freq1.monet1,
                                                      by = "ID",
                                                      all.x = T)
shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp <- na.omit(shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp)
str(shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp)
shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp <- shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp[, c(1,2)]
table(shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp$BIZ_UNIT)
barplot(table(shpInfo.A01.rfm.data.rec1.freq1.monet1.exshp$BIZ_UNIT),
        names = c("B01", "B02", "B03", "C01", "C02", "c03", "D01", "D02", "D03"))
"""
(B01:호텔, B02:여행사, B03:면세점,
C01:영화관, C02:테마파크, C03:야구관람, D01:패스트푸드, D02:패밀리레스토랑, D03:카페)
"""