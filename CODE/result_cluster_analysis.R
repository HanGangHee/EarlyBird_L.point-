#그룹별 관심 사항 분석



shp.customer.merge <- merge(shpInfo, customer.loyalty, by = "ID", rm.na = T)
head(shp.customer.merge)

# shpInfo를 클러스터별로 분리
cluster1 <- shp.customer.merge[shp.customer.merge$cluster == 1, ]
cluster2 <- shp.customer.merge[shp.customer.merge$cluster == 2, ]
cluster3 <- shp.customer.merge[shp.customer.merge$cluster == 3, ]
cluster4 <- shp.customer.merge[shp.customer.merge$cluster == 4, ]

##### 클러스터별로 관심 BIZ UNIT 분석 ######
tmp <- as.matrix(table(shp.customer.merge$BIZ_UNIT))
tmp1 <- as.matrix(table(cluster1$BIZ_UNIT))
tmp2 <- as.matrix(table(cluster2$BIZ_UNIT))
tmp3 <- as.matrix(table(cluster3$BIZ_UNIT))
tmp4 <- as.matrix(table(cluster4$BIZ_UNIT))


slice1 <- c(tmp1[1,1], tmp1[2,1], tmp1[3,1], tmp1[4,1], tmp1[5,1])
slice2 <- c(tmp2[1,1], tmp2[2,1], tmp2[3,1], tmp2[4,1], tmp2[5,1])
slice3 <- c(tmp3[1,1], tmp3[2,1], tmp3[3,1], tmp3[4,1], tmp3[5,1])
slice4 <- c(tmp4[1,1], tmp4[2,1], tmp4[3,1], tmp4[4,1], tmp4[5,1])

lbls2 <- c("A01", "A02", "A03", "A04", "A05")

pct1 <- round(slice1/sum(slice1)*100)
pct2 <- round(slice2/sum(slice2)*100)
pct3 <- round(slice3/sum(slice3)*100)
pct4 <- round(slice4/sum(slice4)*100)

lbls2 <- paste(lbls2, pct1, sep=": ")
lbls2 <- paste(lbls2, "%", sep="")

pie(slice1, labels = lbls2, main="cluser1", col = rainbow(length(lbls2)))
pie(slice2, labels = lbls2, main="cluser2", col = rainbow(length(lbls2)))
pie(slice3, labels = lbls2, main="cluser3", col = rainbow(length(lbls2)))
pie(slice4, labels = lbls2, main="cluser4", col = rainbow(length(lbls2)))








