#그룹별 관심 사항 분석
library(dplyr)


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

#나이와 성별 비교

cluster1 <- inner_join(cluster1, customer, by = c("ID" = "ID"))
cluster2 <- inner_join(cluster2, customer, by = c("ID" = "ID"))
cluster3 <- inner_join(cluster3, customer, by = c("ID" = "ID"))
cluster4 <- inner_join(cluster4, customer, by = c("ID" = "ID"))

cluster1.analysis <- cluster1 %>%
  group_by(ID, AGE_PRD, GENDER) %>%
  summarise(count_id = length(ID))
cluster1.analysis <- as.data.frame(cluster1.analysis)


cluster2.analysis <- cluster2 %>%
  group_by(ID, AGE_PRD, GENDER) %>%
  summarise(count_id = length(ID))

cluster3.analysis <- cluster3 %>%
  group_by(ID, AGE_PRD, GENDER) %>%
  summarise(count_id = length(ID))

cluster4.analysis <- cluster4 %>%
  group_by(ID, AGE_PRD, GENDER) %>%
  summarise(count_id = length(ID))

cluster1.analysis <- as.data.frame(cluster1.analysis)
cluster2.analysis <- as.data.frame(cluster2.analysis)
cluster3.analysis <- as.data.frame(cluster3.analysis)
cluster4.analysis <- as.data.frame(cluster4.analysis)

str(cluster2.analysis)

table(cluster1.analysis$AGE_PRD, cluster1.analysis$GENDER)
table(cluster2.analysis$AGE_PRD, cluster2.analysis$GENDER)
table(cluster3.analysis$AGE_PRD, cluster3.analysis$GENDER)
table(cluster4.analysis$AGE_PRD, cluster4.analysis$GENDER)

barplot(c(length(cluster1.analysis$ID),
          length(cluster2.analysis$ID),
          length(cluster3.analysis$ID),
          length(cluster4.analysis$ID)),
        names = c("cluster1", "cluster2", "cluster3", "cluster4"),
        main = "각cluster별 고객의 수",
        col = rainbow(4))


barplot(c(mean(cluster1$BUY_AM),
          mean(cluster2$BUY_AM),
          mean(cluster3$BUY_AM),
          mean(cluster4$BUY_AM)),
        names = c("cluster1", "cluster2", "cluster3", "cluster4"),
        main = "각cluster별 고객의 평균 소비량",
        col = rainbow(8))

barplot(c(mean(cluster1.analysis$count_id),
          mean(cluster2.analysis$count_id),
          mean(cluster3.analysis$count_id),
          mean(cluster4.analysis$count_id)),
        names = c("cluster1", "cluster2", "cluster3", "cluster4"),
        main = "각cluster별 고객의 평균 이용 횟수",
        col = rainbow(8))

barplot(table(cluster1.analysis$AGE_PRD), main="Age of Cluster1")
barplot(table(cluster2.analysis$AGE_PRD), main="Age of Cluster2")
barplot(table(cluster3.analysis$AGE_PRD), main="Age of Cluster3")
barplot(table(cluster4.analysis$AGE_PRD), main="Age of Cluster4")

#cluster1 category merge
cluster1.tmp$PD_S_C <- as.factor(cluster1.tmp$PD_S_C)
category$PD_S_C <- as.factor(category$PD_S_C)
cluster1.tmp <- left_join(cluster1.tmp, category, by.x = intersect("BIZ_UNIT", "PS_S_C"))
cluster1.tmp[cluster1.tmp$PD_H_NM == "?", ] <- NA
cluster1.tmp <- na.omit(cluster1.tmp)


