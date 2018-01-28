library(arulesViz)

# 10등급 쇼핑 외 점포 이용

str(exShpInfo)
str(rate90)
exShpInfo$ID <- factor(exShpInfo$ID)


exshp.rate90.merge <- merge(exShpInfo, rate90, by = "ID", all.y = T)
str(exshp.rate90.merge)
exshp.rate90.merge.sub <- exshp.rate90.merge[, c(1,2)]
str(exshp.rate90.merge.sub)

exshp.rate90.merge.df <- as.data.frame.matrix(table(exshp.rate90.merge.sub$ID, exshp.rate90.merge.sub$BIZ_UNIT))
exshp.rate90.merge.df <- exshp.rate90.merge.df[rowSums(exshp.rate90.merge.df) > 0,]
exshp.rate90.merge.df[exshp.rate90.merge.df > 1] <- 1
exshp.rate90.merge.tran <- as.matrix(exshp.rate90.merge.df)
exshp.rate90.merge.tran <- as(exshp.rate90.merge.tran, "transactions")



class(exshp.rate90.merge.tran)
summary(exshp.rate90.merge.tran)
inspect(exshp.rate90.merge.tran[1:10])
itemFrequency(exshp.rate90.merge.tran, type = 'absolute')

# margin 문제 해결
par("mar")
par(mar = c(1, 1, 1, 1))

itemFrequencyPlot(exshp.rate90.merge.tran, topN = 9)








