library(arulesViz)

#install.packages("dummies")
library(dummies)
library(dplyr)
getwd()


summary(shpA02.mon9.rfm.data$rfm)



shpA02.mon9.rfm.data.1 <- shpA02.mon9.rfm.data[shpA02.mon9.rfm.data$rfm <= 2.78, ]
shpA02.mon9.rfm.data.2 <- shpA02.mon9.rfm.data[shpA02.mon9.rfm.data$rfm > 2.78 &
                                                 shpA02.mon9.rfm.data$rfm <= 4.183, ]
shpA02.mon9.rfm.data.3 <- shpA02.mon9.rfm.data[shpA02.mon9.rfm.data$rfm > 4.183 &
                                                 shpA02.mon9.rfm.data$rfm <= 5.620, ]
shpA02.mon9.rfm.data.4 <- shpA02.mon9.rfm.data[shpA02.mon9.rfm.data$rfm > 5.62, ]

length(shpA02.mon9.rfm.data.1$ID) + length(shpA02.mon9.rfm.data.2$ID) + length(shpA02.mon9.rfm.data.3$ID) +length(shpA02.mon9.rfm.data.4$ID) 

shpA02.mon9.rfm.data.1$rfm <- as.factor(shpA02.mon9.rfm.data.1$rfm)
shpA02.mon9.rfm.data.2$rfm <- as.factor(shpA02.mon9.rfm.data.2$rfm)
shpA02.mon9.rfm.data.3$rfm <- as.factor(shpA02.mon9.rfm.data.3$rfm)
shpA02.mon9.rfm.data.4$rfm <- as.factor(shpA02.mon9.rfm.data.4$rfm)


table(shpA02.mon9.rfm.data.1$rfm)
table(shpA02.mon9.rfm.data.2$rfm)
table(shpA02.mon9.rfm.data.3$rfm)
table(shpA02.mon9.rfm.data.4$rfm)




summary(shpA03.mon9.rfm.data$rfm)



shpA03.mon9.rfm.data.1 <- shpA03.mon9.rfm.data[shpA03.mon9.rfm.data$rfm <= 2.42, ]
shpA03.mon9.rfm.data.2 <- shpA03.mon9.rfm.data[shpA03.mon9.rfm.data$rfm > 2.42 &
                                                 shpA03.mon9.rfm.data$rfm <= 4.84, ]
shpA03.mon9.rfm.data.3 <- shpA03.mon9.rfm.data[shpA03.mon9.rfm.data$rfm > 4.84 &
                                                 shpA03.mon9.rfm.data$rfm <= 6.27, ]
shpA03.mon9.rfm.data.4 <- shpA03.mon9.rfm.data[shpA03.mon9.rfm.data$rfm > 6.27, ]

length(shpA03.mon9.rfm.data.1$ID) + length(shpA03.mon9.rfm.data.2$ID) + length(shpA03.mon9.rfm.data.3$ID) +length(shpA03.mon9.rfm.data.4$ID) 

shpA03.mon9.rfm.data.1$rfm <- as.factor(shpA03.mon9.rfm.data.1$rfm)
shpA03.mon9.rfm.data.2$rfm <- as.factor(shpA03.mon9.rfm.data.2$rfm)
shpA03.mon9.rfm.data.3$rfm <- as.factor(shpA03.mon9.rfm.data.3$rfm)
shpA03.mon9.rfm.data.4$rfm <- as.factor(shpA03.mon9.rfm.data.4$rfm)


table(shpA03.mon9.rfm.data.1$rfm)
table(shpA03.mon9.rfm.data.2$rfm)
table(shpA03.mon9.rfm.data.3$rfm)
table(shpA03.mon9.rfm.data.4$rfm)


summary(shpA04.mon9.rfm.data$rfm)



shpA04.mon9.rfm.data.1 <- shpA04.mon9.rfm.data[shpA04.mon9.rfm.data$rfm <= 2.84, ]
shpA04.mon9.rfm.data.2 <- shpA04.mon9.rfm.data[shpA04.mon9.rfm.data$rfm > 2.84 &
                                                 shpA04.mon9.rfm.data$rfm <= 4.12, ]
shpA04.mon9.rfm.data.3 <- shpA04.mon9.rfm.data[shpA04.mon9.rfm.data$rfm > 4.12 &
                                                 shpA04.mon9.rfm.data$rfm <= 5.4, ]
shpA04.mon9.rfm.data.4 <- shpA04.mon9.rfm.data[shpA04.mon9.rfm.data$rfm > 5.4, ]

length(shpA04.mon9.rfm.data.1$ID) + length(shpA04.mon9.rfm.data.2$ID) + length(shpA04.mon9.rfm.data.3$ID) +length(shpA04.mon9.rfm.data.4$ID) 

shpA04.mon9.rfm.data.1$rfm <- as.factor(shpA04.mon9.rfm.data.1$rfm)
shpA04.mon9.rfm.data.2$rfm <- as.factor(shpA04.mon9.rfm.data.2$rfm)
shpA04.mon9.rfm.data.3$rfm <- as.factor(shpA04.mon9.rfm.data.3$rfm)
shpA04.mon9.rfm.data.4$rfm <- as.factor(shpA04.mon9.rfm.data.4$rfm)


table(shpA04.mon9.rfm.data.1$rfm)
table(shpA04.mon9.rfm.data.2$rfm)
table(shpA04.mon9.rfm.data.3$rfm)
table(shpA04.mon9.rfm.data.4$rfm)

# rfm data를 arules를 위한 transaction matrix으로 변환 
createTransaction <- function(sectors,rfmData){
  rfmData <- merge(eval(parse(text = sectors))[,c("ID", "PD_H_NM", "DE_DT")],
                   rfmData[,c(1,2)],
                   by = "ID",
                   all.x = T)
  rfmData <- na.omit(rfmData)
  rfmData <- rfmData[, c("ID","PD_H_NM", "DE_DT")]
  rfmData <-  cbind(rfmData[,c("ID","DE_DT")],
                    dummy(rfmData$PD_H_NM, sep = "-"))
  cols <- c()
  for( i in colnames(rfmData)){
    if((i != "ID") & (i != "DE_DT")){
      i <- strsplit(i, "-")
      i <- i[[1]][2]
    }
    cols <- c(cols,i)
  }
  colnames(rfmData) <- cols  
  
  rfmData <- aggregate(. ~ ID + DE_DT, 
                       data = rfmData,
                       FUN = sum)
  
  rfmData <- rfmData[, -c(1, 2)] #ID  DATE 제거
  rfmData[rfmData > 1] <- 1 
  rfmData <- as(as.matrix(rfmData), "transactions")
  return(rfmData)
}


#exShpInfo
rfmWithExShp <- function(name, rfmData){
  rfmData <- na.omit(merge(exShpInfo[, c("ID", "BIZ_UNIT")],
                           rfmData[, c(1,2)],
                           by = "ID",
                           all.x = T))
  rfmData <- rfmData[, c("ID", "BIZ_UNIT")]
  png(filename = paste(name, ".png", sep = ""))
  barplot(table(rfmData$BIZ_UNIT), main = paste(name, "쇼핑외 업종 이용"))
  dev.off()
}
rfmWithExShp("A02 그룹 1",shpA02.mon9.rfm.data.1)
rfmWithExShp("A02 그룹 2",shpA02.mon9.rfm.data.2)
rfmWithExShp("A02 그룹 3",shpA02.mon9.rfm.data.3)
rfmWithExShp("A02 그룹 4",shpA02.mon9.rfm.data.4)


rfmWithExShp("A03 그룹 1",shpA03.mon9.rfm.data.1)
rfmWithExShp("A03 그룹 2",shpA03.mon9.rfm.data.2)
rfmWithExShp("A03 그룹 3",shpA03.mon9.rfm.data.3)
rfmWithExShp("A03 그룹 4",shpA03.mon9.rfm.data.4)


rfmWithExShp("A04 그룹 1",shpA04.mon9.rfm.data.1)
rfmWithExShp("A04 그룹 2",shpA04.mon9.rfm.data.2)
rfmWithExShp("A04 그룹 3",shpA04.mon9.rfm.data.3)
rfmWithExShp("A04 그룹 4",shpA04.mon9.rfm.data.4)

genderAndAge <- function(name, rfmData){
  rfmData <- inner_join(rfmData,
                        RAW_Demo,
                        by = "ID")
  rfmData <- na.omit(rfmData)
  
  png(filename = paste(name, " 성별.png", sep = ""))
  barplot(table(rfmData$GENDER), main = paste(name, "성별"))
  dev.off()
  
  png(filename = paste(name, " 나이.png", sep = ""))
  barplot(table(rfmData$AGE_PRD), main = paste(name, "나이"))
  dev.off()
}


genderAndAge("A02 그룹 1",shpA02.mon9.rfm.data.1)
genderAndAge("A02 그룹 2",shpA02.mon9.rfm.data.2)
genderAndAge("A02 그룹 3",shpA02.mon9.rfm.data.3)
genderAndAge("A02 그룹 4",shpA02.mon9.rfm.data.4)


genderAndAge("A03 그룹 1",shpA03.mon9.rfm.data.1)
genderAndAge("A03 그룹 2",shpA03.mon9.rfm.data.2)
genderAndAge("A03 그룹 3",shpA03.mon9.rfm.data.3)
genderAndAge("A03 그룹 4",shpA03.mon9.rfm.data.4)

genderAndAge("A04 그룹 1",shpA04.mon9.rfm.data.1)
genderAndAge("A04 그룹 2",shpA04.mon9.rfm.data.2)
genderAndAge("A04 그룹 3",shpA04.mon9.rfm.data.3)
genderAndAge("A04 그룹 4",shpA04.mon9.rfm.data.4)
