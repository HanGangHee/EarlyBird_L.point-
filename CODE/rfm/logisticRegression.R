
load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/RAW_demo.RDa")
str(RAW_Demo)
RAW_Demo$ID <- as.factor(RAW_Demo$ID)
RAW_Demo$GENDER <- factor(RAW_Demo$GENDER, levels = c(1, 2), labels = c("남자", "여자"))
RAW_Demo$AGE_PRD <- as.factor(RAW_Demo$AGE_PRD)
RAW_Demo <- RAW_Demo[, c(1, 2, 3)]

shpA01.mon9 <- shpA01[shpA01$DE_DT - as.Date("2015-09-30") < 0,]자
shpA01.mon3 <- shpA01[shpA01$DE_DT - as.Date("2015-09-30") > 0,]
length(unique(shpA01.mon9$ID))
length(unique(shpA01.mon3$ID))



shpA02.mon9 <- shpA02[shpA02$DE_DT - as.Date("2015-09-30") < 0,]
shpA02.mon3 <- shpA02[shpA02$DE_DT - as.Date("2015-09-30") > 0,]
length(unique(shpA02.mon9$ID))
length(unique(shpA02.mon3$ID))


shpA03.mon9 <- shpA03[shpA03$DE_DT - as.Date("2015-09-30") < 0,]
shpA03.mon3 <- shpA03[shpA03$DE_DT - as.Date("2015-09-30") > 0,]
length(unique(shpA03.mon9$ID))
length(unique(shpA03.mon3$ID))


shpA04.mon9 <- shpA04[shpA04$DE_DT - as.Date("2015-09-30") < 0,]
shpA04.mon3 <- shpA04[shpA04$DE_DT - as.Date("2015-09-30") > 0,]
length(unique(shpA04.mon9$ID))
length(unique(shpA04.mon3$ID))


shpA05.mon9 <- shpA05[shpA05$DE_DT - as.Date("2015-09-30") < 0,]
shpA05.mon3 <- shpA05[shpA05$DE_DT - as.Date("2015-09-30") > 0,]
length(unique(shpA05.mon9$ID))
length(unique(shpA05.mon3$ID))


# 업종별로 이용하는 고객의 수 

#  recency frequency manetary  모두 3개 기준으로 segmentation 진해
shpA02.mon9.rfm <- rfm_auto(shpA02.mon9, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA03.mon9.rfm <- rfm_auto(shpA03.mon9, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA04.mon9.rfm <- rfm_auto(shpA04.mon9, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA05.mon9.rfm <- rfm_auto(shpA05.mon9, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))

shpA02.mon9.rfm$breaks


shpA02.mon9.rfm.data <- shpA02.mon9.rfm$rfm
shpA03.mon9.rfm.data <- shpA03.mon9.rfm$rfm
shpA04.mon9.rfm.data <- shpA04.mon9.rfm$rfm
shpA05.mon9.rfm.data <- shpA05.mon9.rfm$rfm



logisticRegression <- function(month3, month9){
  month3 <- as.data.frame(table(month3$ID))
  colnames(month3) <- c("ID", "v")
  month3 <- month3[month3$v != 0, ]
  month3$v <- 1
  
  
  
  month9 <- merge(month9,month3,by = "ID", all.x = T)
  
  month9[(is.na(month9))] <- 0
  month9 <- month9[,c(5,6,7,8)]
  month9$v <- as.factor(month9$v)
  model <- glm(v ~ RecencyClass + FrequencyClass + MonetaryClass,
               data = month9,
               family = "binomial")
  return(model)
}



shoA02.model <- logisticRegression(shpA02.mon3, shpA02.mon9.rfm.data)
shoA03.model <- logisticRegression(shpA03.mon3, shpA03.mon9.rfm.data)
shoA04.model <- logisticRegression(shpA04.mon3, shpA04.mon9.rfm.data)
shoA05.model <- logisticRegression(shpA05.mon3, shpA05.mon9.rfm.data)



summary(shoA02.model)
summary(shoA03.model)
summary(shoA04.model)
summary(shoA05.model)



shpA02.mon9.rfm.data$rfm <- shpA02.mon9.rfm.data$RecencyClass*0.68 + shpA02.mon9.rfm.data$FrequencyClass*0.83 + shpA02.mon9.rfm.data$MonetaryClass*0.59 
shpA03.mon9.rfm.data$rfm <- shpA03.mon9.rfm.data$RecencyClass*0.99 + shpA03.mon9.rfm.data$FrequencyClass*0.96 + shpA03.mon9.rfm.data$MonetaryClass*0.47
shpA04.mon9.rfm.data$rfm <- shpA04.mon9.rfm.data$RecencyClass*0.78 + shpA04.mon9.rfm.data$FrequencyClass*0.85 + shpA04.mon9.rfm.data$MonetaryClass*0.43
summary(shpA02.mon9.rfm.data$rfm)
summary(shpA03.mon9.rfm.data$rfm)
summary(shpA04.mon9.rfm.data$rfm)

A02.q <- length(shpA02.mon9.rfm.data$ID)/4
A03.q <- length(shpA03.mon9.rfm.data$ID)/4
A04.q <- length(shpA04.mon9.rfm.data$ID)/4 
A04.q <- 1296
shpA02.mon9.rfm.data <- shpA02.mon9.rfm.data[order(shpA02.mon9.rfm.data$rfm),]
shpA03.mon9.rfm.data <- shpA03.mon9.rfm.data[order(shpA03.mon9.rfm.data$rfm),]
shpA04.mon9.rfm.data <- shpA04.mon9.rfm.data[order(shpA04.mon9.rfm.data$rfm),]

shpA02.mon9.rfm.data.1 <- shpA02.mon9.rfm.data[1:A02.q, ]
#쇼핑외 업종
x <- rfmWithExShp(shpA02.mon9.rfm.data.1)

shpA02.mon9.rfm.data.1 <- inner_join(shpA02.mon9.rfm.data.1,
                                     RAW_Demo,
                                     by = "ID")

