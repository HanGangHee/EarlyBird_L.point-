#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/total_shp.RDa")
#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/BEST_SHP.RDa")
library(dplyr)
#install.packages("devtools")
library(easyRFM)
devtools::install_github("hoxo-m/easyRFM")
#(A01:백화점, A02:대형마트, A03:슈퍼마켓, A04:편의점, A05:드러그스토어)
str(shpInfo)
shpInfo$ID <- as.factor(shpInfo$ID)
shpInfo$DE_DT <- as.character(shpInfo$DE_DT)
shpInfo$DE_DT <- as.Date(shpInfo$DE_DT, format = "%Y%m%d")
shpInfo.A01 <- shpInfo[shpInfo$BIZ_UNIT == 'A01', ]
shpInfo.A02 <- shpInfo[shpInfo$BIZ_UNIT == 'A02', ]
shpInfo.A03 <- shpInfo[shpInfo$BIZ_UNIT == 'A03', ]
shpInfo.A04 <- shpInfo[shpInfo$BIZ_UNIT == 'A04', ]
shpInfo.A05 <- shpInfo[shpInfo$BIZ_UNIT == 'A05', ]

shpInfo.A01.rfm <- shpInfo.A01[, c('ID', 'DE_DT', 'BUY_AM')] 
shpInfo.A02.rfm <- shpInfo.A02[, c('ID', 'DE_DT', 'BUY_AM')] 
shpInfo.A03.rfm <- shpInfo.A03[, c('ID', 'DE_DT', 'BUY_AM')] 
shpInfo.A04.rfm <- shpInfo.A04[, c('ID', 'DE_DT', 'BUY_AM')] 
shpInfo.A05.rfm <- shpInfo.A05[, c('ID', 'DE_DT', 'BUY_AM')] 

length(unique(shpInfo.A01.rfm$ID))
length(unique(shpInfo.A02.rfm$ID))
length(unique(shpInfo.A03.rfm$ID))
length(unique(shpInfo.A04.rfm$ID))
length(unique(shpInfo.A05.rfm$ID))


#5점?  3점? 
shpInfo.A01.rfm <- shpInfo.A01 %>%
                        group_by(ID) %>%
                          summarise(recency = max(as.numeric(DE_DT)) - as.numeric(as.Date("2014-12-29" , format = "%Y-%m-%d")),
                                    frequenci = length(ID), monitery = sum(BUY_AM)/length(ID))
summary(shpInfo.A01.rfm)



shpInfo.A02.rfm <- shpInfo.A02 %>%
  group_by(ID) %>%
  summarise(recency = max(as.numeric(DE_DT)) - as.numeric(as.Date("2014-12-29" , format = "%Y-%m-%d")),
            frequenci = length(ID), monitery = sum(BUY_AM)/length(ID))
summary(shpInfo.A02.rfm)

shpInfo.A03.rfm <- shpInfo.A03 %>%
  group_by(ID) %>%
  summarise(recency = max(as.numeric(DE_DT)) - as.numeric(as.Date("2014-12-29" , format = "%Y-%m-%d")),
            frequenci = length(ID), monitery = sum(BUY_AM)/length(ID))
summary(shpInfo.A02.rfm)


shpInfo.A04.rfm <- shpInfo.A04 %>%
  group_by(ID) %>%
  summarise(recency = max(as.numeric(DE_DT)) - as.numeric(as.Date("2014-12-29" , format = "%Y-%m-%d")),
            frequenci = length(ID), monitery = sum(BUY_AM)/length(ID))
summary(shpInfo.A04.rfm)


shpInfo.A05.rfm <- shpInfo.A05 %>%
  group_by(ID) %>%
  summarise(recency = max(as.numeric(DE_DT)) - as.numeric(as.Date("2014-12-29" , format = "%Y-%m-%d")),
            frequenci = length(ID), monitery = sum(BUY_AM)/length(ID))
summary(shpInfo.A05.rfm)




shpInfo.A01.rfm <- rfm_auto(shpInfo.A01, payment = "BUY_AM", date = "DE_DT", id = "ID")
str(shpInfo.A01.rfm$rfm)
shpInfo.A01.rfm$breaks
shpInfo.A01.rfm$classes
shpInfo.A01.rfm$get_table()
shpInfo.A01.rfm$get_sliced_rfm()






