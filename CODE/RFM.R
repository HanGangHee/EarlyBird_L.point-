#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/total_shp.RDa")
#load("C:/Users/gksrkdgml/EarlyBird_L.point-/DATA/BEST_SHP.RDa")

library(dplyr)
#install.packages("devtools")
library(easyRFM)
#devtools::install_github("hoxo-m/easyRFM")

#(A01:백화점, A02:대형마트, A03:슈퍼마켓, A04:편의점, A05:드러그스토어)


shpInfo <- total_shp
str(shpInfo)
#shpInfo 전처리
shpInfo$ID <- as.factor(shpInfo$ID)
shpInfo$DE_DT <- as.character(shpInfo$DE_DT)
shpInfo$DE_DT <- as.Date(shpInfo$DE_DT, format = "%Y%m%d")

# 업종별로 데이터 분리
shpInfo.A01 <- shpInfo[shpInfo$BIZ_UNIT == 'A01', c('ID', 'DE_DT', 'BUY_AM')]
shpInfo.A02 <- shpInfo[shpInfo$BIZ_UNIT == 'A02', c('ID', 'DE_DT', 'BUY_AM')]
shpInfo.A03 <- shpInfo[shpInfo$BIZ_UNIT == 'A03', c('ID', 'DE_DT', 'BUY_AM')]
shpInfo.A04 <- shpInfo[shpInfo$BIZ_UNIT == 'A04', c('ID', 'DE_DT', 'BUY_AM')]
shpInfo.A05 <- shpInfo[shpInfo$BIZ_UNIT == 'A05', c('ID', 'DE_DT', 'BUY_AM')]


# 업종별로 이용하는 고객의 수 
length(unique(shpInfo.A01$ID))
length(unique(shpInfo.A02$ID))
length(unique(shpInfo.A03$ID))
length(unique(shpInfo.A04$ID))
length(unique(shpInfo.A05$ID))


shpA01.rfm <- rfm_auto(shpInfo.A01, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA02.rfm <- rfm_auto(shpInfo.A02, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA03.rfm <- rfm_auto(shpInfo.A03, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA04.rfm <- rfm_auto(shpInfo.A04, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))
shpA05.rfm <- rfm_auto(shpInfo.A05, payment = "BUY_AM", date = "DE_DT", id = "ID", breaks = list(r=3, f=3,m=3))



A01 <- "shpA01.rfm"
A02 <- "shpA02.rfm"
A03 <- "shpA03.rfm"
A04 <- "shpA04.rfm"
A05 <- "shpA05.rfm"

# A01 27개로 segment
for(i in 1:3){
  name = A01
  for(j in 1:3){
    for(k in 1:3){
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(name, shpA01.rfm$rfm[shpA01.rfm$rfm$RecencyClass == i &
                                         shpA01.rfm$rfm$FrequencyClass == j &
                                           shpA01.rfm$rfm$MonetaryClass == k, ])
    }
  }
}

# A02 27개로 segment
for(i in 1:3){
  name = A02
  for(j in 1:3){
    for(k in 1:3){
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(name, shpA02.rfm$rfm[shpA02.rfm$rfm$RecencyClass == i &
                                         shpA02.rfm$rfm$FrequencyClass == j &
                                         shpA02.rfm$rfm$MonetaryClass == k, ])
    }
  }
}

# A03 27개로 segment
for(i in 1:3){
  name = A03
  for(j in 1:3){
    for(k in 1:3){
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(name, shpA03.rfm$rfm[shpA03.rfm$rfm$RecencyClass == i &
                                         shpA03.rfm$rfm$FrequencyClass == j &
                                         shpA03.rfm$rfm$MonetaryClass == k, ])
    }
  }
}

# A04 27개로 segment
for(i in 1:3){
  name = A04
  for(j in 1:3){
    for(k in 1:3){
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(name, shpA04.rfm$rfm[shpA04.rfm$rfm$RecencyClass == i &
                                         shpA04.rfm$rfm$FrequencyClass == j &
                                         shpA04.rfm$rfm$MonetaryClass == k, ])
    }
  }
}

# A05 27개로 segment
for(i in 1:3){
  name = A05
  for(j in 1:3){
    for(k in 1:3){
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(name, shpInfo.A05.rfm$rfm[shpA05.rfm$rfm$RecencyClass == i &
                                         shpA05.rfm$rfm$FrequencyClass == j &
                                         shpA05.rfm$rfm$MonetaryClass == k, ])
    }
  }
}




