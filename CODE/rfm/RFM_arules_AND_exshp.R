
library(arulesViz)

#install.packages("dummies")
library(dummies)


createSectorTransaction <- function(sector){
  gc()
  sector <- sector[, c("ID","PD_M_NM", "DE_DT")]
  sector <- na.omit(sector)
  sector <-  cbind(sector[,c("ID","DE_DT")],
                    dummy(sector$PD_M_NM, sep = ","))
  cols <- c()
  for( i in colnames(sector)){
    if((i != "ID") & (i != "DE_DT")){
      i <- strsplit(i, ",")
      i <- i[[1]][2]
    }
    cols <- c(cols,i)
  }
  colnames(sector) <- cols  
  
  sector <- aggregate(. ~ ID + DE_DT, 
                       data = sector,
                       FUN = sum)
  
  sector <- sector[, -c(1, 2)] #ID  DATE 제거
  
  sector <- sector[rowSums(sector) > 0,]  
  sector[sector > 1] <- 1 
  sector <- as(as.matrix(sector), "transactions")
  return(sector)
}





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





#분석 예제
x <- sector
x <- createTransaction(A05, shpA05.rec1.freq1.mone2)
class(x)
summary(x)
inspect(x[1:10])
itemFrequency(x, type = 'absolute')
itemFrequencyPlot(x, support = 0.05, main = "item frequency plot above support 10%")

# shp 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = entire
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      #assign(paste(name, ".preference", sep = ""), matrixForTFIDF(entire, eval(parse(text = name))))
      save(list = name, file = paste(name, ".preference.Rda", sep = ""))
      #rm(list = paste(name, ".transaction", sep = ""))
    }
  }
}
# A01 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = A01
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(paste(name, ".transaction", sep = ""), createTransaction(A01, eval(parse(text = name))))
    }
  }
}

# A02 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = A02
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(paste(name, ".transaction", sep = ""), createTransaction(A02, eval(parse(text = name))))
      name = A02
    }
  }
}

# A03 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = A03
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(paste(name, ".transaction", sep = ""), createTransaction(A03, eval(parse(text = name))))
    }
  }
}

# A04 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = A04
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(paste(name, ".transaction", sep = ""), createTransaction(A04, eval(parse(text = name))))
    }
  }
}

# A05 27개로 segment
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = A05
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      assign(paste(name, ".transaction", sep = ""), createTransaction(A05, eval(parse(text = name))))
    }
  }
}


#exShpInfo
rfmWithExShp <- function(rfmData){
  rfmData <- na.omit(merge(exShpInfo[, c("ID", "BIZ_UNIT")],
                   rfmData[, c(1,2)],
                   by = "ID",
                   all.x = T))
  rfmData <- rfmData[, c("ID", "BIZ_UNIT")]
  barplot(table(rfmData$BIZ_UNIT))
  return(rfmData)
}

#(B01:호텔, B02:여행사, B03:면세점,C01:영화관, C02:테마파크, C03:야구관람, D01:패스트푸드, D02:패밀리레스토랑, D03:카페)

# margin 문제 해결
par("mar")
par(mar = c(1, 1, 1, 1))

#table()
#barplot(,names = c("B01", "B02", "B03", "C01", "C02", "c03", "D01", "D02", "D03"))
