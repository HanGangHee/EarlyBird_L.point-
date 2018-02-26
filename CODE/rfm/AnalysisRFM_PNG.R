getwd()
setwd("C:/Users/gksrkdgml/EarlyBird_L.point-/OUTPUT/shp")
#exShpInfo
rfmWithExShp <- function(name, rfmData){
  rfmData <- na.omit(merge(exShpInfo[, c("ID", "BIZ_UNIT")],
                           rfmData[, c(1,2)],
                           by = "ID",
                           all.x = T))
  rfmData <- rfmData[, c("ID", "BIZ_UNIT")]
  
  png(paste(name, '.png', sep = ""))
  barplot(table(rfmData$BIZ_UNIT), main = paste(name, "쇼핑외 업종 이용 정보"))
  dev.off()
}


rfmWithShp <- function(name, rfmData){
  rfmData <- na.omit(merge(shpInfo[, c("ID", "BIZ_UNIT")],
                           rfmData[, c(1,2)],
                           by = "ID",
                           all.x = T))
  rfmData <- rfmData[, c("ID", "BIZ_UNIT")]
  
  png(paste(name, '.png', sep = ""))
  barplot(table(rfmData$BIZ_UNIT), main = paste(name, "쇼핑 업종 이용 정보"))
  dev.off()
}

for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = entire
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      rfmWithExShp(name, eval(parse(text = name)))
    }
  }
}
for(i in 1:3){
  for(j in 1:3){
    for(k in 1:3){
      name = entire
      name <- paste(name, paste(".rec",i, sep = ""), sep = "")
      name<- paste(name, paste(".freq",j, sep = ""), sep = "")
      name <- paste(name, paste(".mone",k, sep = ""), sep = "")
      rfmWithShp(name, eval(parse(text = name)))
    }
  }
}
