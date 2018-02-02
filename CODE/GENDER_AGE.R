#############################
###DATA LOAD#################

load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_Demo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_EXShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_ShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_ProdInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/total_shp.Rda")


#############################
###PACKAGE LOAD##############

library(dplyr)


#############################
##GENDER, AGEPRD Distribution


DEMO_M = filter(RAW_Demo,GENDER=='1')
DEMO_W = filter(RAW_Demo,GENDER=='2')

DEMO_M20 = filter(DEMO_M,AGE_PRD=='20PRD')
DEMO_M30 = filter(DEMO_M,AGE_PRD=='30PRD')
DEMO_M40 = filter(DEMO_M,AGE_PRD=='40PRD')
DEMO_M50 = filter(DEMO_M,AGE_PRD=='50PRD')
DEMO_M60 = filter(DEMO_M,AGE_PRD=='60PRD')

DEMO_W20 = filter(DEMO_W,AGE_PRD=='20PRD')
DEMO_W30 = filter(DEMO_W,AGE_PRD=='30PRD')
DEMO_W40 = filter(DEMO_W,AGE_PRD=='40PRD')
DEMO_W50 = filter(DEMO_W,AGE_PRD=='50PRD')
DEMO_W60 = filter(DEMO_W,AGE_PRD=='60PRD')

########################################
####틀 만들기############################

PD_M = data.frame(names(table(total_shp$PD_M_NM)))
PD_M$FREQ = 0
names(PD_M) <- c('Var1','FREQ')

PD_M2 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M2$FREQ = 0
names(PD_M2) <- c('Var1','FREQ')

PD_M3 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M3$FREQ = 0
names(PD_M3) <- c('Var1','FREQ')

PD_M4 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M4$FREQ = 0
names(PD_M4) <- c('Var1','FREQ')

PD_M5 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M5$FREQ = 0
names(PD_M5) <- c('Var1','FREQ')

PD_M6 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M6$FREQ = 0
names(PD_M6) <- c('Var1','FREQ')

PD_M7 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M7$FREQ = 0
names(PD_M7) <- c('Var1','FREQ')

PD_M8 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M8$FREQ = 0
names(PD_M8) <- c('Var1','FREQ')

PD_M9 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M9$FREQ = 0
names(PD_M9) <- c('Var1','FREQ')

PD_M10 = data.frame(names(table(total_shp$PD_M_NM)))
PD_M10$FREQ = 0
names(PD_M10) <- c('Var1','FREQ')


#DEMO_M20$ID,DEMO_M30$ID,DEMO_M40$ID,DEMO_M50$ID,DEMO_M60$ID,DEMO_W20$ID,DEMO_W30$ID,DEMO_W40$ID,DEMO_W50$ID,DEMO_W60$ID

ID_M20 = as.character(DEMO_M20$ID)
ID_M30 = as.character(DEMO_M30$ID)
ID_M40 = as.character(DEMO_M40$ID)
ID_M50 = as.character(DEMO_M50$ID)
ID_M60 = as.character(DEMO_M60$ID)
ID_W20 = as.character(DEMO_W20$ID)
ID_W30 = as.character(DEMO_W30$ID)
ID_W40 = as.character(DEMO_W40$ID)
ID_W50 = as.character(DEMO_W50$ID)
ID_W60 = as.character(DEMO_W60$ID)

for(i in 1:length(ID_M20))
{
  temp = filter(total_shp,ID == ID_M20[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M$FREQ = PD_M$FREQ + tt$Freq
  }
}

for(i in 1:length(ID_M30))
{
  temp = filter(total_shp,ID == ID_M30[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M2,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M2$FREQ = PD_M2$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_M40))
{
  temp = filter(total_shp,ID == ID_M40[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M3,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M3$FREQ = PD_M3$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_M50))
{
  temp = filter(total_shp,ID == ID_M50[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M4,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M4$FREQ = PD_M4$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_M60))
{
  temp = filter(total_shp,ID == ID_M60[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M5,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M5$FREQ = PD_M5$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_W20))
{
  temp = filter(total_shp,ID == ID_W20[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M6,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M6$FREQ = PD_M6$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_W30))
{
  temp = filter(total_shp,ID == ID_W30[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M7,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M7$FREQ = PD_M7$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_W40))
{
  temp = filter(total_shp,ID == ID_W40[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M8,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M8$FREQ = PD_M8$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_W50))
{
  temp = filter(total_shp,ID == ID_W50[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M9,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M9$FREQ = PD_M9$FREQ + tt$Freq
  }
}
for(i in 1:length(ID_W60))
{
  temp = filter(total_shp,ID == ID_W60[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M10,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M10$FREQ = PD_M10$FREQ + tt$Freq
  }
}


PD_M = PD_M[with(PD_M, order(-FREQ)),]  # PD_M을 FREQ 큰 순으로 정렬
PD_M2 = PD_M2[with(PD_M2, order(-FREQ)),]  # PD_M2을 FREQ 큰 순으로 정렬
PD_M3 = PD_M3[with(PD_M3, order(-FREQ)),]  # PD_M3을 FREQ 큰 순으로 정렬
PD_M4 = PD_M4[with(PD_M4, order(-FREQ)),]  # PD_M4을 FREQ 큰 순으로 정렬
PD_M5 = PD_M5[with(PD_M5, order(-FREQ)),]  # PD_M5을 FREQ 큰 순으로 정렬
PD_M6 = PD_M6[with(PD_M6, order(-FREQ)),]  # PD_M6을 FREQ 큰 순으로 정렬
PD_M7 = PD_M7[with(PD_M7, order(-FREQ)),]  # PD_M7을 FREQ 큰 순으로 정렬
PD_M8 = PD_M8[with(PD_M8, order(-FREQ)),]  # PD_M8을 FREQ 큰 순으로 정렬
PD_M9 = PD_M9[with(PD_M9, order(-FREQ)),]  # PD_M9을 FREQ 큰 순으로 정렬
PD_M10 = PD_M10[with(PD_M10, order(-FREQ)),]  # PD_M10을 FREQ 큰 순으로 정렬

TOP20_PDM = head(PD_M,20)
TOP20_PDM2 = head(PD_M2,20)
TOP20_PDM3 = head(PD_M3,20)
TOP20_PDM4 = head(PD_M4,20)
TOP20_PDM5 = head(PD_M5,20)
TOP20_PDM6 = head(PD_M6,20)
TOP20_PDM7 = head(PD_M7,20)
TOP20_PDM8 = head(PD_M8,20)
TOP20_PDM9 = head(PD_M9,20)
TOP20_PDM10 = head(PD_M10,20)

barplot(TOP20_PDM$FREQ,names=TOP20_PDM$Var1)
barplot(TOP20_PDM2$FREQ,names=TOP20_PDM2$Var1)
barplot(TOP20_PDM3$FREQ,names=TOP20_PDM3$Var1)
barplot(TOP20_PDM4$FREQ,names=TOP20_PDM4$Var1)
barplot(TOP20_PDM5$FREQ,names=TOP20_PDM5$Var1)
barplot(TOP20_PDM6$FREQ,names=TOP20_PDM6$Var1)
barplot(TOP20_PDM7$FREQ,names=TOP20_PDM7$Var1)
barplot(TOP20_PDM8$FREQ,names=TOP20_PDM8$Var1)
barplot(TOP20_PDM9$FREQ,names=TOP20_PDM9$Var1)
barplot(TOP20_PDM10$FREQ,names=TOP20_PDM10$Var1)
