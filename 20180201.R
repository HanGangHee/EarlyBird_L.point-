
###############
##SCATTERPLOT3D

SHP_EXSHP <-data.frame(BEST_SHP[1],BEST_EXSHP[1])

x<-names(table(BEST_SHP[1]))
y<-names(table(BEST_EXSHP[1]))

SHP_EXSHP <-table(SHP_EXSHP)
z <-as.integer(SHP_EXSHP)
#B01 쭉 B02 쭉
xx<-factor(rep(x,9), levels=x)   # OK
yy<-factor(rep(y,each=148), levels=y)

library(scatterplot3d)
scatterplot3d(xx,yy,z,x.ticklabs = c("",x,""),y.ticklabs = c("",y,""),type="h")


#############################
###DATA LOAD#################

load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_Demo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_EXShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/data/RAW_ShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/data/RAW_ProdInfo.Rda")


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
DEMO_w30 = filter(DEMO_W,AGE_PRD=='30PRD')
DEMO_W40 = filter(DEMO_W,AGE_PRD=='40PRD')
DEMO_W50 = filter(DEMO_W,AGE_PRD=='50PRD')
DEMO_W60 = filter(DEMO_W,AGE_PRD=='60PRD')




######################################
######TOTAL BUY AMOUNT################

RAW_ShpInfo$ID = as.numeric(RAW_ShpInfo$ID)   # ID가 character라서 numeric으로 변환
TBA = 0  # TOTAL BUY AMOUNT
TBA[1:nrow(RAW_Demo)] = 0  # 20000개짜리 0

for (i in 1:nrow(RAW_Demo)) # RAW_ShpInfo로부터 TBA 도출
{
  temp = filter(RAW_ShpInfo,ID == i)
  temp_total = sum(temp$BUY_AM)
  
  TBA[i] = temp_total  
  
}

RAW_Demo$TBA = TBA   # DEMO에TBA column 추가

RAW_Demo = RAW_Demo[with(RAW_Demo, order(-TBA)),]  # DEMO를 TBA 큰 순으로 정렬

TBA1_DEMO = RAW_Demo[1:2000,]
TBA2_DEMO = RAW_Demo[2001:4000,]
TBA3_DEMO = RAW_Demo[4001:6000,]
TBA4_DEMO = RAW_Demo[6001:8000,]
TBA5_DEMO = RAW_Demo[8001:10000,]
TBA6_DEMO = RAW_Demo[10001:12000,]
TBA7_DEMO = RAW_Demo[12001:14000,]
TBA8_DEMO = RAW_Demo[14001:16000,]
TBA9_DEMO = RAW_Demo[16001:18000,]
TBA10_DEMO = RAW_Demo[18001:20000,]

#######################################
#######PD_M_MN #########################

load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/total_shp.Rda")

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



ID_TBA = data.frame(TBA1_DEMO$ID,TBA2_DEMO$ID,TBA3_DEMO$ID,TBA4_DEMO$ID,TBA5_DEMO$ID,TBA6_DEMO$ID,TBA7_DEMO$ID,TBA8_DEMO$ID,TBA9_DEMO$ID,TBA10_DEMO$ID)
# TBA로 10등급 아이디 분류

#TBA1_shpInfo = filter(total_shp,ID == as.character(ID_TBA$TBA1_DEMO.ID)) 왜안되는건지 모르겠음

ID1 = as.character(ID_TBA$TBA1_DEMO.ID)
ID2 = as.character(ID_TBA$TBA2_DEMO.ID)
ID3 = as.character(ID_TBA$TBA3_DEMO.ID)
ID4 = as.character(ID_TBA$TBA4_DEMO.ID)
ID5 = as.character(ID_TBA$TBA5_DEMO.ID)
ID6 = as.character(ID_TBA$TBA6_DEMO.ID)
ID7 = as.character(ID_TBA$TBA7_DEMO.ID)
ID8 = as.character(ID_TBA$TBA8_DEMO.ID)
ID9 = as.character(ID_TBA$TBA9_DEMO.ID)
ID10 = as.character(ID_TBA$TBA10_DEMO.ID)


#############################################

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID1[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M$FREQ = PD_M$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID2[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M2$FREQ = PD_M2$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID3[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M3$FREQ = PD_M3$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID4[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M4$FREQ = PD_M4$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID5[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M5$FREQ = PD_M5$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID6[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M6$FREQ = PD_M6$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID7[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M7$FREQ = PD_M7$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID8[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M8$FREQ = PD_M8$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID9[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
    tt$Freq[is.na(tt$Freq)] <- 0
    PD_M9$FREQ = PD_M9$FREQ + tt$Freq
  }
}

for(i in 1:2000)
{
  temp = filter(total_shp,ID == ID10[i])
  t = data.frame(table(temp$PD_M_NM))
  if(nrow(t)!=0)
  {
    tt = merge(PD_M,t,by='Var1',all=TRUE)
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


