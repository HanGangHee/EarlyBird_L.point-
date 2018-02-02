
###############
##SCATTERPLOT3D

SHP_EXSHP <-data.frame(BEST_SHP[1],BEST_EXSHP[1])

x<-names(table(BEST_SHP[1]))
y<-names(table(BEST_EXSHP[1]))

SHP_EXSHP <-table(SHP_EXSHP)
z <-as.integer(SHP_EXSHP)
#B01 쫙 B02 쫙 
xx<-factor(rep(x,9), levels=x)   # OK
yy<-factor(rep(y,each=148), levels=y)

library(scatterplot3d)
scatterplot3d(xx,yy,z,x.ticklabs = c("",x,""),y.ticklabs = c("",y,""),type="h")


#############################
###DATA LOAD#################

load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_Demo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_EXShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_ShpInfo.Rda")
load(file="C:/Users/Administrator/Desktop/원희/대학교/Lpoint공모전/data/RAW_ProdInfo.Rda")


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

RAW_ShpInfo$ID = as.numeric(RAW_ShpInfo$ID)   # ID가 character 라서 numeric으로 바꿈

TBA = 0  # TOTAL BUY AMOUNT
TBA[1:nrow(RAW_Demo)] = 0  # 20000개의 0

for (i in 1:nrow(RAW_Demo)) # RAW_ShpInfo로부터 TBA 구하는 반복문
{
  temp = filter(RAW_ShpInfo,ID == i)
  temp_total = sum(temp$BUY_AM)
  
  TBA[i] = temp_total  
  
}

RAW_Demo$TBA = TBA   # DEMO에 TBA column 추가

RAW_Demo = RAW_Demo[with(RAW_Demo, order(-TBA)),]  # DEMO를 TBA가 큰 순서로 정렬

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
####틀만들기############################

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

PD_M = data.frame(names(table(total_shp$PD_M_NM)))
PD_M$FREQ = 0
names(PD_M) <- c('Var1','FREQ')

PD_M = data.frame(names(table(total_shp$PD_M_NM)))
PD_M$FREQ = 0
names(PD_M) <- c('Var1','FREQ')



ID_TBA = data.frame(TBA1_DEMO$ID,TBA2_DEMO$ID,TBA3_DEMO$ID,TBA4_DEMO$ID,TBA5_DEMO$ID,TBA6_DEMO$ID,TBA7_DEMO$ID,TBA8_DEMO$ID,TBA9_DEMO$ID,TBA10_DEMO$ID)
# TBA로 나눈 10등급의 ID

#TBA1_shpInfo = filter(total_shp,ID == as.character(ID_TBA$TBA1_DEMO.ID)) 이거 왜 안되지;;

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



