rm(list=ls())     # ???? ????????

rm('bp1','bp2','bp3','bpt','bp2','data1','data2','data3','data4')

RAW_Demo<-read.table("C://Users//Administrator//Desktop//????//???б?//Lpoint??????//data//Demo.txt",header=TRUE,sep=",",colClasses=c('character'))
RAW_ShpInfo<-read.table("C://Users//Administrator//Desktop//????//???б?//Lpoint??????//data//???ξ?��??ǰ????��??.txt",header=TRUE,sep=",",colClasses=c('character','character','character','character','character','character','character','integer','integer'))
RAW_EXShpInfo<-read.table("C://Users//Administrator//Desktop//????//???б?//Lpoint??????//data//???οܾ?��?̿?��??.txt",header=TRUE,sep=",",colClasses=c('character','character','character','integer','integer'))
RAW_ProdInfo<-read.table("C://Users//Administrator//Desktop//????//???б?//Lpoint??????//data//???ξ?��??ǰ?з?��??.txt",header=TRUE,sep=",",colClasses=c('character'))


###########
PDH <- unique(select(RAW_ProdInfo,'PD_H_NM'))
for (i in 1:nrow(PDH))
  PDH$number[i] <- i
names(PDH) <- c('BESTSHP','NUMSHP')


testt <- merge(testt,PDH,by='BESTSHP',all=TRUE)

EXSHP<-data.frame(c('B01','B02','B03','C01','C02','C03','D01','D02','D03'),c(1,2,3,4,5,6,7,8,9))
names(EXSHP)<-c('BESTEXSHP','EXSHPNUM')

testt<-merge(DEMO,EXSHP,by='BESTEXSHP',all=TRUE)

AGE<-data.frame(c('20PRD','30PRD','40PRD','50PRD','60PRD'),c(20,30,40,50,60))
names(AGE)<-c('AGE_PRD','AGENUM')

temp<-testtt[with(testtt,order(testtt$ID)),]
clean<-temp[1:20000,]

clean<-merge(clean,AGE,by='AGE_PRD',all=TRUE)

beforecluster<-select(clean,c('ID','GENDER','SHPSUM','EXSHPSUM','EXSHPNUM','NUMSHP','AGENUM'))
beforecluster$GENDER <- as.integer(beforecluster$GENDER)
beforecluster$ID<-as.integer(beforecluster$ID)

num<-matrix(beforecluster)
#########

sort(testtt,'ID')

total_shp<-merge(RAW_ShpInfo,RAW_ProdInfo,c('BIZ_UNIT','PD_S_C'))
total_shp<-merge(total_shp,RAW_Demo,'ID')

DATA_ID <- subset(RAW_Demo,select = 'ID')

for (j in 1:nrow(DATA_ID))
{
  samp_EXSHP<-subset(RAW_EXShpInfo,ID == DATA_ID[j,1])    # RAW_EXShpInfo???? ID?? 05072?? DATA?? ?̾Ƴ???
  
  #  samp_EXSHP<-samp_EXSHP[order(-samp_EXSHP$U_AM),]    # samp_EXSHP?? U_AM�� ????��?? sorting.
  
  samp_SHP<-subset(RAW_ShpInfo,ID == DATA_ID[j,1])     #RAW_ShpInfo ???? ID?? 05072?? DATA?? ?̱? 
  #  samp_SHP<-samp_SHP[order(-samp_SHP$BUY_AM),]
  
  #samp_BIZUNT<-table(samp_SHP$BIZ_UNIT)        # 05072?? ???? ??��?? ???? (05072?? ??????Ʈ?? ??ȭ���� ???ְ???.)
  
  if (nrow(samp_SHP) != 0 && nrow(samp_EXSHP) != 0)
  { 
    temp<-merge(samp_SHP,RAW_ProdInfo,c('BIZ_UNIT','PD_S_C'))
    
    NAME_PD <- data.frame(names(table(temp$PD_H_NM)))
    #BEST_SHP
    for (i in 1:nrow(NAME_PD))
    {
      temp1<-subset(temp, PD_H_NM == NAME_PD$names.table.temp.PD_H_NM..[i])[,8]
      sum_temp1 <- sum(temp1)
      
      NAME_PD$SUM[i] <- sum_temp1
    }
    
    NAME_PD<-NAME_PD[order(-NAME_PD$SUM),]    # NAME_PD?? SUM�� ????��?? sorting. 
    
    if(j==1)
    {
      BEST_SHP<-NAME_PD[1,]
      BEST_SHP$names.table.temp.PD_H_NM.. = factor(BEST_SHP$names.table.temp.PD_H_NM,levels=names(table(total_shp$PD_H_NM)))
    } 
    else  {
      BEST_SHP[j,]<-NAME_PD[1,]
    }
    
    # samp_EXSHP???? ???? ??�� ??�� ?????? BIZ_UNIT ???? 
    
    NAME_EXSHP <- data.frame(names(table(samp_EXSHP$BIZ_UNIT)))
    
    for (i in 1:nrow(NAME_EXSHP))
    {
      temp2<-subset(samp_EXSHP, BIZ_UNIT == NAME_EXSHP$names.table.samp_EXSHP.BIZ_UNIT..[i])[,4]
      sum_temp2 <- sum(temp2)
      
      NAME_EXSHP$SUM[i] <- sum_temp2
    }
  
    NAME_EXSHP<-NAME_EXSHP[order(-NAME_EXSHP$SUM),]    # NAME_EXSHP?? SUM�� ????��?? sorting. 
    ######
    if(j==1)
    {
      BEST_EXSHP<-NAME_EXSHP[1,]
      BEST_EXSHP$names.table.samp_EXSHP.BIZ_UNIT.. = factor(BEST_EXSHP$names.table.samp_EXSHP.BIZ_UNIT..,levels=names(table(RAW_EXShpInfo$BIZ_UNIT)))
    } 
    else  {
      BEST_EXSHP[j,]<-NAME_EXSHP[1,]
    }
  }
}
############

names(BEST_SHP)<-c('BESTSHP','SHPSUM')
names(BEST_EXSHP)<-c('BESTEXSHP','EXSHPSUM')

DEMO<-data.frame(RAW_Demo,BEST_SHP,BEST_EXSHP)

############# CLUSTERING

install.packages("NbClust")
library(NbClust)

nc <- NbClust(beforecluster, min.nc=2, max.nc=6, method="kmeans")
par(mfrow=c(1,1))
barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen")



install.packages("caret")
library(caret)

install.packages("cluster")
library(cluster)


beforecluster<-na.omit(beforecluster)
kmean.beforecluster<-kmeans(beforecluster,4,nstart=10)
round(sum(kmean.beforecluster$withinss),2) 

install.packages("fpc")        # ??Ű?? ?ν???
install.packages("mclust")

library(fpc)                            # ?ν????? ??Ű???? ?ҷ??��?
library(mclust)

pamk.result <- pamk(iris2)        # pamk?Լ??? ?????͸? ?־ ?з??ϰ? ?????? pamk.result ?????? ?????ϱ?

# number of clusters               

pamk.result$nc              



##########scatterplot3d
SHP_EXSHP <-data.frame(BEST_SHP[1],BEST_EXSHP[1])

x<-names(table(BEST_SHP[1]))
y<-names(table(BEST_EXSHP[1]))

SHP_EXSHP <-table(SHP_EXSHP)
z <-as.integer(SHP_EXSHP)
#B01 ?? B02 ?? 
xx<-factor(rep(x,9), levels=x)   # OK
yy<-factor(rep(y,each=148), levels=y)

library(scatterplot3d)
scatterplot3d(xx,yy,z,x.ticklabs = c("",x,""),y.ticklabs = c("",y,""),type="h")

#��?? ??��?? ?????Ƿ??? B03



###############################################????

library(dplyr)

#data1 <- read.table(file.choose(), header=TRUE, sep=",")
#data2 <- read.table(file.choose(), header=TRUE, sep=",")
#data3 <- read.table(file.choose(), header=TRUE, sep=",")
#data4 <- read.table(file.choose(), header=TRUE, sep=",")
data1<-RAW_Demo
data2<-RAW_ShpInfo
data3<-RAW_EXShpInfo
data4<-RAW_ProdInfo


# ?? ???? ??��?? ???? ???? / ????, 


bp1 <- aggregate(data3$U_AM~data3$ID, data3, sum) #???? ??�� ?????? ?Ѿ?
bp2 <- aggregate(data2$BUY_AM~data2$ID, data2, sum) #???? ?? ??�� ?????? ?Ѿ?
colnames(bp1) <- c("ID","BUY_AM")
colnames(bp2) <- c("ID","BUY_AM")

#bptable => ?��� ?? ???ξ?��, ???? ?? ??��, ??ü / ?? ??
bptable <- merge(data1, bp1, by="ID", all="TRUE")               
bptable <- merge(bptable, bp2, by="ID", all="TRUE")
bptable <- rename(bptable, SHOP_AM=BUY_AM.x , NOSHOP_AM = BUY_AM.y)   #???? 
bptable[is.na(bptable$SHOP_AM), "SHOP_AM"] = 0
bptable[is.na(bptable$NOSHOP_AM), "NOSHOP_AM"] = 0                      
bptable$TOTAL_AM <- bptable$SHOP_AM+bptable$NOSHOP_AM       


sum(bptable$TOTAL_AM==0)    #???? ?????Ͱ? ???? 105???? ?????? ��??


bpt <- subset(bptable, select =c("ID","TOTAL_AM"))    #??ü ?? ?? ??�� ?? ��??
bpt <- bpt[c(order(bpt$TOTAL_AM)),]

plot(bpt$TOTAL_AM) # ??ü ???ŷ? ????  => ?ķ??? ??Ģ ��??
bpt2 <- bpt[-c(20000),]    # 0��??
bpt2 <- bpt2[-c(1:105),]    # ?̻?ġ ��??
plot(bpt2$TOTAL_AM) # ?̻?ġ ��?? ?? ???? => ??�� 10% ũ?? ?ٸ? => ?? ?????? ?м? ?ʿ? 

head(bpt2)
tail(bpt2)

sp_r <- c(0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 0.91, 0.92, 0.93, 0.94, 0.95, 0.96, 0.97, 0.98, 0.99, 0.995, 1)  #seperate ratio
nsp_r <- quantile(bpt2$TOTAL_AM, sp_r) #?????? ??��?? ??
nsp_r <- data.frame(nsp_r)
nsp_r <- nsp_r$nsp_r

sb1 <- subset(bpt, subset=(bpt$TOTAL_AM)>=11093848)   # ??�� 10%
sum(sb1$TOTAL_AM/10000)/sum(bpt$TOTAL_AM/10000)  # ??�� 10%?? ??ü ???? ???? ???? 54%
sb2 <- subset(bpt, subset=(bpt$TOTAL_AM)>=6534938)   # ??�� 20%
sum(sb2$TOTAL_AM/10000)/sum(bpt$TOTAL_AM/10000)  # ??�� 20%?? ??ü ???? ???? ???? 70%   ?ķ?????Ģ ????��??



bptable$gn <- ifelse(bptable$TOTAL_AM > nsp_r[19],1,    #group number?? ?з?(??�� 0.5%, 1??, 2??, .... 10??, 20??, 30??, ... ??�� 10?۱???
                     ifelse(bptable$TOTAL_AM > nsp_r[18],2,
                            ifelse(bptable$TOTAL_AM > nsp_r[17],3,
                                   ifelse(bptable$TOTAL_AM > nsp_r[16],4,
                                          ifelse(bptable$TOTAL_AM > nsp_r[15],5,
                                                 ifelse(bptable$TOTAL_AM > nsp_r[14],6,
                                                        ifelse(bptable$TOTAL_AM > nsp_r[13],7,
                                                               ifelse(bptable$TOTAL_AM > nsp_r[12],8,
                                                                      ifelse(bptable$TOTAL_AM > nsp_r[11],9,
                                                                             ifelse(bptable$TOTAL_AM > nsp_r[10],10,
                                                                                    ifelse(bptable$TOTAL_AM > nsp_r[9],11,
                                                                                           ifelse(bptable$TOTAL_AM > nsp_r[8],12,
                                                                                                  ifelse(bptable$TOTAL_AM > nsp_r[7],13,
                                                                                                         ifelse(bptable$TOTAL_AM > nsp_r[6],14,
                                                                                                                ifelse(bptable$TOTAL_AM > nsp_r[5],15,
                                                                                                                       ifelse(bptable$TOTAL_AM > nsp_r[4],16,
                                                                                                                              ifelse(bptable$TOTAL_AM > nsp_r[3],17,
                                                                                                                                     ifelse(bptable$TOTAL_AM > nsp_r[2],18,
                                                                                                                                            ifelse(bptable$TOTAL_AM > nsp_r[1],19,20)))))))))))))))))))

table(bptable$gn)

bptable$grp <- ifelse(bptable$TOTAL_AM == 0, 4,               #???? ?? : 4
                      ifelse(bptable$NOSHOP_AM == 0,1,        #only shop : 1
                             ifelse(bptable$SHOP_AM == 0, 2 ,3)))  # only noshop : 2, shop&noshop : 3

compshop <- filter(bptable, bptable$grp ==3 )  # ???? ??
compshop$rate <- compshop$SHOP_AM/(compshop$TOTAL_AM)  # ??ü ?? shop?? ??��
compshop <- subset(compshop, select=c("ID","rate"))


bptable1 <- merge(bptable, compshop, by="ID", all="TRUE")   # ??�� ??ģ,  ???????? ??????��?? 


lbadata <- merge(data2,data4, c("BIZ_UNIT","PD_S_C"))
lbadata2 <- merge(bptable1, lbadata,by="ID", all="TRUE")


lbadata2$daytype <- as.integer(as.Date(as.character(lbadata2$DE_DT), "%Y%m%d"))%%7   # ???? => Date Ÿ??��??
#��???콺 ???? ?̿? 7?? ????, 4->?????? 0-> ?????? 1-> ?ݿ???

lbadata2$timetype <- ifelse(lbadata2$DE_HR <=5, 1, ifelse(lbadata2$DE_HR <= 11, 2, ifelse(lbadata2$DE_HR <= 17, 3, 4)))   
#?ð? ????(6~11) ??(12~17) ????(18~23) ????(0~5)
table(lbadata2$timetype)      #üũ




# ?ؾ??? ?? : ?????? ?̱?, bp ??ġ??,  test1 merge?ؼ?  ��?? bp?κ?!




####################????

test <-select(total_shp,'ID')
