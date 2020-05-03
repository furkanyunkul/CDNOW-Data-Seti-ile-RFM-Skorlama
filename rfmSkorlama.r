#alisveris en son
#son bir hafta alisveris 5
#bir hafta bir ay 4
#bir ay uc ay 3
#3 ay alti ay 2
#6 ay 1 yil 1

cdnow=read.table("CDNOW_sample.txt")

cdnow$V1=NULL #1 kolonu sildik

colnames(cdnow)=c("ID","Tarih","Adet","Fiyat")

str(cdnow)

cdnow$ID=as.factor(as.character(cdnow$ID))
cdnow$Tarih=as.Date(as.character(cdnow$Tarih),"%Y%m%d")
#Sys.Date() bugunun tarihi
#as.numeric(Sys.Date()-cdnow$Tarih[1]) datanin bugun ile farki
refDay=max(cdnow$Tarih)
as.numeric(refDay-cdnow$Tarih)

library(dplyr)


rfmRecency=cdnow %>% group_by(ID)%>%summarise(Recency=as.numeric(refDay)-as.numeric(max(Tarih))) 

rfmFrequency=cdnow%>%group_by(ID)%>%summarise(Frequency=n()) #siklik

rfmMonitary=cdnow%>%group_by(ID)%>%summarise(Monitary=sum(Fiyat))

#merge diyoruz
rfm=merge(rfmFrequency,rfmMonitary,by="ID")
rfm=merge(rfm,rfmRecency,by="ID")

#Toplam satisa gore puanlama
quantile(rfm$Monitary)
#0-20 20-45 45-105 105-1000 1000-6600 e gore bolduk
rankM=cut(rfm$Monitary,breaks=c(0,20,45,105,1000,6600))
levels(rankM)
#Araliklari puan yaprsak
levels(rankM)=c(1,2,3,4,5)
levels(rankM)
head(rankM)


#Tarih farkina gore puanlama
quantile(rfm$Recency)
#en son gunden farki 0-60 60-217 217-473 473-506 506-550 gun
rankR=cut(rfm$Recency,breaks=c(0,60,217,473,506,550))
levels(rankR)
#Araliklari puan yaprsak az olmasi burda daha oncelikli
levels(rankR)=c(5,4,3,2,1)

#sikliga gore puanlama
quantile(rfm$Frequency)
#siklik cinsinden 0,1,3,5,60 a gore bolduk
rankF=cut(rfm$Frequency,breaks = c(0,1,2,3,5,60))
levels(rankF)

#skorlama tablomuzu olusturduk
rfmScore=data.frame(cbind(rfm$ID,rankR,rankF,rankM))
