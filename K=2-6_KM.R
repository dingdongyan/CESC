rm(list=ls())
setwd('C:/Users/CQU_ddy/Desktop/data/ddy20201022-COXPH/4_K=2-6_KM/')

library(e1071)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(survminer)
library(survcomp)

clinic2020_CESC<-read.csv("55COXPH_Diff_CESC_KM_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
#clinic2020_CESC<-read.csv("TOP20COXPH_Diff_CESC_KM_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
#clinic2020_CESC<-read.csv("TOP30COXPH_Diff_CESC_KM_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据

clinic2020_CESC<-t(clinic2020_CESC)
clinic2020_CESC<-as.data.frame(clinic2020_CESC)
patient.vital_status<-rep(1,length(clinic2020_CESC[,1]))#先令所有生存状态都为1
#View(head(clinic2020_CESC))
patient.vital_status[which(is.na(clinic2020_CESC$patient.days_to_death))]=0#将失访的生存状态标记为0

clinic2020_CESC$patient.vital_status<-patient.vital_status#给列表新增生存状态列
clinic2020_CESC$patient.days_to_last_followup[is.na(clinic2020_CESC$patient.days_to_last_followup)]<-0#最后访问天数为NA的将其最后访问天数改为0
clinic2020_CESC$patient.days_to_death[is.na(clinic2020_CESC$patient.days_to_death)]<-0#死亡天数为NA的将其死亡天数改为0
clinic2020_CESC$time<-clinic2020_CESC$patient.days_to_last_followup+clinic2020_CESC$patient.days_to_death#新增列表time列,其值为死亡天数和随访天数的和
#删除掉原来列表中的 patient.days_to_last_followup2020 和 patient.days_to_death 列
clinic2020_CESC<-clinic2020_CESC[,c(-which(names(clinic2020_CESC)=="patient.days_to_last_followup"),-which(names(clinic2020_CESC)=="patient.days_to_death"))]
#View(head(clinic2020_CESC))
#######################数据准备完毕
#######################k-means
clinic2020_CESC_kmeans<-clinic2020_CESC[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]

###K=2
set.seed(1)
clinic2020_CESC_kmeans_2<-kmeans(clinic2020_CESC_kmeans,center=2)
#View(head(clinic2020_CESC_kmeans_2))
kmeansgroup2<-clinic2020_CESC_kmeans_2$cluster #分类贴标签
#View(head(kmeansgroup2)) 分类贴标签
clinic2020_CESC_2<-clinic2020_CESC
#View(head(clinic2020_CESC_2))
clinic2020_CESC_2$kmeansgroup2<-kmeansgroup2
fit2020_CESC_2 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup2, data = clinic2020_CESC_2)
jpeg(file = "CESC_2means.jpg")
ggsurvplot(fit2020_CESC_2 , data =clinic2020_CESC_2, pval=TRUE)
dev.off()

####K=3
set.seed(1)
clinic2020_CESC_kmeans_3<-kmeans(clinic2020_CESC_kmeans,center=3)
kmeansgroup3<-clinic2020_CESC_kmeans_3$cluster
clinic2020_CESC_3<-clinic2020_CESC
clinic2020_CESC_3$kmeansgroup3<-kmeansgroup3
fit2020_CESC_3 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup3, data = clinic2020_CESC_3)
jpeg(file = "CESC_3means.jpg")
getwd()
ggsurvplot(fit2020_CESC_3 , data =clinic2020_CESC_3, pval=TRUE)
dev.off()

##K=4
set.seed(1)
clinic2020_CESC_kmeans_4<-kmeans(clinic2020_CESC_kmeans,center=4)
kmeansgroup4<-clinic2020_CESC_kmeans_4$cluster
clinic2020_CESC_4<-clinic2020_CESC
clinic2020_CESC_4$kmeansgroup4<-kmeansgroup4
fit2020_CESC_4 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup4, data = clinic2020_CESC_4)
jpeg(file = "CESC_4means.jpg")
ggsurvplot(fit2020_CESC_4 , data =clinic2020_CESC_4, pval=TRUE)
dev.off()

########K=5
set.seed(1)
clinic2020_CESC_kmeans_5<-kmeans(clinic2020_CESC_kmeans,center=5)
kmeansgroup5<-clinic2020_CESC_kmeans_5$cluster
clinic2020_CESC_5<-clinic2020_CESC
clinic2020_CESC_5$kmeansgroup5<-kmeansgroup5
fit2020_CESC_5 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup5, data = clinic2020_CESC_5)
jpeg(file = "CESC_5means.jpg")
ggsurvplot(fit2020_CESC_5 , data =clinic2020_CESC_5, pval=TRUE)
dev.off()

###K=6
set.seed(1)
clinic2020_CESC_kmeans_6<-kmeans(clinic2020_CESC_kmeans,center=6)
kmeansgroup6<-clinic2020_CESC_kmeans_6$cluster
clinic2020_CESC_6<-clinic2020_CESC
clinic2020_CESC_6$kmeansgroup6<-kmeansgroup6
fit2020_CESC_6 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup6, data = clinic2020_CESC_6)
jpeg(file = "CESC_6means.jpg")
ggsurvplot(fit2020_CESC_6 , data =clinic2020_CESC_6, pval=TRUE)
dev.off()
