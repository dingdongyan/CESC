rm(list=ls())
setwd('C:/Users/CQU_ddy/Desktop/data')

library(e1071)
library(lattice)
library(survival)
library(Formula)
library(ggplot2)
library(Hmisc)
library(ggpubr)
library(survminer)
library(survcomp)
library(pROC)

clinic2020_CESC<-read.csv("TOP20-CESC-miRNA-20201022.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic2020_CESC<-as.data.frame(clinic2020_CESC)
patient.vital_status<-rep(1,length(clinic2020_CESC[,1]))#先令所有生存状态都为1
patient.vital_status[which(is.na(clinic2020_CESC$patient.days_to_death))]=0#将失访的生存状态标记为0

clinic2020_CESC$patient.vital_status<-patient.vital_status#给列表新增生存状态列
clinic2020_CESC$patient.days_to_last_followup[is.na(clinic2020_CESC$patient.days_to_last_followup)]<-0#最后访问天数为NA的将其最后访问天数改为0
clinic2020_CESC$patient.days_to_death[is.na(clinic2020_CESC$patient.days_to_death)]<-0#死亡天数为NA的将其死亡天数改为0
clinic2020_CESC$time<-clinic2020_CESC$patient.days_to_last_followup+clinic2020_CESC$patient.days_to_death#新增列表time列,其值为死亡天数和随访天数的和
#删除掉原来列表中的 patient.days_to_last_followup2020 和 patient.days_to_death 列
clinic2020_CESC<-clinic2020_CESC[,c(-which(names(clinic2020_CESC)=="patient.days_to_last_followup"),-which(names(clinic2020_CESC)=="patient.days_to_death"))]
#######################数据准备完毕
#######################k-means
clinic2020_CESC_kmeans<-clinic2020_CESC[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]

###K=2
set.seed(1)
clinic2020_CESC_kmeans_2<-kmeans(clinic2020_CESC_kmeans,center=2)
kmeansgroup2<-clinic2020_CESC_kmeans_2$cluster #分类贴标签
clinic2020_CESC_2<-clinic2020_CESC
#fix(clinic2020_CESC_2)
clinic2020_CESC_2$kmeansgroup2<-kmeansgroup2
fit2020_CESC_2 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup2, data = clinic2020_CESC_2)
jpeg(file = "CESC_2means.jpg")
ggsurvplot(fit2020_CESC_2, pval=TRUE, data =clinic2020_CESC_2)
dev.off()

######SVM 2
set.seed(1)
#############SVM 
index_2020_CESC<-c(1:nrow(clinic2020_CESC))

testindex_2020_CESC<-sample(index_2020_CESC, trunc(length(index_2020_CESC)*0.3))##测试集
###2组
traindata_svm_2020_CESC_2<-clinic2020_CESC_2[-testindex_2020_CESC,]##重要特征及分组
#fix(traindata_svm_2020_CESC_2)
#colname_newdata<-colnames(traindata_svm_2020_CESC_2)
#write.table(traindata_svm_2020_CESC_2, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-TrianSet-data.txt", col.names = colname_newdata, row.names = rownames(traindata_svm_2020_CESC_2), quote=F, sep = "\t")

testdata_svm_2020_CESC_2<-clinic2020_CESC_2[testindex_2020_CESC,]
#fix(testdata_svm_2020_CESC_2)
#colname_newdata<-colnames(testdata_svm_2020_CESC_2)
#write.table(testdata_svm_2020_CESC_2, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-TestSet-data.txt", col.names = colname_newdata, row.names = rownames(testdata_svm_2020_CESC_2), quote=F, sep = "\t")

###ddy
traindata_svm_2020_CESC_2_0<-traindata_svm_2020_CESC_2[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]
#fix(traindata_svm_2020_CESC_2_0)
testdata_svm_2020_CESC_2_0<-testdata_svm_2020_CESC_2[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]
#fix(testdata_svm_2020_CESC_2_0)
###ddy
clinic2020_datasvm_CESC_model_2<-svm(kmeansgroup2~., data=traindata_svm_2020_CESC_2_0, type = 'C',kernel = 'radial' )
clinic2020_datasvm_CESC_pre_2<-predict(clinic2020_datasvm_CESC_model_2,testdata_svm_2020_CESC_2_0[,-which(names(testdata_svm_2020_CESC_2_0)=="kmeansgroup2")])
#write.table(clinic2020_datasvm_CESC_pre_2, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-TestSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_2<-table(testdata_svm_2020_CESC_2[,which(names(testdata_svm_2020_CESC_2)=="kmeansgroup2")],clinic2020_datasvm_CESC_pre_2,dnn=c("真实值","预测值"))

confusionmatrix_svm_2020_CESC_2##混淆矩阵
#########################ROC curve
clinic2020_datasvm_CESC_pre_2_nu<-as.numeric(clinic2020_datasvm_CESC_pre_2)

modelroc <- roc(testdata_svm_2020_CESC_2$kmeansgroup2,clinic2020_datasvm_CESC_pre_2_nu)

plot(modelroc,
     print.auc=TRUE, ##图像上输出AUC的值
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像
     #grid=c(0.1, 0.1), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.8, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

dev.off()
############################

##################### train set ########################
clinic2020_datasvm_CESC_pre_2_train<-predict(clinic2020_datasvm_CESC_model_2,traindata_svm_2020_CESC_2_0[,-which(names(traindata_svm_2020_CESC_2_0)=="kmeansgroup2")])
#write.table(clinic2020_datasvm_CESC_pre_2_train, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-TrainSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_2_train<-table(traindata_svm_2020_CESC_2[,which(names(traindata_svm_2020_CESC_2)=="kmeansgroup2")],clinic2020_datasvm_CESC_pre_2_train,dnn=c("真实值","预测值"))

confusionmatrix_svm_2020_CESC_2_train##混淆矩阵
#########################ROC curve
clinic2020_datasvm_CESC_pre_2_nu_train<-as.numeric(clinic2020_datasvm_CESC_pre_2_train)

modelroc_train <- roc(traindata_svm_2020_CESC_2$kmeansgroup2,clinic2020_datasvm_CESC_pre_2_nu_train)

plot(modelroc_train,
     print.auc=TRUE, ##图像上输出AUC的值
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像
     ##grid=c(0.1, 0.2), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.8, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

dev.off()

############################
################################### 307 All ####################################
#fix(traindata_svm_2020_CESC_2_0)
#fix(clinic2020_CESC_2)
#colname_newdata<-colnames(clinic2020_CESC_2)
#write.table(clinic2020_CESC_2, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-307AllSet-data.txt", col.names = colname_newdata, row.names = rownames(clinic2020_CESC_2), quote=F, sep = "\t")

clinic2020_CESC_2_new<-clinic2020_CESC_2
#fix(clinic2020_CESC_2_new)
clinic2020_CESC_2_Allpre<-clinic2020_CESC_2_new[,c(-which(names(clinic2020_CESC_2_new)=="patient.vital_status"),-which(names(clinic2020_CESC_2_new)=="time"))]
clinic2020_datasvm_CESC_pre_2_Allpre<-predict(clinic2020_datasvm_CESC_model_2,clinic2020_CESC_2_Allpre[,-which(names(clinic2020_CESC_2_Allpre)=="kmeansgroup2")])
#write.table(clinic2020_datasvm_CESC_pre_2_Allpre, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=2-307AllSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_2_Allpre<-table(clinic2020_CESC_2_new[,which(names(clinic2020_CESC_2_new)=="kmeansgroup2")],clinic2020_datasvm_CESC_pre_2_Allpre,dnn=c("真实值","预测值"))

confusionmatrix_svm_2020_CESC_2_Allpre##混淆矩阵
#########################ROC curve
clinic2020_datasvm_CESC_pre_2_nu_Allpre<-as.numeric(clinic2020_datasvm_CESC_pre_2_Allpre)

modelroc_2_Allpre <- roc(clinic2020_CESC_2_new$kmeansgroup2,clinic2020_datasvm_CESC_pre_2_nu_Allpre)

plot(modelroc_2_Allpre,
     print.auc=TRUE, ##图像上输出AUC的值
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像
     ##grid=c(0.1, 0.2), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.8, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

dev.off()

################################### K=2 307AllPre KM ######################

######## Kmeans 3组
set.seed(1)
clinic2020_CESC_kmeans_3<-kmeans(clinic2020_CESC_kmeans,center=3)
kmeansgroup3<-clinic2020_CESC_kmeans_3$cluster
clinic2020_CESC_3<-clinic2020_CESC
clinic2020_CESC_3$kmeansgroup3<-kmeansgroup3
fit2020_CESC_3 <- survfit(Surv(time, patient.vital_status) ~ kmeansgroup3, data = clinic2020_CESC_3)
jpeg(file = "CESC_3means.jpg")
ggsurvplot(fit2020_CESC_3, pval=TRUE, data =clinic2020_CESC_3)
dev.off()

###################SVM 3组
set.seed(1)

traindata_svm_2020_CESC_3<-clinic2020_CESC_3[-testindex_2020_CESC,]##重要特征及分组
#fix(traindata_svm_2020_CESC_3)
#colname_newdata<-colnames(traindata_svm_2020_CESC_3)
#write.table(traindata_svm_2020_CESC_3, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-TrianSet-data.txt", col.names = colname_newdata, row.names = rownames(traindata_svm_2020_CESC_3), quote=F, sep = "\t")

testdata_svm_2020_CESC_3<-clinic2020_CESC_3[testindex_2020_CESC,]
#fix(testdata_svm_2020_CESC_3)
#colname_newdata<-colnames(testdata_svm_2020_CESC_3)
#write.table(testdata_svm_2020_CESC_3, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-TestSet-data.txt", col.names = colname_newdata, row.names = rownames(testdata_svm_2020_CESC_3), quote=F, sep = "\t")

###ddy
traindata_svm_2020_CESC_3_0<-traindata_svm_2020_CESC_3[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]
#fix(traindata_svm_2020_CESC_3_0)
testdata_svm_2020_CESC_3_0<-testdata_svm_2020_CESC_3[,c(-which(names(clinic2020_CESC)=="patient.vital_status"),-which(names(clinic2020_CESC)=="time"))]
#fix(testdata_svm_2020_CESC_3_0)
###ddy
clinic2020_datasvm_CESC_model_3<-svm(kmeansgroup3~., data=traindata_svm_2020_CESC_3_0, type = 'C',kernel = 'radial' )
clinic2020_datasvm_CESC_pre_3<-predict(clinic2020_datasvm_CESC_model_3,testdata_svm_2020_CESC_3_0[,-which(names(testdata_svm_2020_CESC_3_0)=="kmeansgroup3")])
#write.table(clinic2020_datasvm_CESC_pre_3, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-TestSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_3<-table(testdata_svm_2020_CESC_3[,which(names(testdata_svm_2020_CESC_3)=="kmeansgroup3")],clinic2020_datasvm_CESC_pre_3,dnn=c("真实值","预测值"))
confusionmatrix_svm_2020_CESC_3

############################### K=3 Test ROC
setwd('C:/Users/CQU_ddy/Desktop/data/Evaluation_COXTOP20 K=2-K=3/K=3-DataSet/Test/')
Test_ROC_data_1<-read.csv("K=3-Group1-test-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Test1 <- roc(Test_ROC_data_1$kmeansgroup3,Test_ROC_data_1$Pre_group)

plot(modelroc_3_Test1,
     col="red", ##曲线颜色
     print.auc=TRUE, ##图像上输出AUC的值
     print.auc.x=0.4, print.auc.y=0.5, ##图像上输出AUC值,坐标为(x, y)
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像 
     #grid=c(0.1, 0.2), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     smooth=F, ##绘制不平滑曲线
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

Test_ROC_data_2<-read.csv("K=3-Group2-test-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Test2 <- roc(Test_ROC_data_2$kmeansgroup3,Test_ROC_data_2$Pre_group)
plot.roc(modelroc_3_Test2,
         add=T, ##增加曲线
         col="black", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.4, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

Test_ROC_data_3<-read.csv("K=3-Group3-test-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Test3 <- roc(Test_ROC_data_3$kmeansgroup3,Test_ROC_data_3$Pre_group)
plot.roc(modelroc_3_Test3,
         add=T, ##增加曲线
         col="green", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.3, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

legend(0.35,0.30, ##图例位置
       bty="n", ##图例样式,默认为“o”
       title="", ## 引号内添加图例标题
       legend=c("Group 1", "Group 2", "Group 3"), ##添加分组
       col=c("red", "black", "green"), ##颜色跟前面一致
       lwd=2) ##线条粗细

dev.off()

#################################### training set
clinic2020_datasvm_CESC_pre_3_train<-predict(clinic2020_datasvm_CESC_model_3,traindata_svm_2020_CESC_3_0[,-which(names(traindata_svm_2020_CESC_3_0)=="kmeansgroup3")])
#write.table(clinic2020_datasvm_CESC_pre_3_train, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-TrainSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_3_train<-table(traindata_svm_2020_CESC_3[,which(names(traindata_svm_2020_CESC_3)=="kmeansgroup3")],clinic2020_datasvm_CESC_pre_3_train,dnn=c("真实值","预测值"))

confusionmatrix_svm_2020_CESC_3_train##混淆矩阵

############################
############################### K=3 Train ROC
setwd('C:/Users/CQU_ddy/Desktop/data/Evaluation_COXTOP20 K=2-K=3/K=3-DataSet/Train/')
Train_ROC_data_1<-read.csv("K=3-Group1-train-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Train1 <- roc(Train_ROC_data_1$kmeansgroup3,Train_ROC_data_1$Pre_group)

plot(modelroc_3_Train1,
     col="red", ##曲线颜色
     print.auc=TRUE, ##图像上输出AUC的值
     print.auc.x=0.4, print.auc.y=0.5, ##图像上输出AUC值,坐标为(x, y)
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像 
     #grid=c(0.1, 0.2), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     smooth=F, ##绘制不平滑曲线
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

Train_ROC_data_2<-read.csv("K=3-Group2-train-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Train2 <- roc(Train_ROC_data_2$kmeansgroup3,Train_ROC_data_2$Pre_group)
plot.roc(modelroc_3_Train2,
         add=T, ##增加曲线
         col="black", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.4, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

Train_ROC_data_3<-read.csv("K=3-Group3-train-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_Train3 <- roc(Train_ROC_data_3$kmeansgroup3,Train_ROC_data_3$Pre_group)
plot.roc(modelroc_3_Train3,
         add=T, ##增加曲线
         col="green", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.3, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

legend(0.35,0.30, ##图例位置
       bty="n", ##图例样式,默认为“o”
       title="", ## 引号内添加图例标题
       legend=c("Group 1", "Group 2", "Group 3"), ##添加分组
       col=c("red", "black", "green"), ##颜色跟前面一致
       lwd=2) ##线条粗细

dev.off()

######################################### 307 ALL ###############################
################################### 307 All ####################################
#fix(clinic2020_CESC_3)
colname_newdata<-colnames(clinic2020_CESC_3)
#write.table(clinic2020_CESC_3, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-307AllSet-data.txt", col.names = colname_newdata, row.names = rownames(clinic2020_CESC_3), quote=F, sep = "\t")

clinic2020_CESC_3_new<-clinic2020_CESC_3
clinic2020_CESC_3_Allpre<-clinic2020_CESC_3_new[,c(-which(names(clinic2020_CESC_3_new)=="patient.vital_status"),-which(names(clinic2020_CESC_3_new)=="time"))]
clinic2020_datasvm_CESC_pre_3_Allpre<-predict(clinic2020_datasvm_CESC_model_3,clinic2020_CESC_3_Allpre[,-which(names(clinic2020_CESC_3_Allpre)=="kmeansgroup3")])
#write.table(clinic2020_datasvm_CESC_pre_3_Allpre, "C:/Users/CQU_ddy/Desktop/COXPHTOP20-K=3-307AllSet-pre.txt", quote=F, sep = "\t")
confusionmatrix_svm_2020_CESC_3_Allpre<-table(clinic2020_CESC_3_new[,which(names(clinic2020_CESC_3_new)=="kmeansgroup3")],clinic2020_datasvm_CESC_pre_3_Allpre,dnn=c("真实值","预测值"))

confusionmatrix_svm_2020_CESC_3_Allpre##混淆矩阵

###########################################
############################### K=3 307AllPre ROC
setwd('C:/Users/CQU_ddy/Desktop/data/Evaluation_COXTOP20 K=2-K=3/K=3-DataSet/307AllSet/')
AllSet_ROC_data_1<-read.csv("K=3-Group1-307All-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_AllSet1 <- roc(AllSet_ROC_data_1$kmeansgroup3,AllSet_ROC_data_1$Pre_group)

plot(modelroc_3_AllSet1,
     col="red", ##曲线颜色
     print.auc=TRUE, ##图像上输出AUC的值
     print.auc.x=0.4, print.auc.y=0.5, ##图像上输出AUC值,坐标为(x, y)
     auc.polygon=TRUE, ##设置AUC曲线下填充
     auc.polygon.col="white", ##设置AUC曲线下填充颜色
     max.auc.polygon=TRUE, ##填充整个图像 
     #grid=c(0.1, 0.2), ##绘制列线条间距为0.1, 行线条间距为0.2
     #grid.col=c("green", "red"), ##绘制列线条颜色为绿色, 行线条颜色为红色
     #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
     smooth=F, ##绘制不平滑曲线
     legacy.axes=TRUE)  ##使横轴从0到1，表示为1-特异度

AllSet_ROC_data_2<-read.csv("K=3-Group2-307All-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_AllSet2 <- roc(AllSet_ROC_data_2$kmeansgroup3,AllSet_ROC_data_2$Pre_group)
plot.roc(modelroc_3_AllSet2,
         add=T, ##增加曲线
         col="black", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.4, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

AllSet_ROC_data_3<-read.csv("K=3-Group3-307All-ROC-data.csv", header=T,row.names= 1, na.strings=c("NA"))
modelroc_3_AllSet3 <- roc(AllSet_ROC_data_3$kmeansgroup3,AllSet_ROC_data_3$Pre_group)
plot.roc(modelroc_3_AllSet3,
         add=T, ##增加曲线
         col="green", ##曲线颜色为红色
         #print.thres=TRUE, print.thres.cex=0.6, ##图像上输出最佳截断值,并使其字体缩放0.8倍
         print.auc=TRUE, ##图像上输出AUC的值
         print.auc.x=0.4, print.auc.y=0.3, ##图像上输出AUC值,坐标为(x, y)
         smooth=F)

legend(0.35,0.30, ##图例位置
       bty="n", ##图例样式,默认为“o”
       title="", ## 引号内添加图例标题
       legend=c("Group 1", "Group 2", "Group 3"), ##添加分组
       col=c("red", "black", "green"), ##颜色跟前面一致
       lwd=2) ##线条粗细

dev.off()




