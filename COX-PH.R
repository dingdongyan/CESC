###单变量回归分析(Cox-PH,log-rank P<0.05)，筛选与生存相关的miRNA表达feature###
rm(list = ls())
setwd('C:/Users/CQU_ddy/Desktop/data/ddy20201022-COXPH/3_COX-PH/')

#加载要用到的包
library("survival")
#library("survminer")
###更新包
#remove.packages("tibble")
#install.packages("tibble")
#读取数据 status->1代表删失事件(此处代表生存状态为alive) 2代表生存状态为dead
dat1 <- read.csv("307_CESC_normalized.csv", header=T,row.names= 1, na.strings=c("NA"))
dim(dat1)
#查看已读取的数据 patient.days_to_last_followup patient.days_to_death patient.vital_status
#head(dat1)
#View(head(dat1))
status<-rep(1,length(dat1[,1]))#先令所有生存状态都为1
status[which(dat1$patient.vital_status=="dead")]=2#将生存状态为dead标记为2，alive和NA都属于删失数据仍为1
dat1$status<-status#给列表新增生存状态列
dat1$patient.days_to_last_followup[is.na(dat1$patient.days_to_last_followup)]<-0#最后访问天数为NA的将其最后访问天数改为0
dat1$patient.days_to_death[is.na(dat1$patient.days_to_death)]<-0#死亡天数为NA的将其死亡天数改为0
dat1$time<-dat1$patient.days_to_last_followup+dat1$patient.days_to_death#新增列表time列,其值为死亡天数和随访天数的和
#删除掉原来列表中的 patient.days_to_last_followup2020 和 patient.days_to_death 列,patient.follow_ups.follow_up.vital_status列
dat1<-dat1[,c(-which(names(dat1)=="patient.days_to_last_followup"),-which(names(dat1)=="patient.days_to_death"),-which(names(dat1)=="patient.vital_status"))]
#View(head(dat1))
fix(dat1)

#进行单变量的Cox回归分析
#res.cox <- coxph(Surv(dat1$time, dat1$status) ~ dat1$hsa.let.7e, data = dat1)
#GSum <- summary(res.cox)

#封装定义回归分析函数
UniCox <- function(x){
  FML <- as.formula(paste0('Surv(dat1$time, dat1$status) ~ ', x))
  res.cox <- coxph(FML, data = dat1)
  GSum <- summary(res.cox)
  #HR风险比
  HR <- round(GSum$coefficients[, 2], 2)
  #P值
  PValue <- round(GSum$coefficients[, 5], 3)
  #95%的置信区间，上下界
  CI <- paste0(round(GSum$conf.int[, 3:4], 2), collapse = "-")
  #将上述三个整合成一个一行三列的数据框
  Unicox <- data.frame('Characteristics' = x,
                       'Hazard Ratio' = HR,
                       'CI95' = CI,
                       'P Value' = PValue)
  #返回数据框
  return (Unicox)
}

#获取数据的列名(数据里包含的所有MiRNA的名称)
VarNames <- colnames(dat1)[c(2:length(dat1[1, ])-2)]
VarNames
length(VarNames)
#应用lapply()函数，缓解内存加快运算速度
univar_analysis <- lapply(VarNames, UniCox)

#筛选出差异表达的feature(存在列表diff_list里)，以及差异表达feature总个数(用变量n来记录)
n <- 0
diff_list <- list()
P_value<-list()
for(i in VarNames){
  a <- UniCox(i)
  P_value[[i]]<-a$P.Value
  if(a$P.Value < 0.05){
    n <- n + 1
    diff_list[n] <- as.character(a$Characteristics)
  } 
}
n #与生存相关差异表达miRNA总个数
P_value

#保存处理结果
#保存差异表达的MiRNA
write.table(P_value, "All_P_value_CESC-COX-PH.txt", col.names = VarNames, row.names = F, quote = F, sep = "\t")
write.table(diff_list, "CESC-COX-diff_list.txt", col.names = F, row.names = F, quote = F, sep = "\t")

###################################### 筛选差异表达的miRNA信息 ##################################
miRNA_name <- read.table('new_CESC-COX-diff_list.txt', head=TRUE, sep = '\t', row.names = 1, check.names = F, quote = "")
miRNA_name<-as.data.frame(miRNA_name)
dim(miRNA_name)
diff_miRNA_name<-colnames(miRNA_name)
colname_dat1<-colnames(dat1)
length(colname_dat1)

newdata <- list()
m <- list()
colname_newdata <- list()
#data <- df_clinic[,which(colnames(df_clinic) %in% diff_miRNA_name)]
#dim(data)
for(i in c(1:length(diff_miRNA_name))){
  b <- diff_miRNA_name[i]
  if(b %in% colname_dat1){
    newdata[[b]] <- dat1[,diff_miRNA_name[i]] 
    colname_newdata[[b]] <- diff_miRNA_name[i]
  }else{
    m[[b]] <- diff_miRNA_name[i]
  }
}
length(m)#在临床信息中，未找到对应miRNA个数
write.table(newdata, "./result/CESC_diff_miRNA_20201022.txt", col.names = colname_newdata, row.names = rownames(dat1), quote=F, sep = "\t")
write.table(m, "./result/NotFind_20201022.txt", col.names = F, row.names = F, quote=F, sep = "\t")
