rm(list=ls())
setwd("C:/Users/CQU_ddy/Desktop/data/ddy20201022-COXPH/2_307个原发癌临床信息筛选")

#读取与标准化后CESC数据表顺序对应的样本编号，（从原标准化后CESC数据提取出来，删掉-01等代表原发癌/转移癌的尾号）
df_name<-read.csv("CESC_normalized_Sample_ID.csv", header=TRUE,row.names= 1, na.strings=c("NA"))
dim(df_name)
df_name<-t(df_name)#若原始Sample_ID以列作为存储,读取ID时-不会读取成"."
df_name<-as.data.frame(df_name)
colname_df_name<-tolower(colnames(df_name))

#读取临床数据，列名为样本编号（字母小写）
df_clinic<-read.csv("CESC.clin.merged.csv", header=T,row.names= 1, na.strings=c("NA"))
df_clinic<-as.data.frame(df_clinic)
colname_df_clinic<-colnames(df_clinic)
dim(df_clinic)
#length(colname_df_clinic)
newdata <- list()
m <- list()
colname_newdata <- list()
#data <- df_clinic[,which(colnames(df_clinic) %in% colname_df_name)]
#dim(data)
for(i in c(1:length(colname_df_name))){
  b <- colname_df_name[i]
  if(b %in% colname_df_clinic){
    newdata[[b]] <- df_clinic[,colname_df_name[i]] 
    colname_newdata[[b]] <- colname_df_name[i]
  }else{
    m[[b]] <- colname_df_name[i]
  }
}
length(m)#在临床信息中，未找到对应样本的临床信息个数
write.table(newdata, "./result/与标准化后CESC数据信息对应的临床信息20201022.txt", col.names = colname_newdata, row.names = rownames(df_clinic), quote=F, sep = "\t")
write.table(m, "./result/NotFind_Sample_ID_20201022.txt", col.names = F, row.names = F, quote=F, sep = "\t")

########################################## t挑选出有用的临床信息 ##########################################################
df_clinic<-read.csv("./汇总临床信息/与标准化后CESC数据信息对应的临床信息20201022.csv", header=T,row.names= 1, na.strings=c("NA"))
df_clinic<-t(df_clinic)
dim(df_clinic)
df_clinic<-as.data.frame(df_clinic)
colname_df_clinic<-colnames(df_clinic)
#rowname_df_clinic<-rownames(df_clinic)
Aim_data<-list()
m_aim<-list()
#Aim_rowname <- list()
Aim_colname <- list()
n1_name<-c('patient.age_at_initial_pathologic_diagnosis',
           'patient.age_began_smoking_in_years',
           'patient.cervical_neoplasm_pathologic_margin_involved_text',
           'patient.cervical_neoplasm_pathologic_margin_involved_type',
           'patient.days_to_death', 
           'patient.days_to_last_followup',
           'patient.gender',
           'patient.height',
           'patient.histological_type',
           'patient.initial_pathologic_diagnosis_method',
           'patient.race_list.race',
           'patient.stage_event.clinical_stage',
           'patient.tumor_tissue_site',
           'patient.hpv_test_results.hpv_test_result.hpv_call_1',
           'patient.hpv_test_results.hpv_test_result.hpv_call_2',
           'patient.hpv_test_results.hpv_test_result.hpv_call_3',
           'patient.hpv_test_results.hpv_test_result.hpv_status',
           'patient.omfs.omf.other_malignancy_anatomic_site',
           'patient.omfs.omf.other_malignancy_anatomic_site_text',
           'patient.omfs.omf.other_malignancy_histological_type',
           'patient.omfs.omf.other_malignancy_histological_type_text')
#patient.days_to_last_followup patient.days_to_death patient.vital_status
#n1_name<-c('patient.days_to_last_followup', 'patient.days_to_death', 'patient.vital_status')
length(n1_name)
for(i in n1_name){
  if(i %in% colname_df_clinic){
    Aim_data[[i]]<-df_clinic[,i]
    Aim_colname[[i]]<-i
  }else{
    m_aim[[i]]<-i
  }
}
length(m_aim)#在临床信息中，未找到对应样本的临床信息个数
#View(head(Aim_data))
#write.table(Aim_data, "C:/Users/CQU_ddy/Desktop/ddy_20201022临床信息20201016.txt", col.names = Aim_colname, row.names = rownames(df_clinic), quote=F, sep = "\t")
write.table(Aim_data, "./汇总临床信息/result/筛选出有用的临床信息20201022.txt", col.names = Aim_colname, row.names = rownames(df_clinic), quote=F, sep = "\t")
write.table(m_aim, "./汇总临床信息/result/NotFind_clinic_20201022.txt", col.names = F, row.names = F, quote=F, sep = "\t")

########################################## 临床信息筛选 ##########################################################
df_clinic_useful<-read.csv("./汇总临床信息/筛选出有用的临床信息20201022.csv", header=T,row.names= 1, na.strings=c("NA"))
dim(df_clinic_useful)
### 1.patient.age_at_initial_pathologic_diagnosis（初诊年龄，小于等于35年，35-60岁之间，大于60岁）
f<-df_clinic_useful$patient.age_at_initial_pathologic_diagnosis
a_na<-sum(is.na(f))
a_na
f2<-f[which(!is.na(f))]
a1<-sum(f2<=35)
a2<-sum(f2>35 & f2<=60)
a3<-sum(f2>60)
n<-sum(c(a1, a2, a3))
if(a_na>0){
  n<- n-a_na
}
n
mydata <- data.frame(age_range=c('age<=35','35<age<=60', 'age>60', "age is na"), count=c(a1, a2, a3, a_na), age_ratio=c(a1/n, a2/n, a3/n, 0/n))
mydata

### 2.patient.age_began_smoking_in_years (开始吸烟的年龄，30岁以下，30岁以上，NA)
f<-df_clinic_useful$patient.age_began_smoking_in_years
b_na<-sum(is.na(f))
b_na
f2<-f[which(!is.na(f))]
b1<-sum(f2<=30)
b2<-sum(f2>30)
c(b1, b2, b_na)
n<-sum(c(b1, b2, b_na))
if(b_na>0){
  n<- n-b_na
}
n
mydata2 <- data.frame(began_smoking_range=c('year<=30','year>30', 'data is NA'), count=c(b1, b2, b_na), began_smoking_ratio=c(b1/n, b2/n, 0/n))
mydata2

### 3.patient.cervical_neoplasm_pathologic_margin_involved_text （统计各种的数目）
f<-df_clinic_useful$patient.cervical_neoplasm_pathologic_margin_involved_text
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 4.patient.cervical_neoplasm_pathologic_margin_involved_type（统计各种的数目）
f<-df_clinic_useful$patient.cervical_neoplasm_pathologic_margin_involved_type
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 5.patient.days_to_death（小于等于3年，大于3年，NA）
f<-df_clinic_useful$patient.days_to_death #以天数存储而不是年
f
b_na<-sum(is.na(f))
b_na
f2<-f[which(!is.na(f))]
b1<-sum(f2<=3*365)
b2<-sum(f2>3*365)
c(b1, b2, b_na)
n<-sum(c(b1, b2, b_na))
if(b_na>0){
  n<- n-b_na
}
n
mydata2 <- data.frame(days_to_death_range=c('year<=3','year>3', 'data is NA'), count=c(b1, b2, b_na), days_to_death_ratio=c(b1/n, b2/n, 0/n))
mydata2

### 6.patient.days_to_last_followup （配合days_to_death完成患者生存率的统计）
f<-df_clinic_useful$patient.days_to_last_followup
f
b_na<-sum(is.na(f))
b_na
f2<-f[which(!is.na(f))]
b1<-sum(f2<=3*365)
b2<-sum(f2>3*365)
c(b1, b2, b_na)
n<-sum(c(b1, b2, b_na))
if(b_na>0){
  n<- n-b_na
}
n
mydata2 <- data.frame(days_to_death_range=c('year<=3','year>3', 'data is NA'), count=c(b1, b2, b_na), days_to_death_ratio=c(b1/n, b2/n, 0/n))
mydata2

### 7.patient.gender（统计各种情况人数）
f<-df_clinic_useful$patient.gender
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 8.patient.height（统计各种情况人数，以10cm为组距）
f<-df_clinic_useful$patient.height
f
f_na<-sum(is.na(f))
f_na
f2<-f[which(!is.na(f))]
max(f2)#身高最大值 177
min(f2)#身高最小值 132
num_range <- max(f2)-min(f2)#身高属性极差 45 故以10为组距可分为5组
labels <- c("< 140", "140 - 150", "150 - 160", "160 - 170", ">= 170")#区间名称
breaks <- c(130, 140, 150, 160, 170, 300)#断点
mytable <- cut(f2, breaks = breaks, labels = labels, right = TRUE )
df <- as.data.frame(table(height=mytable))
df
sum(table(height=mytable))
f_na #缺失值个数

### 9.patient.histological_type（统计各种情况参数）
f<-df_clinic_useful$patient.histological_type
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 10.patient.initial_pathologic_diagnosis_method （统计各种情况人数）
f<-df_clinic_useful$patient.initial_pathologic_diagnosis_method
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 11.patient.race_list.race （统计各种情况人数）
f<-df_clinic_useful$patient.race_list.race
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 12.patient.stage_event.clinical_stage （统计各种情况人数）
f<-df_clinic_useful$patient.stage_event.clinical_stage
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 13.patient.tumor_tissue_site （统计各种情况人数）
f<-df_clinic_useful$patient.tumor_tissue_site
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 14.patient.hpv_test_results.hpv_test_result.hpv_call_1（统计各种情况人数）
f<-df_clinic_useful$patient.hpv_test_results.hpv_test_result.hpv_call_1
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 15.patient.hpv_test_results.hpv_test_result.hpv_call_2（统计各种情况人数）
f<-df_clinic_useful$patient.hpv_test_results.hpv_test_result.hpv_call_2
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 16.patient.hpv_test_results.hpv_test_result.hpv_call_3（统计各种情况人数）
f<-df_clinic_useful$patient.hpv_test_results.hpv_test_result.hpv_call_3
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 17.patient.hpv_test_results.hpv_test_result.hpv_status（统计各种情况人数）
f<-df_clinic_useful$patient.hpv_test_results.hpv_test_result.hpv_status
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 18.patient.omfs.omf.other_malignancy_anatomic_site（统计各种情况人数）
f<-df_clinic_useful$patient.omfs.omf.other_malignancy_anatomic_site
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 19.patient.omfs.omf.other_malignancy_anatomic_site_text（统计各种情况人数）
f<-df_clinic_useful$patient.omfs.omf.other_malignancy_anatomic_site_text
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 20.patient.omfs.omf.other_malignancy_histological_type（统计各种情况人数）
f<-df_clinic_useful$patient.omfs.omf.other_malignancy_histological_type
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数

### 21.patient.omfs.omf.other_malignancy_histological_type_text（统计各种情况人数）
f<-df_clinic_useful$patient.omfs.omf.other_malignancy_histological_type_text
f
f1<-unique(f)#值的种类总共有哪几种
#View(head(table(f)))
as.data.frame(table(f))
sum(table(f))
sum(is.na(f))#缺失值个数


