rm(list=ls())
setwd("C:/Users/CQU_ddy/Desktop/data/3year_115Sample_clinical")

############## 数据准备 307个样本中，与3年生存期相关的样本共计115个 #############
df_name<-read.csv("115_Sample_ID.csv", header=TRUE,row.names= 1, na.strings=c("NA"))
dim(df_name)
df_name<-t(df_name)#若原始Sample_ID以列作为存储,读取ID时-不会读取成"."
df_name<-as.data.frame(df_name)
colname_df_name<-tolower(colnames(df_name))

df_clinic<-read.csv("CESC_clinical_information.csv", header=T,row.names= 1, na.strings=c("NA"))
df_clinic<-as.data.frame(df_clinic)
df_clinic<-t(df_clinic)
colname_df_clinic<-colnames(df_clinic)
dim(df_clinic)
#View(head(colname_df_clinic))

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
write.table(newdata, "./All_3年生存期排序临床信息.txt", col.names = colname_newdata, row.names = rownames(df_clinic), quote=F, sep = "\t")


#################################################### 3年生存期，115个样本的临床信息的卡方分析 ##############################
setwd("./Clinical_Chi-Square_data/")

## 1.patient.age_at_initial_pathologic_diagnosis
clinic_data_1<-read.csv("./1_age_3_year_survival_Chi-Square_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_1 <- data.frame(clinic_data_1$Surval, clinic_data_1$patient.age_at_initial_pathologic_diagnosis)

clinic.data_1 = table(clinic_data_1$Surval, clinic_data_1$patient.age_at_initial_pathologic_diagnosis)
print(clinic.data_1)

print(chisq.test(clinic.data_1))

## 2.patient.age_began_smoking_in_years
clinic_data_2<-read.csv("./2_begansmoking_age_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_2 <- data.frame(clinic_data_2$Surval, clinic_data_2$patient.age_began_smoking_in_years)

clinic.data_2 = table(clinic_data_2$Surval, clinic_data_2$patient.age_began_smoking_in_years)
print(clinic.data_2)

print(chisq.test(clinic.data_2))

## 3.patient.cervical_neoplasm_pathologic_margin_involved_text
clinic_data_3<-read.csv("./3_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_3 <- data.frame(clinic_data_3$Surval, clinic_data_3$patient.cervical_neoplasm_pathologic_margin_involved_text)

clinic.data_3 = table(clinic_data_3$Surval, clinic_data_3$patient.cervical_neoplasm_pathologic_margin_involved_text)
print(clinic.data_3)

print(chisq.test(clinic.data_3))

## 4.patient.cervical_neoplasm_pathologic_margin_involved_type
clinic_data_4<-read.csv("./4_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_4 <- data.frame(clinic_data_4$Surval, clinic_data_4$patient.cervical_neoplasm_pathologic_margin_involved_type)

clinic.data_4 = table(clinic_data_4$Surval, clinic_data_4$patient.cervical_neoplasm_pathologic_margin_involved_type)
print(clinic.data_4)

print(chisq.test(clinic.data_4))

## 5.patient.height
clinic_data_5<-read.csv("./5_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_5 <- data.frame(clinic_data_5$Surval, clinic_data_5$patient.height)

clinic.data_5 = table(clinic_data_5$Surval, clinic_data_5$patient.height)
print(clinic.data_5)

print(chisq.test(clinic.data_5))

## 6.patient.histological_type
clinic_data_6<-read.csv("./6_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_6 <- data.frame(clinic_data_6$Surval, clinic_data_6$patient.histological_type)

clinic.data_6 = table(clinic_data_6$Surval, clinic_data_6$patient.histological_type)
print(clinic.data_6)

print(chisq.test(clinic.data_6))

## 7.patient.race_list.race
clinic_data_7<-read.csv("./7_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_7 <- data.frame(clinic_data_7$Surval, clinic_data_7$patient.race_list.race)

clinic.data_7 = table(clinic_data_7$Surval, clinic_data_7$patient.race_list.race)
print(clinic.data_7)

print(chisq.test(clinic.data_7))

## 8.patient.stage_event.clinical_stage
clinic_data_8<-read.csv("./8_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_8 <- data.frame(clinic_data_8$Surval, clinic_data_8$patient.stage_event.clinical_stage)

clinic.data_8 = table(clinic_data_8$Surval, clinic_data_8$patient.stage_event.clinical_stage)
print(clinic.data_8)

print(chisq.test(clinic.data_8))

## 9.patient.hpv_test_results.hpv_test_result.hpv_call_1
clinic_data_9<-read.csv("./9_115survival_data.csv", header=T,row.names= 1, na.strings=c("NA"))#读入数据
clinic.data_9 <- data.frame(clinic_data_9$Surval, clinic_data_9$patient.hpv_test_results.hpv_test_result.hpv_call_1)

clinic.data_9 = table(clinic_data_9$Surval, clinic_data_9$patient.hpv_test_results.hpv_test_result.hpv_call_1)
print(clinic.data_9)

print(chisq.test(clinic.data_9))










