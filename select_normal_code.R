# 切换工作路径
rm(list = ls())
setwd('C:/Users/CQU_ddy/Desktop/data/ddy20201022-COXPH/0_原始MIRNA数据筛选出正常人样本')

# 筛选数据(筛选出癌组织样本，正常组织样本，转移癌样本...)
library(stringr)

# 加载OV_2col_data数据
datddy = read.table("CESC.miRseq_RPKM_log2.txt", header = T, sep = '\t')
# 去掉datddy数据的第一列(miRNA名称)
dat2 = datddy[, -1]
# 查看数据维数
dim(dat2) #542 312
#View(head(dat2))

# 筛选癌组织样本数据(此种方法只能筛选出-01*的数据，筛选不全)
dat.tumor = dat2[, str_detect(colnames(dat2), '.01')]
dim(dat.tumor)
View(head(dat.tumor))

row.names(dat.tumor) = datddy[, 1]
# 列名顺序经过筛选已变，不能再通过赋值替换了
# colnames(dat.tumor) = colnames(datddy)
write.table(dat.tumor, "CESC_tumor_miRNA1.txt", col.names = T, row.names = T, quote = F, sep = '\t')

# 筛选正常组织样本数据(此种方法只能筛选出-11*的数据,筛选不全)
dat.normal = dat2[, str_detect(colnames(dat2), '.11')]
dim(dat.normal)#把包含11的全筛选进来了，多筛选了一个癌症患者样本
View(head(dat.normal))

row.names(dat.normal) = datddy[, 1]
write.table(dat.normal, "CESC_normal_miRNA.txt", col.names = T, row.names = T, quote = F, sep = '\t')

# 筛选漏掉的样本Sample ID
m <- colnames(datddy)[!(colnames(datddy) %in% colnames(dat.tumor))]# 从原数据中踢掉的所有已筛选的癌症患者Sample_ID
m
m <- m[!(m %in% colnames(dat.normal))]# 从原数据中踢掉的所有已筛选的癌症患者和正常人的Sample_ID
m#上述所有流程筛选漏掉的样本Sample_ID # TCGA.HM.A6W2.06 TCGA.UC.A7PG.06 为转移癌样本

