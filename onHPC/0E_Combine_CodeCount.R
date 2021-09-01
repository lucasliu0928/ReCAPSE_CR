library(openxlsx)
#######################################################################
##############              Data dir                     ############## 
#######################################################################
data_dir <- "/recapse/intermediate_data/0_Codes/Count/"
outdir   <- "/recapse/intermediate_data/0_Codes/Count/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/Count/"
# outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/Count/"

#######################################################################
#2. Combine
#######################################################################
#Proc
proc_files <- list.files(paste0(data_dir,"Unique_Proc_PtsLevelCount2/"),full.names = T)
proc_ct_df <- do.call(rbind,lapply(proc_files, read.xlsx))
write.xlsx(proc_ct_df,paste0(outdir,"proc_count.xlsx"))

#Diagnoise
diag_files <- list.files(paste0(data_dir,"Unique_Diag_PtsLevelCount2/"),full.names = T)
diag_ct_df <- do.call(rbind,lapply(diag_files, read.xlsx))
write.xlsx(diag_ct_df,paste0(outdir,"diag_count.xlsx"))

#Drug
drug_files <- list.files(paste0(data_dir,"Unique_Drug_PtsLevelCount2/"),full.names = T)
drug_ct_df <- do.call(rbind,lapply(drug_files, read.xlsx))
write.xlsx(drug_ct_df,paste0(outdir,"drug_count.xlsx"))
