source("Recapse_Ultility.R")
################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


################################################################################ 
#1. Load all per month with char data
################################################################################ 
All_data <- read.csv(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.csv"), stringsAsFactors = F)
missing_table <- get_missing_rate_table(All_data,colnames(All_data))
table(All_data$y_PRE_OR_POST_2ndEvent) #661098,  1 :31563 

################################################################################ 
#2. Load Grouped codes
################################################################################ 
grouped_unique_diag_df <- read.csv(paste0(data_dir,"11_grouped_unique_diag_df.csv"), stringsAsFactors = F)
grouped_unique_diag_df$CCS_CATEGORY <- paste0("CCS_D_",grouped_unique_diag_df$CCS_CATEGORY)
grouped_unique_proc_df <- read.csv(paste0(data_dir,"11_grouped_unique_proc_df.csv"), stringsAsFactors = F)
grouped_unique_proc_df$CCS_CATEGORY <- paste0("CCS_P_",grouped_unique_proc_df$CCS_CATEGORY)
grouped_unique_drug_df <- read.csv(paste0(data_dir,"11_grouped_unique_drug_df.csv"), stringsAsFactors = F)
grouped_unique_drug_df$specific_group <- paste0("DM3_SPE_",grouped_unique_drug_df$specific_group)


diag_feature_df <- get_code_feature_df_func(All_data,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","CCS_CATEGORY")
write.csv(diag_feature_df,paste0(outdir,"12_diag_feature_df.csv"),row.names = F)
proc_feature_df <- get_code_feature_df_func(All_data,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","CCS_CATEGORY")
write.csv(proc_feature_df,paste0(outdir,"12_proc_feature_df.csv"),row.names = F)
drug_feature_df <- get_code_feature_df_func(All_data,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","specific_group")
write.csv(drug_feature_df,paste0(outdir,"12_drug_feature_df.csv"),row.names = F)

##Get race1,race2....,grade2,summ_stage2,laterality9,er_stat0
# er_stat1
# pr_stat0
# pr_stat1
# her2_stat0
# her2_stat1
# surg_prim_site20