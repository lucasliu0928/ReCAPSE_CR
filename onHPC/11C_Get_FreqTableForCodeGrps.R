source("Recapse_Ultility.R")
################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
data_dir <- "/recapse/intermediate_data/11B_CodeCount_Features/"
outdir <- "/recapse/intermediate_data/11C_CodeGroup_Freq_tb/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11B_CodeCount_Features/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11C_CodeGroup_Freq_tb/"


################################################################################ 
#1. Combine all pts permonth code group feature data into one
################################################################################ 
#1.DM3
DM3_SPE_tb <- get_code_group_freq( paste0(data_dir,"DM3/Specific/"),"DM3_SPE")
DM3_GEN_tb <- get_code_group_freq( paste0(data_dir,"DM3/General/"),"DM3_GEN")
write.csv(DM3_SPE_tb,paste0(outdir,"11C_DM3_SPE_freq_tb.csv"),row.names = F)
write.csv(DM3_GEN_tb,paste0(outdir,"11C_DM3_GEN_freq_tb.csv"),row.names = F)

#CCS
CCS_diag_tb <- get_code_group_freq( paste0(data_dir,"CCS/Diag/"),"CCS_D")
CCS_proc_tb <- get_code_group_freq( paste0(data_dir,"CCS/Proc/"),"CCS_P")
write.csv(CCS_diag_tb,paste0(outdir,"11C_CCS_D_freq_tb.csv"),row.names = F)
write.csv(CCS_proc_tb,paste0(outdir,"11C_CCS_P_freq_tb.csv"),row.names = F)


#Chubak
Chubak_Diag_Category_tb <- get_code_group_freq( paste0(data_dir,"Chubak/Diag_Category/"),"Chubak_D")
Chubak_Diag_Type_tb <- get_code_group_freq( paste0(data_dir,"Chubak/Diag_Type/"),"Chubak_D")
Chubak_Proc_Category_tb <- get_code_group_freq( paste0(data_dir,"Chubak/Proc_Category/"),"Chubak_P")
Chubak_Proc_Type_tb <- get_code_group_freq( paste0(data_dir,"Chubak/Proc_Type/"),"Chubak_P")
write.csv(Chubak_Diag_Category_tb,paste0(outdir,"11C_Chubak_D_Category_freq_tb.csv"),row.names = F)
write.csv(Chubak_Diag_Type_tb,paste0(outdir,"11C_Chubak_D_Type_freq_tb.csv"),row.names = F)
write.csv(Chubak_Proc_Category_tb,paste0(outdir,"11C_Chubak_P_Cateogry_freq_tb.csv"),row.names = F)
write.csv(Chubak_Proc_Type_tb,paste0(outdir,"11C_Chubak_P_Type_freq_tb.csv"),row.names = F)

#Ritzwoller
Ritzwoller_diag_tb <- get_code_group_freq( paste0(data_dir,"Ritzwoller/Diag/"),"Ritzw_D")
Ritzwoller_proc_tb <- get_code_group_freq( paste0(data_dir,"Ritzwoller/Proc/"),"Ritzw_P")
write.csv(Ritzwoller_diag_tb,paste0(outdir,"11C_Ritzw_D_freq_tb.csv"),row.names = F)
write.csv(Ritzwoller_proc_tb,paste0(outdir,"11C_Ritzw_P_freq_tb.csv"),row.names = F)
