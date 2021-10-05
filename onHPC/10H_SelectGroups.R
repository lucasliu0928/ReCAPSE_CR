source("Recapse_Ultility.R")
#This script get patient level group count for Final Id (All enrolls) in valid month

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "10G_Counts_UniqueGrp_PtsLevel/WithPossibleMonthsHasNoCodes/")

outdir     <- paste0(proj_dir, "10H_Selected_Grps/WithPossibleMonthsHasNoCodes/")


################################################################################
#Select Final grps for model ready data
#Select grps from:
#1. Count_CCS_Diag_Unique_Grps.xlsx
#2. Count_CCS_proc_Unique_Grps.xlsx
#3. Count_DM3_SPE_Unique_Grps.xlsx
################################################################################
count_df1 <- read.xlsx(paste0(data_dir1,"Count_CCS_Diag_Unique_Grps.xlsx"),sheet = 1)
count_df2 <- read.xlsx(paste0(data_dir1,"Count_CCS_proc_Unique_Grps.xlsx"),sheet = 1)
count_df3 <- read.xlsx(paste0(data_dir1,"Count_DM3_SPE_Unique_Grps.xlsx"),sheet = 1)

#Diag
selected_index1 <- which(count_df1[,"Frac_PtsHasTheGrp_SBCE"] > 0.1 | count_df1[,"Frac_PtsHasTheGrp_nonSBCE"] > 0.15)
Final_selected_grps_df1 <- count_df1[selected_index1,"Code_Grp"] #130
write.xlsx(Final_selected_grps_df1,paste0(outdir,"Selected_CCSDiag_Unique_Grps.xlsx"))

#Proc
selected_index2 <- which(count_df2[,"Frac_PtsHasTheGrp_SBCE"] > 0.1 | count_df2[,"Frac_PtsHasTheGrp_nonSBCE"] > 0.15)
Final_selected_grps_df2 <- count_df2[selected_index2,"Code_Grp"] #61
write.xlsx(Final_selected_grps_df2,paste0(outdir,"Selected_CCSProc_Unique_Grps.xlsx"))

#Drug
selected_index3 <- which(count_df3[,"Frac_PtsHasTheGrp_SBCE"] > 0.1 | count_df3[,"Frac_PtsHasTheGrp_nonSBCE"] > 0.15)
Final_selected_grps_df3 <- count_df3[selected_index3,"Code_Grp"] #27
write.xlsx(Final_selected_grps_df3,paste0(outdir,"Selected_DM3SPEDrug_Unique_Grps.xlsx"))



