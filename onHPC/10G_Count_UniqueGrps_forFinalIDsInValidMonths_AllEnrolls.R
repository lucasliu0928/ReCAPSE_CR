source("Recapse_Ultility.R")
#This script get patient level group count for Final Id (All enrolls) in valid month

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
data_dir2  <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")
data_dir_CCSdiag  <- paste0(proj_dir, "10B_CCSDiagFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")
data_dir_CCSproc  <- paste0(proj_dir, "10C_CCSProcFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")
data_dir_DM3spe   <- paste0(proj_dir, "10D_DM3SPEFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")
data_dir_DM3gen   <- paste0(proj_dir, "10E_DM3GENFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")
data_dir_SGNN     <- paste0(proj_dir, "10F_ShortGNNFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")

outdir   <- paste0(proj_dir, "10G_Counts_UniqueGrp_PtsLevel/WithPossibleMonthsHasNoCodes/")


################################################################################
#'Load pts SBCElabel
################################################################################
PTS_char_df <- read.xlsx(paste0(data_dir1,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
SBCE_PTs    <- PTS_char_df[which(PTS_char_df$SBCE==1),"study_id"]    
nonSBCE_PTs <- PTS_char_df[which(PTS_char_df$SBCE==0),"study_id"] 

################################################################################
#Read unique code discrption file
################################################################################
diag_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Diag_And_Groups_inALLClaims.xlsx"),sheet = 1)
proc_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Proc_And_Groups_inALLClaims.xlsx"),sheet = 1)
drug_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Drug_And_Groups_inALLClaims.xlsx"),sheet = 1)

################################################################################
#Count and add discription 
################################################################################
#Diag:
CCS_Diag_count_tb  <- get_count_table_func(data_dir_CCSdiag,"CCS_DIAG",SBCE_PTs,nonSBCE_PTs)
CCS_Diag_count_tb  <- add_grp_discrption_func(CCS_Diag_count_tb,diag_disp_df,"CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION")
write.xlsx(CCS_Diag_count_tb,paste0(outdir,"Count_CCS_Diag_Unique_Grps.xlsx"))


CCS_Proc_count_tb  <- get_count_table_func(data_dir_CCSproc,"CCS_PROC",SBCE_PTs,nonSBCE_PTs)
CCS_Proc_count_tb  <- add_grp_discrption_func(CCS_Proc_count_tb,proc_disp_df,"CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION")
write.xlsx(CCS_Proc_count_tb,paste0(outdir,"Count_CCS_proc_Unique_Grps.xlsx"))


DM3_SPE_count_tb   <- get_count_table_func(data_dir_DM3spe,"DM3_SPE",SBCE_PTs,nonSBCE_PTs)
DM3_SPE_count_tb$Grp_Discrip <- NA  #No discrip for DM3
write.xlsx(DM3_SPE_count_tb,paste0(outdir,"Count_DM3_SPE_Unique_Grps.xlsx"))

DM3_GEN_count_tb   <- get_count_table_func(data_dir_DM3gen,"DM3_GEN",SBCE_PTs,nonSBCE_PTs)
DM3_GEN_count_tb$Grp_Discrip <- NA  #No discrip for DM3
write.xlsx(DM3_GEN_count_tb,paste0(outdir,"Count_DM3_GEN_Unique_Grps.xlsx"))


#Short GNN
S_GNN_count_tb     <- get_count_table_func(data_dir_SGNN,"S_GNN",SBCE_PTs,nonSBCE_PTs)
S_GNN_count_tb     <- add_grp_discrption_func(S_GNN_count_tb,drug_disp_df,"short_GNN","specific_group") ##Use DM3 SPE as the discrption for S_GNN, to see how many matched to DM3 spe
colnames(S_GNN_count_tb)[6] <- "DM3_SPE"
write.xlsx(S_GNN_count_tb,paste0(outdir,"Count_S_GNN_Unique_Grps.xlsx"))


