source("Recapse_Ultility.R")
#Transoformation takes a lot of time
#So in this script, we only do transformation for selected code group that has occurance freqeuncy > 0.05
################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
data_dir <- "/recapse/intermediate_data/"
Codecount_dir <- "/recapse/intermediate_data/11B_CodeCount_Features/"
outdir <- "/recapse/intermediate_data/11E_CodeTransform_Features/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# Codecount_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11B_CodeCount_Features/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11E_CodeTransform_Features/"


################################################################################ 
#1. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(data_dir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id)

#########################################################################################################
#2. Load selected feature groups
#########################################################################################################
selected_Features_df <- read.csv(paste0(data_dir,"11D_Selcted_CodeGroups_freqGT005_tb.csv"),stringsAsFactors = F)
selected_Features <- selected_Features_df$Code_Group

#########################################################################################################
#2. For each patient per month code count data, get correponding code transformation features
#Only do transformation for preselected code group that > threhold of occurance 
#1) time_since : the time since the most recent occurrence of this code group
#2) time_until:  the time since the soonest future occurrence of this code group
#3) cumul_ratio: the total number of occurrences in each patient up to the time in question of that grouping divided by total elapsed time
#########################################################################################################
foreach (i = 1: length(Final_IDs)) %dopar% {
  curr_id <- Final_IDs[i]
  #CCS
  ccs_diag_file <- paste0(Codecount_dir,"CCS/Diag/","ID",curr_id,"_CCS_diag_feature_df.xlsx")
  ccs_diag_df     <- read_codecount_df(ccs_diag_file,selected_Features)
  ccs_proc_file <- paste0(Codecount_dir,"CCS/Proc/","ID",curr_id,"_CCS_proc_feature_df.xlsx")
  ccs_proc_df     <- read_codecount_df(ccs_proc_file,selected_Features)
  
  #DM3
  DM3_specific_file <- paste0(Codecount_dir,"DM3/Specific/","ID",curr_id,"_specific_drug_feature_df.xlsx")
  DM3_SPE_df     <- read_codecount_df(DM3_specific_file,selected_Features)
  DM3_general_file <- paste0(Codecount_dir,"DM3/General/","ID",curr_id,"_general_drug_feature_df.xlsx")
  DM3_GEN_df     <- read_codecount_df(DM3_general_file,selected_Features)
  
  #Chubak
  chubak_diag_category_file <- paste0(Codecount_dir,"Chubak/Diag_Category/","ID",curr_id,"_chubak_diag_category_feature_df.xlsx")
  chubak_D_Category_df     <- read_codecount_df(chubak_diag_category_file,selected_Features)
  chubak_diag_type_file <- paste0(Codecount_dir,"Chubak/Diag_Type/","ID",curr_id,"_chubak_diag_type_feature_df.xlsx")
  chubak_D_Type_df     <- read_codecount_df(chubak_diag_type_file,selected_Features)
  
  chubak_proc_category_file <- paste0(Codecount_dir,"Chubak/Proc_Category/","ID",curr_id,"_chubak_proc_category_feature_df.xlsx")
  chubak_P_Category_df     <- read_codecount_df(chubak_proc_category_file,selected_Features)
  chubak_proc_type_file <- paste0(Codecount_dir,"Chubak/Proc_Type/","ID",curr_id,"_chubak_proc_type_feature_df.xlsx")
  chubak_P_Type_df     <- read_codecount_df(chubak_proc_type_file,selected_Features)
  
  #Ritzwoller
  ritz_diag_file <- paste0(Codecount_dir,"Ritzwoller/Diag/","ID",curr_id,"_Ritzwoller_diag_category_feature_df.xlsx")
  ritz_diag_df     <- read_codecount_df(ritz_diag_file,selected_Features)
  ritz_proc_file <- paste0(Codecount_dir,"Ritzwoller/Proc/","ID",curr_id,"_Ritzwoller_proc_type_feature_df.xlsx")
  ritz_proc_df     <- read_codecount_df(ritz_proc_file,selected_Features)
  
  #Check row matches
  #identical(ccs_diag_df$study_id,ritz_proc_df$study_id)
  #identical(ccs_diag_df$Month_Start,ritz_proc_df$Month_Start)
  
  #combine all seleted code count features
  comb_df <- cbind(ccs_diag_df,ccs_proc_df,
                   DM3_SPE_df,DM3_GEN_df,
                   chubak_D_Category_df,chubak_D_Type_df,
                   chubak_P_Category_df,chubak_P_Type_df,
                   ritz_diag_df,ritz_proc_df)
  IDandMonth_cols <- which(colnames(comb_df) %in% c("study_id","Month_Start"))
  Reduant_IDandMonth_cols <- IDandMonth_cols[3:length(IDandMonth_cols)]
  comb_df <- comb_df[,-Reduant_IDandMonth_cols]
  
  #Transform
  comb_trans_df <- apply_code_transforamtion_func(comb_df)
  #remove redudant ID and month and month index
  IDandMonth_cols2 <- which(colnames(comb_trans_df) %in% c("study_id","Month_Start","Month_Index"))
  Reduant_IDandMonth_cols2 <- IDandMonth_cols2[4:length(IDandMonth_cols2)]
  comb_trans_df <- comb_trans_df[,-Reduant_IDandMonth_cols2]
    
  write.xlsx(comb_trans_df,paste0(outdir,"ID",curr_id,"_SelectedGrps_transf_df.xlsx"))
  

}


