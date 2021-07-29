source("Recapse_Ultility.R")
#This script:
#for each patient for selected features(By freq) combines:
#1.CodeCount feature
#2.CodeTrans feature
#3.BinaryChar feature

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
project_dir <- "/recapse/intermediate_data/"
codeCount_feature_dir  <- paste0(project_dir,"11B_CodeCount_Features/")
codeTransf_feature_dir <- paste0(project_dir,"11E_CodeTransform_Features/")
binarychar_feature_dir <- project_dir
selected_feature_dir   <- project_dir
outdir                 <- paste0(project_dir,"12_ModelReadyData/")

# # #local
# project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# codeCount_feature_dir  <- paste0(project_dir,"11B_CodeCount_Features/")
# codeTransf_feature_dir <- paste0(project_dir,"11E_CodeTransform_Features/")
# binarychar_feature_dir <- project_dir
# selected_feature_dir   <- project_dir
# outdir                 <- paste0(project_dir,"12_ModelReadyData/")


################################################################################ 
#1. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(project_dir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id) #23378

#########################################################################################################
#3. Load selected feature groups
#########################################################################################################
selected_Features_df <- read.csv(paste0(selected_feature_dir,"11D_Selcted_CodeGroups_freqGT005_tb.csv"),stringsAsFactors = F)
selected_Features <- selected_Features_df$Code_Group #51
selected_CCS_D <- selected_Features_df[which(grepl("CCS_D",selected_Features_df$Code_Group)==T),"Code_Group"] #12
selected_CCS_P <- selected_Features_df[which(grepl("CCS_P",selected_Features_df$Code_Group)==T),"Code_Group"] #1
selected_DM3_SPE <- selected_Features_df[which(grepl("DM3_SPE",selected_Features_df$Code_Group)==T),"Code_Group"] #11
# selected_DM3_GEN <- selected_Features_df[which(grepl("DM3_GEN",selected_Features_df$Code_Group)==T),"Code_Group"] #8
# selected_Chubak <- selected_Features_df[which(grepl("Chubak",selected_Features_df$Code_Group)==T),"Code_Group"] #17
# selected_Ritzw <- selected_Features_df[which(grepl("Ritzw",selected_Features_df$Code_Group)==T),"Code_Group"] #2 (unkown grp)

################################################################################ 
#2. Load All binary char feature
################################################################################ 
All_binary_char_features <- as.data.frame(fread(paste0(binarychar_feature_dir,"11_All_PerMonthData_WithBinaryChar.csv")))
length(unique(All_binary_char_features$study_id)) # 23378

#########################################################################################################
#2. For each patient combine three dataframe
#########################################################################################################
#1) Only use CCS and DM3
foreach (i = 1: length(Final_IDs)) %dopar% {
  curr_id <- Final_IDs[i]
  #1.CodeCount feature
  #CCS Diag
  CodeCount_file1 <- paste0(codeCount_feature_dir,"/CCS/Diag/","ID",curr_id,"_CCS_diag_feature_df.xlsx")
  CodeCount_df1 <- read.xlsx(CodeCount_file1,sheet = 1)
  keep_indxes <- which(colnames(CodeCount_df1) %in% c("study_id","Month_Start",selected_CCS_D)) ###Only Keep selected features  and IDs
  CodeCount_df1 <- CodeCount_df1[,keep_indxes]  #12

  #CCS Proc
  CodeCount_file2 <- paste0(codeCount_feature_dir,"/CCS/Proc/","ID",curr_id,"_CCS_proc_feature_df.xlsx")
  CodeCount_df2 <- read.xlsx(CodeCount_file2,sheet = 1)
  keep_indxes <- which(colnames(CodeCount_df2) %in% c("study_id","Month_Start",selected_CCS_P)) ###Only Keep selected features  and IDs
  CodeCount_df2 <- CodeCount_df2[,keep_indxes]  #1
  
  #DM3 Specific Drug
  CodeCount_file3 <- paste0(codeCount_feature_dir,"/DM3/Specific/","ID",curr_id,"_specific_drug_feature_df.xlsx")
  CodeCount_df3 <- read.xlsx(CodeCount_file3,sheet = 1)
  keep_indxes <- which(colnames(CodeCount_df3) %in% c("study_id","Month_Start",selected_DM3_SPE)) ###Only Keep selected features  and IDs
  CodeCount_df3 <- CodeCount_df3[,keep_indxes]  #11
  
  #2.CodeTrans feature
  CodeTrans_file <- paste0(codeTransf_feature_dir,"ID",curr_id,"_SelectedGrps_transf_df.xlsx")
  CodeTrans_df <- read.xlsx(CodeTrans_file,sheet = 1)
  simp_colnames <- gsub("time_since_|time_until_|cumul_ratio_","",colnames(CodeTrans_df))
  keep_indxes <- which(simp_colnames %in% c("study_id","Month_Start",selected_CCS_D,selected_CCS_P,selected_DM3_SPE)) ###Only Keep selected features  and IDs
  CodeTrans_df <- CodeTrans_df[,keep_indxes]  #72
  
  #3.BinaryChar feature
  BinaryChar_df <- All_binary_char_features[which(All_binary_char_features[,"study_id"] == curr_id),]
  BinaryChar_df[,"Month_Start"] <- as.character(BinaryChar_df[,"Month_Start"])
  exclude_indxes <- which(colnames(BinaryChar_df) %in% c("Diag_Codes","Proc_Codes","Drug_Codes",
                                                         "has_second_event","months_to_second_event")) ###excluded char features
  BinaryChar_df <- BinaryChar_df[,-exclude_indxes]  #179
  
  #Reoder rows in other df to CodeCount1 
  reorder_indxes <- match(CodeCount_df1[,"Month_Start"],CodeCount_df2[,"Month_Start"])
  CodeCount_df2 <- CodeCount_df2[reorder_indxes,]
  reorder_indxes <- match(CodeCount_df1[,"Month_Start"],CodeCount_df3[,"Month_Start"])
  CodeCount_df3 <- CodeCount_df3[reorder_indxes,]
  reorder_indxes <- match(CodeCount_df1[,"Month_Start"],CodeTrans_df[,"Month_Start"])
  CodeTrans_df <- CodeTrans_df[reorder_indxes,]
  reorder_indxes <- match(CodeCount_df1[,"Month_Start"],BinaryChar_df[,"Month_Start"])
  BinaryChar_df <- BinaryChar_df[reorder_indxes,]
  
  #combine 
  comb_df <- cbind(CodeCount_df1,CodeCount_df2,CodeCount_df3,CodeTrans_df,BinaryChar_df)
  #remove duplicated ID columns
  ID_month_col <- which(colnames(comb_df) %in% c("study_id","Month_Start"))
  dup_ID_month_col <- ID_month_col[3:length(ID_month_col)]
  comb_df <- comb_df[,-dup_ID_month_col]
  

  write.xlsx(comb_df,paste0(outdir,"ID",curr_id,"_ModelReady_df.xlsx"))
  
}