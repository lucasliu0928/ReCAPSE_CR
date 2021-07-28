source("Recapse_Ultility.R")
################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
data_dir <- "/recapse/intermediate_data/"
perMonth_dir <- "/recapse/intermediate_data/10_perMonthData_withChar/"
outdir <- "/recapse/intermediate_data/11B_CodeCount_Features/"

#local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
perMonth_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10_perMonthData_withChar/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11B_CodeCount_Features/"

################################################################################ 
#1. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(data_dir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id)

# ################################################################################ 
# #1. Load all per month with char data
# ################################################################################ 
# All_data <- read.csv(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.csv"), stringsAsFactors = F)
# missing_table <- get_missing_rate_table(All_data,colnames(All_data))
# table(All_data$y_PRE_OR_POST_2ndEvent)  #0: 2036132   1: 83301 

################################################################################ 
#2. Load Grouped codes
################################################################################ 
grouped_unique_diag_df <- read.csv(paste0(data_dir,"3B_Codes_And_Groups/Unique_Diag_And_Groups.csv"), stringsAsFactors = F)
grouped_unique_proc_df <- read.csv(paste0(data_dir,"3B_Codes_And_Groups/Unique_Proc_And_Groups.csv"), stringsAsFactors = F)
grouped_unique_drug_df <- read.csv(paste0(data_dir,"3B_Codes_And_Groups/Unique_Drug_And_Groups.csv"), stringsAsFactors = F)

#Correct typo for column names
colnames(grouped_unique_diag_df) <- gsub("Chuback","Chubak",colnames(grouped_unique_diag_df))
colnames(grouped_unique_proc_df) <- gsub("Chuback","Chubak",colnames(grouped_unique_proc_df))

#DM3 drug names and group blanks
grouped_unique_drug_df <- grouped_unique_drug_df[-which(grouped_unique_drug_df$Drug_name == ""),] #remove blank drug name
grouped_unique_drug_df[which(grouped_unique_drug_df$general_group == ""),"general_group"] <- NA #recode blank drug grp as NA


#Add prefix to the groups
#CCS
grouped_unique_diag_df$CCS_CATEGORY <- paste0("CCS_D_",grouped_unique_diag_df$CCS_CATEGORY)
grouped_unique_proc_df$CCS_CATEGORY <- paste0("CCS_P_",grouped_unique_proc_df$CCS_CATEGORY)

#Chubak
grouped_unique_diag_df$Chubak_Category <- paste0("Chubak_D_",grouped_unique_diag_df$Chubak_Category)
grouped_unique_diag_df$Chubak_Type     <- paste0("Chubak_D_",grouped_unique_diag_df$Chubak_Type)
grouped_unique_proc_df$Chubak_Category <- paste0("Chubak_P_",grouped_unique_proc_df$Chubak_Category)
grouped_unique_proc_df$Chubak_Type     <- paste0("Chubak_P_",grouped_unique_proc_df$Chubak_Type)

#code having two groups
twogrps_indxes <- which(grepl("[$]",grouped_unique_diag_df[,"Chubak_Category"])==T)
grouped_unique_diag_df[twogrps_indxes,"Chubak_Category"] <- sub( '(?<=.\\${4})', 'Chubak_D_', grouped_unique_diag_df[twogrps_indxes,"Chubak_Category"], perl=TRUE )
twogrps_indxes <- which(grepl("[$]",grouped_unique_diag_df[,"Chubak_Type"])==T)
grouped_unique_diag_df[twogrps_indxes,"Chubak_Type"] <- sub( '(?<=.\\${4})', 'Chubak_D_', grouped_unique_diag_df[twogrps_indxes,"Chubak_Type"], perl=TRUE )

twogrps_indxes <- which(grepl("[$]",grouped_unique_proc_df[,"Chubak_Category"])==T)
grouped_unique_proc_df[twogrps_indxes,"Chubak_Category"] <- sub( '(?<=.\\${4})', 'Chubak_P_', grouped_unique_proc_df[twogrps_indxes,"Chubak_Category"], perl=TRUE )
twogrps_indxes <- which(grepl("[$]",grouped_unique_proc_df[,"Chubak_Type"])==T)
grouped_unique_proc_df[twogrps_indxes,"Chubak_Type"] <- sub( '(?<=.\\${4})', 'Chubak_P_', grouped_unique_proc_df[twogrps_indxes,"Chubak_Type"], perl=TRUE )



#Ritzwoller
grouped_unique_diag_df$Ritzwoller_Category <- paste0("Ritzw_D_",grouped_unique_diag_df$Ritzwoller_Category)
grouped_unique_proc_df$Ritzwoller_Category <- paste0("Ritzw_P_",grouped_unique_proc_df$Ritzwoller_Category)

twogrps_indxes <- which(grepl("[$]",grouped_unique_diag_df[,"Ritzwoller_Category"])==T)
grouped_unique_diag_df[twogrps_indxes,"Ritzwoller_Category"] <- sub( '(?<=.\\${4})', 'Ritzw_D_', grouped_unique_diag_df[twogrps_indxes,"Ritzwoller_Category"], perl=TRUE )
twogrps_indxes <- which(grepl("[$]",grouped_unique_proc_df[,"Ritzwoller_Category"])==T)
grouped_unique_proc_df[twogrps_indxes,"Ritzwoller_Category"] <- sub( '(?<=.\\${4})', 'Ritzw_P_', grouped_unique_proc_df[twogrps_indxes,"Ritzwoller_Category"], perl=TRUE )


#DM3
grouped_unique_drug_df$specific_group <- paste0("DM3_SPE_",grouped_unique_drug_df$specific_group)
grouped_unique_drug_df$general_group  <- paste0("DM3_GEN_",grouped_unique_drug_df$general_group)


################################################################################ 
#2. For each patient per month data, get correponding code count features
################################################################################ 
foreach (i = 1: length(Final_IDs)) %dopar% {
  curr_id <- 10000
  curr_id <- Final_IDs[i]
  curr_perMonth_file <- paste0(perMonth_dir,"ID",curr_id,"_PerMonthData_WithMonthChar_df.xlsx")
  
  if (file.exists(curr_perMonth_file) == T){
    curr_df <- read.xlsx(curr_perMonth_file,sheet = 1)
    curr_drug_counting_df1 <- get_code_feature_df_func(curr_df,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","specific_group")
    curr_drug_counting_df2 <- get_code_feature_df_func(curr_df,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","general_group")
    
  }else{
    curr_drug_counting_df1 <- NULL
    curr_drug_counting_df2 <- NULL
    
  }
  
  
  write.xlsx(curr_comb_df,paste0(outdir,"ID",curr_id,"_","PerMonthData_WithMonthChar_df.xlsx"))
  
}

################################################################################ 
#3.DM3 Drug gourps
################################################################################ 
#specific group
drug_feature_df <- get_code_feature_df_func(All_data,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","specific_group")
write.csv(drug_feature_df,paste0(outdir,"DM3/specific_drug_feature_df.csv"),row.names = F)

#general group
drug_feature_df <- get_code_feature_df_func(All_data,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","general_group")
write.csv(drug_feature_df,paste0(outdir,"DM3/general_drug_feature_df.csv"),row.names = F)


################################################################################ 
#4. CCS Grouped code count
################################################################################ 
diag_feature_df <- get_code_feature_df_func(All_data,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","CCS_CATEGORY")
write.csv(diag_feature_df,paste0(outdir,"CCS/CCS_diag_feature_df.csv"),row.names = F)
proc_feature_df <- get_code_feature_df_func(All_data,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","CCS_CATEGORY")
write.csv(proc_feature_df,paste0(outdir,"CCS/CCS_proc_feature_df.csv"),row.names = F)


################################################################################ 
#4. Chubak Grouped code count
################################################################################ 
diag_feature_df <- get_code_feature_df_func(All_data,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Chubak_Category")
write.csv(diag_feature_df,paste0(outdir,"Chubak/Chubak_diag_Category_feature_df.csv"),row.names = F)
diag_feature_df <- get_code_feature_df_func(All_data,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Chubak_Type")
write.csv(diag_feature_df,paste0(outdir,"Chubak/Chubak_diag_Type_feature_df.csv"),row.names = F)

proc_feature_df <- get_code_feature_df_func(All_data,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Chubak_Category")
write.csv(proc_feature_df,paste0(outdir,"Chubak/Chubak_proc_Category_feature_df.csv"),row.names = F)
proc_feature_df <- get_code_feature_df_func(All_data,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Chubak_Type")
write.csv(proc_feature_df,paste0(outdir,"Chubak/Chubak_proc_Type_feature_df.csv"),row.names = F)


################################################################################ 
#4. Ritzwoller Grouped code count
################################################################################ 
diag_feature_df <- get_code_feature_df_func(All_data,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Ritzwoller_Category")
write.csv(diag_feature_df,paste0(outdir,"Ritzwoller/Ritzwoller_diag_Category_feature_df.csv"),row.names = F)

proc_feature_df <- get_code_feature_df_func(All_data,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Ritzwoller_Category")
write.csv(proc_feature_df,paste0(outdir,"Ritzwoller/Ritzwoller_proc_Category_feature_df.csv"),row.names = F)
