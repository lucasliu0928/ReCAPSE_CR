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

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# perMonth_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10_perMonthData_withChar/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11B_CodeCount_Features/"

################################################################################ 
#1. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(data_dir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id)

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

#numbers
length(unique(unlist(strsplit(unique(grouped_unique_diag_df$CCS_CATEGORY),split = "$$$$",fixed = T)))) #265
length(unique(unlist(strsplit(unique(grouped_unique_proc_df$CCS_CATEGORY),split = "$$$$",fixed = T)))) #222


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

#numbers
length(unique(unlist(strsplit(unique(grouped_unique_proc_df$Chubak_Category),split = "$$$$",fixed = T)))) #24
length(unique(unlist(strsplit(unique(grouped_unique_proc_df$Chubak_Type),split = "$$$$",fixed = T)))) #133

length(unique(unlist(strsplit(unique(grouped_unique_diag_df$Chubak_Category),split = "$$$$",fixed = T)))) #13
length(unique(unlist(strsplit(unique(grouped_unique_diag_df$Chubak_Type),split = "$$$$",fixed = T)))) #78

#Ritzwoller
grouped_unique_diag_df$Ritzwoller_Category <- paste0("Ritzw_D_",grouped_unique_diag_df$Ritzwoller_Category)
grouped_unique_proc_df$Ritzwoller_Category <- paste0("Ritzw_P_",grouped_unique_proc_df$Ritzwoller_Category)

twogrps_indxes <- which(grepl("[$]",grouped_unique_diag_df[,"Ritzwoller_Category"])==T)
grouped_unique_diag_df[twogrps_indxes,"Ritzwoller_Category"] <- sub( '(?<=.\\${4})', 'Ritzw_D_', grouped_unique_diag_df[twogrps_indxes,"Ritzwoller_Category"], perl=TRUE )
twogrps_indxes <- which(grepl("[$]",grouped_unique_proc_df[,"Ritzwoller_Category"])==T)
grouped_unique_proc_df[twogrps_indxes,"Ritzwoller_Category"] <- sub( '(?<=.\\${4})', 'Ritzw_P_', grouped_unique_proc_df[twogrps_indxes,"Ritzwoller_Category"], perl=TRUE )

#numbers:
length(unique(unlist(strsplit(unique(grouped_unique_diag_df$Ritzwoller_Category),split = "$$$$",fixed = T)))) #5
length(unique(unlist(strsplit(unique(grouped_unique_proc_df$Ritzwoller_Category),split = "$$$$",fixed = T)))) #6


#DM3
grouped_unique_drug_df$specific_group <- paste0("DM3_SPE_",grouped_unique_drug_df$specific_group)
grouped_unique_drug_df$general_group  <- paste0("DM3_GEN_",grouped_unique_drug_df$general_group)
#numbers:
length(unique(unlist(strsplit(unique(grouped_unique_drug_df$specific_group),split = "$$$$",fixed = T)))) #58
length(unique(unlist(strsplit(unique(grouped_unique_drug_df$general_group),split = "$$$$",fixed = T)))) #13


################################################################################ 
#2. For each patient per month data, get correponding code count features
################################################################################ 
foreach (i = 1: length(Final_IDs)) %dopar% {
  curr_id <- Final_IDs[i]
  curr_perMonth_file <- paste0(perMonth_dir,"ID",curr_id,"_PerMonthData_WithMonthChar_df.xlsx")
  
  if (file.exists(curr_perMonth_file) == T){
    curr_df <- read.xlsx(curr_perMonth_file,sheet = 1)
    #DM3
    curr_drug_counting_df1 <- get_code_feature_df_func(curr_df,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","specific_group")
    curr_drug_counting_df2 <- get_code_feature_df_func(curr_df,grouped_unique_drug_df,"Unique_Drug_Codes","Drug_Codes","general_group")
    #CCS
    curr_CCS_counting_df_diag <- get_code_feature_df_func(curr_df,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","CCS_CATEGORY")
    curr_CCS_counting_df_proc <- get_code_feature_df_func(curr_df,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","CCS_CATEGORY")
    
    #Chubak
    curr_chubak_counting_df_diag_category <- get_code_feature_df_func(curr_df,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Chubak_Category")
    curr_chubak_counting_df_diag_type <- get_code_feature_df_func(curr_df,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Chubak_Type")

    curr_chubak_counting_df_proc_category <- get_code_feature_df_func(curr_df,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Chubak_Category")
    curr_chubak_counting_df_proc_type <- get_code_feature_df_func(curr_df,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Chubak_Type")
    
    #rizwoller
    curr_ritz_counting_df_diag_category <- get_code_feature_df_func(curr_df,grouped_unique_diag_df,"Unique_Diag_Codes","Diag_Codes","Ritzwoller_Category")
    curr_ritz_counting_df_proc_category <- get_code_feature_df_func(curr_df,grouped_unique_proc_df,"Unique_Proc_Codes","Proc_Codes","Ritzwoller_Category")
    
    #output
    write.xlsx(curr_drug_counting_df1,paste0(outdir,"DM3/Specific/","ID",curr_id,"_","specific_drug_feature_df.xlsx"))
    write.xlsx(curr_drug_counting_df2,paste0(outdir,"DM3/General/","ID",curr_id,"_","general_drug_feature_df.xlsx"))
    
    write.xlsx(curr_CCS_counting_df_diag,paste0(outdir,"CCS/Diag/","ID",curr_id,"_","CCS_diag_feature_df.xlsx"))
    write.xlsx(curr_CCS_counting_df_proc,paste0(outdir,"CCS/Proc/","ID",curr_id,"_","CCS_proc_feature_df.xlsx"))
    
    write.xlsx(curr_chubak_counting_df_diag_category,paste0(outdir,"Chubak/Diag_Category/","ID",curr_id,"_","chubak_diag_category_feature_df.xlsx"))
    write.xlsx(curr_chubak_counting_df_diag_type,paste0(outdir,"Chubak/Diag_Type/","ID",curr_id,"_","chubak_diag_type_feature_df.xlsx"))
    write.xlsx(curr_chubak_counting_df_proc_category,paste0(outdir,"Chubak/Proc_Category/","ID",curr_id,"_","chubak_proc_category_feature_df.xlsx"))
    write.xlsx(curr_chubak_counting_df_proc_type,paste0(outdir,"Chubak/Proc_Type/","ID",curr_id,"_","chubak_proc_type_feature_df.xlsx"))
    
    
    write.xlsx(curr_ritz_counting_df_diag_category,paste0(outdir,"Ritzwoller/Diag/","ID",curr_id,"_","Ritzwoller_diag_category_feature_df.xlsx"))
    write.xlsx(curr_ritz_counting_df_proc_category,paste0(outdir,"Ritzwoller/Proc/","ID",curr_id,"_","Ritzwoller_proc_type_feature_df.xlsx"))
    

  }
  

  
}


