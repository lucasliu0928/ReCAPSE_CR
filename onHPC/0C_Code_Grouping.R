source("Recapse_Ultility.R")

#######################################################################
##############              Data dir                     ############## 
#######################################################################
code_data_dir <- "/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/recapse/data/"
outdir   <- "/recapse/intermediate_data/0_Codes/"

#local
code_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"


################################################################################
#1. Load grping data
################################################################################
#1. Load CCS Groups file:                                      
CCS_df <- load_and_clean_CSS_data(grping_data_dir)
CCS_Diag_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Diag","ICD10_Diag")),]
CCS_Proc_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Proc","ICD10_Proc")),]

#2.Load CCS servis procedure group file 
CCS_SProc_df <- load_and_clean_CSS_ServicesP_data(grping_data_dir)

#3. Load Chuback Groups file:                                
Chuback_df <- load_and_clean_Chubak_data(grping_data_dir)
Chuback_Diag_df <- Chuback_df[which(Chuback_df$Code.type %in% c("ICD-9 diagnosis")),]
Chuback_Proc_df <- Chuback_df[which(Chuback_df$Code.type %in% c("HCPC","ICD-9 procedure","CPT", "CPT category II","CPT category III")),]

#4. load Ritzwoller file:  
Ritzwoller_df <- load_and_clean_Ritzwoller_data(grping_data_dir)
Ritzwoller_Diag_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Diagnostic"),] #48
Ritzwoller_Proc_df <- Ritzwoller_df[which(Ritzwoller_df$D_or_P == "Procedure"),] #1008

#5.Load drug group 
DM3_df <- load_and_clean_DM3_data(grping_data_dir)

################################################################################
#2. Load unique codes data
################################################################################
unique_diag_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Diag_Codes.xlsx"),sheet = 1)
unique_proc_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Proc_Codes.xlsx"),sheet = 1)
unique_drug_df <- read.xlsx(paste0(code_data_dir,"0_Cleaned_Unique_Drug_Codes.xlsx"),sheet = 1)


################################################################################
#3. Code grouping
################################################################################
#1. diagnose codes
grouped_unique_diag_df <- group_codes_into_CCS_func(unique_diag_df,CCS_Diag_df,NULL) #add CCS
grouped_unique_diag_df <- group_codes_into_chubak_func(grouped_unique_diag_df,Chuback_Diag_df) #add chuback
grouped_unique_diag_df <- group_codes_into_Ritzwoller_func(grouped_unique_diag_df,Ritzwoller_Diag_df) #add ritzwoller
write.xlsx(grouped_unique_diag_df,paste0(outdir,"Unique_Diag_And_Groups_inALLClaims.xlsx"))

#2. procedure codes
grouped_unique_proc_df <- group_codes_into_CCS_func(unique_proc_df,CCS_Proc_df,CCS_SProc_df) #add CCS
grouped_unique_proc_df <- group_codes_into_chubak_func(grouped_unique_proc_df,Chuback_Proc_df)  #add chuback
grouped_unique_proc_df <- group_codes_into_Ritzwoller_func(grouped_unique_proc_df,Ritzwoller_Proc_df)#add ritzwoller
write.xlsx(grouped_unique_proc_df,paste0(outdir,"Unique_Proc_And_Groups_inALLClaims.xlsx"))

#3. DM3 drug codes
grouped_unique_drug_df <- group_drugcodes_into_DM3_func(unique_drug_df,DM3_df)
write.xlsx(grouped_unique_drug_df,paste0(outdir,"Unique_Drug_And_Groups_inALLClaims.xlsx"))

################################################################################
#4. Report stats for  grouping
################################################################################
report_code_grps_func <- function(in_data, grp_name){
  # in_data <- grouped_unique_diag_df
  # grp_name <- "CCS_CATEGORY"
  
  #Get total number of unique codes
  n_codes <- nrow(in_data)
  
  #Get number of codes have groups
  has_grp_indexes <- which(is.na(in_data[,grp_name])==F)
  n_codes_hasgrps <- length(has_grp_indexes)
  
  #Number of codes have no groups
  n_codes_NOgrps <-  n_codes - n_codes_hasgrps
  
  #Number of groups
  n_grps <-  length(unique(in_data[has_grp_indexes,grp_name]))
  

  
  grp_stats <- cbind.data.frame(n_codes,n_codes_hasgrps,n_codes_NOgrps,n_grps)

  return(grp_stats)
}

#Diag
CCS_stats <- report_code_grps_func(grouped_unique_diag_df,"CCS_CATEGORY")
Chubak_Type_stats <- report_code_grps_func(grouped_unique_diag_df,"Chubak_Type")
Chubak_Category_stats <- report_code_grps_func(grouped_unique_diag_df,"Chubak_Category")
Ritzwoller_Type_stats <- report_code_grps_func(grouped_unique_diag_df,"Ritzwoller_Type")
Ritzwoller_Category_stats <- report_code_grps_func(grouped_unique_diag_df,"Ritzwoller_Category")
Diag_stats <- rbind(CCS_stats,Chubak_Type_stats,Chubak_Category_stats,Ritzwoller_Type_stats,Ritzwoller_Category_stats)
rownames(Diag_stats) <- c("CCS","Chubak_Type","Chubak_Category","Ritzwoller_Type","Ritzwoller_Category")
rownames(Diag_stats) <- paste0("DIAG_",rownames(Diag_stats))
#Proc
CCS_stats <- report_code_grps_func(grouped_unique_proc_df,"CCS_CATEGORY")
Chubak_Type_stats <- report_code_grps_func(grouped_unique_proc_df,"Chubak_Type")
Chubak_Category_stats <- report_code_grps_func(grouped_unique_proc_df,"Chubak_Category")
Ritzwoller_Type_stats <- report_code_grps_func(grouped_unique_proc_df,"Ritzwoller_Type")
Ritzwoller_Category_stats <- report_code_grps_func(grouped_unique_proc_df,"Ritzwoller_Category")
Proc_stats <- rbind(CCS_stats,Chubak_Type_stats,Chubak_Category_stats,Ritzwoller_Type_stats,Ritzwoller_Category_stats)
rownames(Proc_stats) <- c("CCS","Chubak_Type","Chubak_Category","Ritzwoller_Type","Ritzwoller_Category")
rownames(Proc_stats) <- paste0("PROC_",rownames(Proc_stats))

#Drug
DM3_stats1 <- report_code_grps_func(grouped_unique_drug_df,"specific_group")
DM3_stats2 <- report_code_grps_func(grouped_unique_drug_df,"general_group")
Drug_stats <- rbind(DM3_stats1,DM3_stats2)
rownames(Drug_stats) <- c("DM3_specific","DM3_general")


#All stats
all_stats <- rbind(Diag_stats,Proc_stats,Drug_stats)
write.csv(all_stats,paste0(outdir,"Codes_Stats_inALLClaims.csv"))
