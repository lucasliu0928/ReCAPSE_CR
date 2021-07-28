source("Recapse_Ultility.R")

#onHPC
grp_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# #local
# grp_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

################################################################################ 
#1. Load unique codes 
################################################################################ 
#1.Load unique codes
unique_diag_df <- read.csv(paste0(data_dir,"11_all_unique_diag_codes_df.csv"),stringsAsFactors = F)
unique_proc_df <- read.csv(paste0(data_dir,"11_all_unique_proc_codes_df.csv"),stringsAsFactors = F)
unique_drug_df <- read.csv(paste0(data_dir,"11_all_unique_drug_codes_df.csv"),stringsAsFactors = F)

########################################################################   
######                2. load HCUP CCS file:                    ########
########################################################################  
CCS_df <- load_and_clean_CSS_data(grp_dir)
CCS_Diag_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Diag","ICD10_Diag")),]
CCS_Proc_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Proc","ICD10_Proc")),]


########################################################################  
#3. Group  diagnose codes
########################################################################  
grouped_unique_diag_df <- group_codes_into_CCS_func(unique_diag_df,CCS_Diag_df)
length(which(is.na(grouped_unique_diag_df$CCS_CATEGORY)==T)) #1594 no grps
length(unique(grouped_unique_diag_df$CCS_CATEGORY)) #265 grps

write.csv(grouped_unique_diag_df,paste0(outdir,"11_grouped_unique_diag_df.csv"),row.names = F)

########################################################################  
#4. Group  procedure codes
########################################################################  
grouped_unique_proc_df <- group_codes_into_CCS_func(unique_proc_df,CCS_Proc_df)
length(which(is.na(grouped_unique_proc_df$CCS_CATEGORY)==T)) #10811 no grps
length(unique(grouped_unique_proc_df$CCS_CATEGORY)) #216 grps

write.csv(grouped_unique_proc_df,paste0(outdir,"11_grouped_unique_proc_df.csv"),row.names = F)


######################################################################## 
## 5. Group Drug codes
######################################################################## 
#1.Load drug group df
DM3_df <- read.csv(paste0(grp_dir,"Code_Groups/Drug Code Groups-DM3.sorted.csv"),stringsAsFactors = F)
DM3_df <- DM3_df[,-1]

# #Clean drug name by removing the source prefix
DM3_df[,"desc"] <- gsub("NC: |NH: |NO: |NS: ","",DM3_df[,"desc"])
DM3_df[,"desc"] <- gsub("[[:punct:]]"," ",DM3_df[,"desc"])
DM3_df[,"desc"] <- trimws(DM3_df[,"desc"], which = c("both"), whitespace = "[ \t\r\n]")

unique_drug_df[,"Drug_name"] <- gsub("[[:punct:]]"," ",unique_drug_df[,"Drug_name"])
unique_drug_df[,"Drug_name"] <- trimws(unique_drug_df[,"Drug_name"], which = c("both"), whitespace = "[ \t\r\n]")

#group drug codes
grouped_unique_drug_df <- group_drugcodes_into_DM3_func(unique_drug_df,DM3_df)

length(which(is.na(grouped_unique_drug_df$specific_group)==T)) # 25798 no grps
length(which(is.na(grouped_unique_drug_df$general_group)==T)) # 25798 no grps
length(unique(grouped_unique_drug_df$specific_group)) #58 grps
length(unique(grouped_unique_drug_df$general_group)) #14 grps

write.csv(grouped_unique_drug_df,paste0(outdir,"11_grouped_unique_drug_df.csv"),row.names = F)
