library(openxlsx)
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/2_RawClaims_perPatient/Medicare/"

# #Local 
# data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/2_RawClaims_perPatient/Medicare/"


#######################################################################################
#Process Medicare, get per patient data as seperated files
#######################################################################################
data_df <- read.csv(paste0(data_dir,"kcr_medicare_claims_fb0015.csv"),stringsAsFactors = F)
unique_IDs <- unique(data_df[,"study_id"])
print(length(unique_IDs))

ID_processed <- as.numeric(gsub("_all_medicare_claims.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  unique_IDs <- unique_IDs[-which(unique_IDs %in% ID_processed)]
}
print(length(unique_IDs))

for (i in 1:length(unique_IDs)){ #Can't use foreach, error produced 
  curr_df <- data_df[which(data_df[,"study_id"] == unique_IDs[i]),]
  write.xlsx(curr_df,paste0(outdir,"ID",unique_IDs[i],"_","all_medicare_claims.xlsx"))
}
