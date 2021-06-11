library(openxlsx)
library(data.table)

data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/perPatientData/Medicare/"

#######################################################################################
#Process Medicare, get per patient data as seperated files
#######################################################################################
data_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv")))
unique_IDs <- unique(data_df[,"study_id"])

system.time(
for (i in 1:length(unique_IDs)){
curr_df <- data_df[which(data_df[,"study_id"] == unique_IDs[i]),]
write.xlsx(curr_df,paste0(outdir,"ID",unique_IDs[i],"_","all_medicare_claims.xlsx"))
}
)
