library(parallel)
library(data.table)
library(openxlsx)


numCores <- detectCores() # get the number of cores available
print(numCores)
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


# 
# #local
# data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/1_ID_Sources_Info"

#######################################################################################
#Get all study id in kcr 
#######################################################################################
kcr_df <- as.data.frame(fread(paste0(data_dir,"uh3_kcrdata.csv")))
kcr_ID <- unique(kcr_df[,"study_id"]) #47130

#######################################################################################
#Read Medicare
#######################################################################################
medicare_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv")))
medicare_IDs <- unique(medicare_df[,"study_id"])

#######################################################################################
#Read medicaid
#######################################################################################
medicaid_health_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv")))
medicaid_pharm_df <- as.data.frame(fread(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv")))
health_ID <- unique(medicaid_health_df[,"study_id"])
pharm_ID <- unique(medicaid_pharm_df[,"study_id"])
medicaid_IDs <- union(health_ID,pharm_ID) #take the union, some pts has no health claims, some has no pharm claim

#######################################################################################
##Combine all ID
#######################################################################################
ALLID_df <- as.data.frame(matrix(NA, nrow = length(kcr_ID),ncol = 3))
colnames(ALLID_df) <- c("Kcr_ID","in_Medicare","in_Medicaid")
for (i in 1:length(kcr_ID)){
  curr_id <- kcr_ID[i]
  ALLID_df[i,"Kcr_ID"] <- curr_id
  
  #in medicare
  if (curr_id %in% medicare_IDs){
    ALLID_df[i,"in_Medicare"] <- 1
  }else{
    ALLID_df[i,"in_Medicare"] <- 0
  }
  
  #in medicaid
  if (curr_id %in% medicaid_IDs){
    ALLID_df[i,"in_Medicaid"] <- 1
  }else{
    ALLID_df[i,"in_Medicaid"] <- 0
  }
}

write.xlsx(ALLID_df,paste0(outdir,"All_ID_Source.xlsx"))

