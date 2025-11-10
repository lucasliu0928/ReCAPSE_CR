library(parallel)
library(data.table)
library(foreach)
library(doParallel)
library(openxlsx)

#######################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores
#######################################################################################


#######################################################################################
#1.Process Medicaid health claims
#######################################################################################
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/2_RawClaims_perPatient/Medicaid_HealthClaims/"

#Load data
medicaid_health_df <- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
health_ID <- unique(medicaid_health_df[,"study_id"])
#Get per patient data

system.time(
foreach (i = 1: length(health_ID)) %dopar% {
     curr_df <- medicaid_health_df[which(medicaid_health_df[,"study_id"] == health_ID[i]),]
     write.xlsx(curr_df,paste0(outdir,"ID",health_ID[i],"_","all_medicaid_healthclaims.xlsx"))
})

