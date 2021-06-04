library(parallel)
library(data.table)
library(foreach)
library(doParallel)
library(openxlsx)

#######################################################################################
#1.Process Medicaid health claims
#######################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir1 <- "/recapse/intermediate_data/perPatientData/Medicaid_HealthClaims/"
#Load data
medicaid_health_df <- as.data.frame(fread(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv")))
health_ID <- unique(medicaid_health_df[,"study_id"])
#Get per patient data
registerDoParallel(numCores)  # use multicore, set to the number of our cores
system.time(
foreach (i = 1: length(health_ID)) %dopar% {
     curr_df <- medicaid_health_df[which(medicaid_health_df[,"study_id"] == health_ID[i]),]
     write.xlsx(curr_df,paste0(outdir1,"ID",health_ID[i],"_","all_medicaid_healthclaims.xlsx"))
})

