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
#1.Process Medicaid pharm claims
#######################################################################################
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/2_RawClaims_perPatient/Medicaid_PharmClaims/"
#Load data
medicaid_pharm_df <- read.csv(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv"),stringsAsFactors = F)
pharm_ID <- unique(medicaid_pharm_df[,"study_id"])

#Get per patient data
system.time(
  foreach (i = 1: length(pharm_ID)) %dopar% {
    curr_df <- medicaid_pharm_df[which(medicaid_pharm_df[,"study_id"] == pharm_ID[i]),]
    write.xlsx(curr_df,paste0(outdir,"ID",pharm_ID[i],"_","all_medicaid_pharmclaims.xlsx"))
  })
