source("Recapse_Ultility.R")

data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/3A_perDay_PerPatientData/"

# local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/3A_perDay_PerPatientData/"

#######################################################################################
#Load IDs
#######################################################################################
ID_df <- read.xlsx(paste0(data_dir,"All_ID_Source.xlsx"),sheet = 1)
#remove the IDs does not have any claims
ID_df <- ID_df[-which(ID_df[,"in_Medicare"] == 0 & ID_df[,"in_Medicaid"] == 0),]

#get analysis ID by source
analysis_ID <- unique(ID_df$Kcr_ID)
medicare_only_ID <- ID_df[which(ID_df[,"in_Medicare"] == 1 & ID_df[,"in_Medicaid"] == 0),"Kcr_ID"]
medicaid_only_ID <- ID_df[which(ID_df[,"in_Medicare"] == 0 & ID_df[,"in_Medicaid"] == 1 ),"Kcr_ID"]
both_ID <- ID_df[which(ID_df[,"in_Medicare"] == 1 & ID_df[,"in_Medicaid"] == 1 ),"Kcr_ID"]

#######################################################################################
#Get unique codes per day from different sources and combine them 
#######################################################################################
medicaid_heath_dir <- paste0(data_dir, "perPatientData/Medicaid_HealthClaims/")
medicaid_pharm_dir <- paste0(data_dir, "perPatientData/Medicaid_PharmClaims/")
medicare_dir <- paste0(data_dir, "perPatientData/Medicare/")

#######################################################################################
#1.Get per day data
#######################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

IDs_processed <-  as.numeric(gsub("_perDay_Data.xlsx|ID","",list.files(outdir)))

analysis_ID <- analysis_ID[-which(analysis_ID %in% IDs_processed)]
print(length(analysis_ID))
foreach (i = 1: length(analysis_ID)) %dopar% {
  curr_id <- analysis_ID[i]
  
  if (curr_id %in% medicare_only_ID){ #medicare
    curr_perDay_data <- get_perDay_medicare(curr_id,medicare_dir)
    
  }else if (curr_id %in% medicaid_only_ID){ #medcaid
    curr_perDay_data <- get_perDay_medicaid(curr_id,medicaid_heath_dir,medicaid_pharm_dir)
    
  }else if (curr_id %in% both_ID){# from two source and combine
    curr_perDay_data <- get_perDay_both(curr_id,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir) #298
  }
  
  write.xlsx(curr_perDay_data,paste0(outdir,"ID",curr_id,"_","perDay_Data.xlsx"))
}
