source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores()  # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


count_code_freq_patient_level <- function(unique_code_df,perPts_unique_codes_df,code_col,outdir,out_subdir){
  unique_codes <- unique_code_df[,"CODE"]
  
  total_pts <- nrow(perPts_unique_codes_df)
  SBCE_pts_indexes <- which(perPts_unique_codes_df$SBCE==1)
  nonSBCE_pts_indexes <- which(perPts_unique_codes_df$SBCE==0)
  
  code_ct_df <- as.data.frame(matrix(NA, nrow = 1, ncol = ncol(unique_code_df)))
  colnames(code_ct_df) <- colnames(unique_code_df)
  
  code_ct_df$N_PTs_HASCODE <- NA
  code_ct_df$Frac_PTS_HASCODE <- NA
  
  code_ct_df$N_SBCE_PTS_HASCODE <- NA
  code_ct_df$Frac_SBCE_PTS_HASCODE <- NA
  
  code_ct_df$N_nonSBCE_PTS_HASCODE <- NA
  code_ct_df$Frac_nonSBCE_PTS_HASCODE <- NA

  
  foreach (i = 1: length(unique_codes)) %dopar% {
    curr_code <- as.character(unique_codes[i])
    search_string <- paste0("\\b",curr_code,"\\b")
    
    code_ct_df[1,1:ncol(unique_code_df)] <- unique_code_df[i,]
    code_ct_df$CODE <- as.character(code_ct_df$CODE)
    
    #all pts
    code_ct_df[1,"N_PTs_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[,code_col])==T))
    code_ct_df[1,"Frac_PTS_HASCODE"] <- code_ct_df[1,"N_PTs_HASCODE"] /total_pts
    
    #SBCE pts
    code_ct_df[1,"N_SBCE_PTS_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[SBCE_pts_indexes,code_col])==T))
    code_ct_df[1,"Frac_SBCE_PTS_HASCODE"] <-  code_ct_df[1,"N_SBCE_PTS_HASCODE"]/length(SBCE_pts_indexes)
    
    #non_SBCE pts
    code_ct_df[1,"N_nonSBCE_PTS_HASCODE"]    <- length(which(grepl(search_string,perPts_unique_codes_df[nonSBCE_pts_indexes,code_col])==T))
    code_ct_df[1,"Frac_nonSBCE_PTS_HASCODE"] <- code_ct_df[1,"N_nonSBCE_PTS_HASCODE"] /length(nonSBCE_pts_indexes)
    
    write.xlsx(code_ct_df,paste0(outdir,out_subdir, curr_code,".xlsx"))
    
  }
}

#######################################################################
##############              Data dir                     ############## 
#######################################################################
SBCE_dir <- "/recapse/intermediate_data/"
data_dir <- "/recapse/intermediate_data/0_Codes/perPatient_UniqueCodes/"
code_data_dir <- "/recapse/intermediate_data/0_Codes/"
outdir   <- "/recapse/intermediate_data/0_Codes/Count/"
  
# #local
# SBCE_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/perPatient_UniqueCodes/"
# code_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/"
# outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/Count/"

################################################################################
#2.Get unique code df per pts and combine
################################################################################
unique_codes_files <- list.files(data_dir)
perPts_unique_codes_df <- do.call(rbind,lapply(paste0(data_dir,unique_codes_files), read.xlsx))

################################################################################
#3.Get SBCE or not SBCE label
################################################################################
SBCE_df <- read.xlsx(paste0(SBCE_dir,"4_updated_All_event_df.xlsx"),sheet = 1)

################################################################################
#4.Analysis ID
################################################################################
analysis_id <- intersect(SBCE_df$study_id,perPts_unique_codes_df$study_id)

################################################################################
#5. update data for anlaysis ID
################################################################################
SBCE_df <- SBCE_df[which(SBCE_df$study_id %in% analysis_id),]
perPts_unique_codes_df <- perPts_unique_codes_df[which(perPts_unique_codes_df$study_id %in% analysis_id),]

################################################################################
#6. add SBCE label
################################################################################
perPts_unique_codes_df$SBCE <- SBCE_df[match(SBCE_df[,"study_id"],perPts_unique_codes_df[,"study_id"]),"SBCE"]


################################################################################
#3.Get unique code and groups from entire data set
################################################################################
unique_diag_df <- read.csv(paste0(code_data_dir,"Unique_Diag_And_Groups_allpts.csv"),stringsAsFactors = F)
unique_proc_df <- read.csv(paste0(code_data_dir,"Unique_Proc_And_Groups_allpts.csv"),stringsAsFactors = F)
unique_drug_df <- read.csv(paste0(code_data_dir,"Unique_Drug_And_Groups_allpts.csv"),stringsAsFactors = F)

################################################################################
#Count pts who has code for all pts, SBCE pts, and non-SBCE pts
################################################################################
count_code_freq_patient_level(unique_proc_df,perPts_unique_codes_df,"Proc_Codes",outdir,"Unique_Proc_PtsLevelCount2/")

count_code_freq_patient_level(unique_diag_df,perPts_unique_codes_df,"Diag_Codes",outdir,"Unique_Diag_PtsLevelCount2/")

count_code_freq_patient_level(unique_drug_df,perPts_unique_codes_df,"Drug_Codes",outdir,"Unique_Drug_PtsLevelCount2/")

