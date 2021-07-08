library(openxlsx)
library(parallel)
library(foreach)
library(doParallel)

numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/Freq_SBCE/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/Freq_SBCE/"

#########################################################################################################
#Load unique Codes per patient df
#########################################################################################################
unique_codes_PerPts_df <- read.xlsx(paste0(data_dir,"AllPatient_UniqueCode_inValidMonth.xlsx"),sheet = 1)

exclude_idx <- which(is.na(unique_codes_PerPts_df$Unique_Diag) == T & 
                       is.na(unique_codes_PerPts_df$Unique_Proc) == T &
                       is.na(unique_codes_PerPts_df$Unique_Drug) == T)
unique_codes_PerPts_df <- unique_codes_PerPts_df[-exclude_idx,]
length(unique(unique_codes_PerPts_df$ID))

#########################################################################################################
#Load pateint event type and date data (Outcome)
#########################################################################################################
All_event_df <- read.csv(paste0(data_dir,"updated_All_event_df.csv"),stringsAsFactors = F)
#1.Get number of IDs in event type files
event_type_IDs <- unique(All_event_df$ID) #40329

#########################################################################################################
#Anlysisi ID
#########################################################################################################
analysis_ID <- unique(intersect(event_type_IDs,unique_codes_PerPts_df$ID))

#########################################################################################################
#updated outcome for analaysis ID
#########################################################################################################
All_event_df <- All_event_df[which(All_event_df$ID %in% analysis_ID),]
SBCE_IDs <- All_event_df[which(All_event_df$SBCE==1),"ID"]
noSBCE_IDs <- All_event_df[which(All_event_df$SBCE==0),"ID"]

#########################################################################################################
##Get All unique codes for all analysis ID
#########################################################################################################
unique_codes_PerPts_df <- unique_codes_PerPts_df[which(unique_codes_PerPts_df$ID %in% analysis_ID),]
all_unique_diag_codes <- unique(unlist(strsplit(unique_codes_PerPts_df$Unique_Diag,split = "$$$$",fixed = T)))
all_unique_proc_codes <- unique(unlist(strsplit(unique_codes_PerPts_df$Unique_Proc,split = "$$$$",fixed = T)))
all_unique_drug_codes <- unique(unlist(strsplit(unique_codes_PerPts_df$Unique_Drug,split = "$$$$",fixed = T)))

#########################################################################################################
#Count unique code freq (# of patient has the code in valid month)
#########################################################################################################
SBCE_uniqueCodes_df <- unique_codes_PerPts_df[which(unique_codes_PerPts_df$ID %in% SBCE_IDs),]
#noSBCE_uniqueCodes_df <- unique_codes_PerPts_df[which(unique_codes_PerPts_df$ID %in% noSBCE_IDs),]
total_n_pts <- length(unique(SBCE_uniqueCodes_df$ID))

# Diag
foreach (i = 1: length(all_unique_diag_codes)) %dopar% {
  curr_code <- all_unique_diag_codes[i]
  pts_idxes_hascode <-  which(grepl(paste0("\\b",curr_code,"\\b"),SBCE_uniqueCodes_df[,"Unique_Diag"])==T)
  n_pts_hascodes <- length(pts_idxes_hascode)
  frac_ots_hascodes <- n_pts_hascodes/total_n_pts
  
  code_freq_df <- cbind.data.frame(curr_code,n_pts_hascodes,round(frac_ots_hascodes,2))
  colnames(code_freq_df) <- c("Code","N_PT_HASCODE","FRAC_PT_HASCODE")
  write.xlsx(code_freq_df,paste0(outdir,"diag/","CODE",curr_code,"_diag_SBCE.xlsx"))
}


# Proc
foreach (i = 1: length(all_unique_proc_codes)) %dopar% {
  curr_code <- all_unique_proc_codes[i]
  pts_idxes_hascode <-  which(grepl(paste0("\\b",curr_code,"\\b"),SBCE_uniqueCodes_df[,"Unique_Proc"])==T)
  n_pts_hascodes <- length(pts_idxes_hascode)
  frac_ots_hascodes <- n_pts_hascodes/total_n_pts
  
  code_freq_df <- cbind.data.frame(curr_code,n_pts_hascodes,round(frac_ots_hascodes,2))
  colnames(code_freq_df) <- c("Code","N_PT_HASCODE","FRAC_PT_HASCODE")
  write.xlsx(code_freq_df,paste0(outdir,"proc/","CODE",curr_code,"_proc_SBCE.xlsx"))
}

#durg
foreach (i = 1: length(all_unique_drug_codes)) %dopar% {
  curr_code <- all_unique_drug_codes[i]
  pts_idxes_hascode <-  which(grepl(paste0("\\b",curr_code,"\\b"),SBCE_uniqueCodes_df[,"Unique_Drug"])==T)
  n_pts_hascodes <- length(pts_idxes_hascode)
  frac_ots_hascodes <- n_pts_hascodes/total_n_pts
  
  code_freq_df <- cbind.data.frame(curr_code,n_pts_hascodes,round(frac_ots_hascodes,2))
  colnames(code_freq_df) <- c("Code","N_PT_HASCODE","FRAC_PT_HASCODE")
  write.xlsx(code_freq_df,paste0(outdir,"drug/","CODE",curr_code,"_drug_SBCE.xlsx"))
}
