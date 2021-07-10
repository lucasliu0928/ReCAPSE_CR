library(openxlsx)

get_freq_tb <- function(analysis_code, analysis_col,analysis_uniqueCodes_df){
  total_n_pts <- length(unique(analysis_uniqueCodes_df$ID))
  
  code_freq_df <- as.data.frame(matrix(NA, nrow = length(analysis_code), ncol = 3))
  colnames(code_freq_df) <- c("Code","N_PT_HASCODE","FRAC_PT_HASCODE")
  for (i in 1:length(analysis_code)){
    if(i %% 1000 == 0){print(i)}
    curr_code <- analysis_code[i]
    pts_idxes_hascode <-  which(grepl(paste0("\\b",curr_code,"\\b"),analysis_uniqueCodes_df[,analysis_col])==T)
    n_pts_hascodes <- length(pts_idxes_hascode)
    frac_ots_hascodes <- n_pts_hascodes/total_n_pts
    
    code_freq_df[i,"Code"] <- curr_code
    code_freq_df[i,"N_PT_HASCODE"] <- n_pts_hascodes
    code_freq_df[i,"FRAC_PT_HASCODE"] <- round(frac_ots_hascodes,2)
    
  }
  
  return(code_freq_df)
}


data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/Freq_Table"

#local
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/Freq_Table"

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
noSBCE_uniqueCodes_df <- unique_codes_PerPts_df[which(unique_codes_PerPts_df$ID %in% noSBCE_IDs),]

diag_FQ_tb_SBCE <- get_freq_tb(all_unique_diag_codes,"Unique_Diag",SBCE_uniqueCodes_df)
proc_FQ_tb_SBCE <- get_freq_tb(all_unique_proc_codes,"Unique_Proc",SBCE_uniqueCodes_df)
drug_FQ_tb_SBCE <- get_freq_tb(all_unique_drug_codes,"Unique_Drug",SBCE_uniqueCodes_df)
write.xlsx(diag_FQ_tb_SBCE,paste0(outdir,"diag_SBCE.xlsx"))
write.xlsx(proc_FQ_tb_SBCE,paste0(outdir,"proc_SBCE.xlsx"))
write.xlsx(drug_FQ_tb_SBCE,paste0(outdir,"drug_SBCE.xlsx"))


diag_FQ_tb_noSBCE <- get_freq_tb(all_unique_diag_codes,"Unique_Diag",noSBCE_uniqueCodes_df)
proc_FQ_tb_noSBCE <- get_freq_tb(all_unique_proc_codes,"Unique_Proc",noSBCE_uniqueCodes_df)
drug_FQ_tb_noSBCE <- get_freq_tb(all_unique_drug_codes,"Unique_Drug",noSBCE_uniqueCodes_df)
write.xlsx(diag_FQ_tb_noSBCE,paste0(outdir,"diag_noSBCE.xlsx"))
write.xlsx(proc_FQ_tb_noSBCE,paste0(outdir,"proc_noSBCE.xlsx"))
write.xlsx(drug_FQ_tb_noSBCE,paste0(outdir,"drug_noSBCE.xlsx"))


