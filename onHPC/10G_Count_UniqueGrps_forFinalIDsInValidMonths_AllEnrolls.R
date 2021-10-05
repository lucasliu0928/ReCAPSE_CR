source("Recapse_Ultility.R")
get_pts_list_ofgrps <- function(dat_dir,ID_list,file_suffix){
  Unique_Grp_list <- list(NA)
  for (i in 1:length(ID_list)){
    if(i %% 1000 == 0){print(i)}
    curr_id <- ID_list[i]
    curr_file <- paste0("ID",curr_id, file_suffix)
    if (file.exists(paste0(dat_dir,curr_file)) == T){
      curr_df <- read.xlsx(paste0(dat_dir,curr_file),sheet = 1)
    }else{
      curr_df <- NULL
    }
    
    Unique_Grp_list[[i]] <- curr_df[,"unique_grps"]
  }
  return(Unique_Grp_list)
}

count_numPts_forGrpFeatures <- function(Unique_Grp_list,all_unique_grps,diag_disp_df,proc_disp_df,drug_disp_df){
  total_num_pts <- length(Unique_Grp_list)
  
  print(paste("Total Unique Grps:",length(all_unique_grps)))
  print(paste("Total Pts:",total_num_pts))
  
  unique_grp_count_df <- as.data.frame(matrix(NA, nrow = length(all_unique_grps), ncol = 4))
  colnames(unique_grp_count_df) <- c("Code_Grp","Num_PtsHasTheGrp","Frac_PtsHasTheGrp","Grp_Discrip")
  for (i in 1:length(all_unique_grps)){
    if(i %% 50 == 0){print(i)}
    curr_grp <- all_unique_grps[i]
    curr_grp_num <- gsub("CCS_DIAG_|CCS_PROC_|DM3_SPE_","",curr_grp)
    
    #Find the index of list which contians current group
    curr_pts_indexes <- which(sapply(Unique_Grp_list, FUN=function(X) curr_grp %in% X))
    
    unique_grp_count_df[i,"Code_Grp"] <- curr_grp
    unique_grp_count_df[i,"Num_PtsHasTheGrp"]  <- length(curr_pts_indexes)
    unique_grp_count_df[i,"Frac_PtsHasTheGrp"] <- length(curr_pts_indexes) /total_num_pts
    
    if(grepl("CCS_DIAG_",curr_grp)==T){
      curr_idxes <- which(diag_disp_df[,"CCS_CATEGORY"] == curr_grp_num)
      curr_discrp <- unique(diag_disp_df[curr_idxes,"CCS_CATEGORY_DESCRIPTION"])
      #use the longer name to get the longer name if multiple
      if (length(curr_discrp) == 0){
        curr_discrp <- NA
      }else{
        longer_disp_idxes <- which(nchar(curr_discrp) == max(nchar(curr_discrp),na.rm = T))[1]
        curr_discrp <- curr_discrp[longer_disp_idxes]
      }
      
    }else if(grepl("CCS_PROC_",curr_grp)==T){
      curr_idxes <- which(proc_disp_df[,"CCS_CATEGORY"] == curr_grp_num)
      curr_discrp <- unique(proc_disp_df[curr_idxes,"CCS_CATEGORY_DESCRIPTION"])
      if (length(curr_discrp) == 0){
        curr_discrp <- NA
      }else{
        longer_disp_idxes <- which(nchar(curr_discrp) == max(nchar(curr_discrp),na.rm = T))[1]
        curr_discrp <- curr_discrp[longer_disp_idxes]
      }
    }else if(grepl("DM3_SPE_",curr_grp)==T){ #no discrp for drugs
      curr_discrp <- curr_grp
    }
    unique_grp_count_df[i,"Grp_Discrip"] <- curr_discrp
  }
  return(unique_grp_count_df)
}

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
file_suffix <- "_Month_CCS_DIAG_UniqueGrps.xlsx"
data_dir1  <- paste0(proj_dir, "10B_CCSDiagFeature_inValidMonth/WithPossibleMonthsHasNoCodes/UniqueGrp/")
data_dir2  <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")
data_dir3  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "10B_Counts_UniqueCodes_PtsLevel/")



################################################################################
#Unique Groups Per Patient File
################################################################################
unique_grps_files <- list.files(data_dir1)
file_PTs_IDs <- gsub(paste0(file_suffix,"|ID"),"",unique_grps_files) #18239

################################################################################
#'Load pts SBCElabel
################################################################################
SBCE_df <- read.xlsx(paste0(data_dir3,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
SBCE_PTs <- SBCE_df[which(SBCE_df$SBCE==1),"study_id"]    
nonSBCE_PTs <- SBCE_df[which(SBCE_df$SBCE==0),"study_id"] 

################################################################################
#SBCE or nonSBCE pts who has files
################################################################################
Anlaysis_All_PTS <- file_PTs_IDs
Anlaysis_SBCE_PTs <- Anlaysis_All_PTS[which(Anlaysis_All_PTS %in% SBCE_PTs)] #1322
Anlaysis_nonSBCE_PTs <- Anlaysis_All_PTS[which(Anlaysis_All_PTS %in% nonSBCE_PTs)] #16917

################################################################################
#Read unique code discrption file
################################################################################
diag_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Diag_And_Groups_inALLClaims.xlsx"),sheet = 1)
proc_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Proc_And_Groups_inALLClaims.xlsx"),sheet = 1)
drug_disp_df <- read.xlsx(paste0(data_dir2,"Unique_Drug_And_Groups_inALLClaims.xlsx"),sheet = 1)


################################################################################
#Load Pt Ids and list of unique grps for each patients
################################################################################
#For all pts
Unique_Grp_list_ALL <- get_pts_list_ofgrps(data_dir1,Anlaysis_All_PTS,file_suffix)
all_unique_grps_ALL <- unique(unlist(Unique_Grp_list_ALL))

#For SBCE pts
Unique_Grp_list_SBCE <- get_pts_list_ofgrps(data_dir1,Anlaysis_SBCE_PTs,file_suffix)

#For nonSBCE pts
Unique_Grp_list_nonSBCE <- get_pts_list_ofgrps(data_dir1,Anlaysis_nonSBCE_PTs,file_suffix)

################################################################################
#For each unique grps feature in all data, count the number of pts who has it
################################################################################
unique_grp_count_df_SBCE <- count_numPts_forGrpFeatures(Unique_Grp_list_SBCE,all_unique_grps_ALL,diag_disp_df,proc_disp_df,drug_disp_df)
colnames(unique_grp_count_df_SBCE) <- paste0(colnames(unique_grp_count_df_SBCE), "_SBCE")

unique_grp_count_df_nonSBCE <- count_numPts_forGrpFeatures(Unique_Grp_list_nonSBCE,all_unique_grps_ALL,diag_disp_df,proc_disp_df,drug_disp_df)
colnames(unique_grp_count_df_nonSBCE) <- paste0(colnames(unique_grp_count_df_nonSBCE),"_nonSBCE")

comb_count_df <- cbind(unique_grp_count_df_SBCE,unique_grp_count_df_nonSBCE)

write.xlsx(comb_count_df,paste0(outdir,"Count_Unique_Grps_WithPossibleMonthsHasNoCodes.xlsx"))

################################################################################
#Select Final grps for model ready data
################################################################################
selected_index <- which(comb_count_df[,"Frac_PtsHasTheGrp_SBCE"] > 0.1 | comb_count_df[,"Frac_PtsHasTheGrp_nonSBCE"] > 0.15)
Final_selected_grps_df <- comb_count_df[selected_index,"Code_Grp_SBCE"]

write.xlsx(Final_selected_grps_df,paste0(outdir,"Selected_Unique_Grps_WithPossibleMonthsHasNoCodes.xlsx"))
