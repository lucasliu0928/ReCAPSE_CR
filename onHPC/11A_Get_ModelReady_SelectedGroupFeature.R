source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "10A_CCSDiagProcF_And_DM3SPEF_inValidMonth/WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
data_dir3  <- paste0(proj_dir, "10B_Counts_UniqueCodes_PtsLevel/")

outdir   <- paste0(proj_dir, "11A_ModelReady_GrpFatures/WithPossibleMonthsHasNoCodes/")


################################################################################
#1.get original per months grp feature files
################################################################################
perMonth_grpfeature_files_original <- list.files(data_dir1)

################################################################################
#2.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir2,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

################################################################################
#3.Final model ready grps
################################################################################
modelready_grps_df <- read.xlsx(paste0(data_dir3,"Selected_Unique_Grps_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1,colNames = F)
modelready_grps_features <- sort(modelready_grps_df[,1])

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_Selected_Grp_Features.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))


########################################################################################################################
#For each pt, generate a dataframe with all selected group feature as columns
########################################################################################################################
foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_Month_Grp_Feature.xlsx")
  
  #old per month groups df
  old_perMonth_df <- read.xlsx(paste0(data_dir1,curr_file),sheet = 1)
  
  #get study_id list
  old_study_id <- old_perMonth_df[,"study_id"]
  
  #get month start list
  old_Month_Start <- old_perMonth_df[,"Month_Start"]
  
  #update old grps feature df only contains features
  remove_idxes      <- which(colnames(old_perMonth_df) %in% c("Enrolled_Month","study_id","Month_Start","Month_End"))
 
  #if old feature df only has one grp feature ncol=5, then after remove, only one column left, make sure it is a dataframe
  if (ncol(old_perMonth_df) == 5){
   left_feature_name <- colnames(old_perMonth_df)[-remove_idxes]
   old_feature_df     <- as.data.frame(old_perMonth_df[,-remove_idxes])
   colnames(old_feature_df) <- left_feature_name
  }else{
    old_feature_df    <- old_perMonth_df[,-remove_idxes]
  }
  

  #only keep old features that are in select groups
  kept_idexes <- which(colnames(old_feature_df) %in% modelready_grps_features)
  if (length(kept_idexes) > 0){
    if (ncol(old_perMonth_df) == 5){
      kept_feature_name <- colnames(old_feature_df)[kept_idexes]
      updated_old_feature_df <- as.data.frame(old_feature_df[,kept_idexes])
      colnames(updated_old_feature_df) <- kept_feature_name
      updated_old_features   <- colnames(updated_old_feature_df)
    }else{
      updated_old_feature_df <- old_feature_df[,kept_idexes]
      updated_old_features   <- colnames(updated_old_feature_df)
    }
  }else{
    updated_old_feature_df <- NULL
    updated_old_features <- NULL
  }
  #new per month groups using all selected grps
  #All entries are initilized with 0
  new_perMonth_df <- as.data.frame(matrix(0, nrow = nrow(old_perMonth_df), ncol = length(modelready_grps_features) + 2))
  colnames(new_perMonth_df) <- c("study_id","Month_Start",modelready_grps_features)

  new_perMonth_df[,"study_id"]    <- old_study_id 
  new_perMonth_df[,"Month_Start"] <- old_Month_Start
    
  #All entires matched with old ones, fill as the old ones, if none of the old ones are selected, then all 0s
  new_perMonth_df[,updated_old_features] <- updated_old_feature_df
  
  write.xlsx(new_perMonth_df,paste0(outdir,"ID",curr_id,"_Selected_Grp_Features.xlsx"))
  
}
