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
data_dir_diag  <- paste0(proj_dir, "10B_CCSDiagFeature_inValidMonth/WithPossibleMonthsHasNoCodes/Feature/")
data_dir_proc  <- paste0(proj_dir, "10C_CCSProcFeature_inValidMonth/WithPossibleMonthsHasNoCodes/Feature/")
data_dir_drug  <- paste0(proj_dir, "10F2_VAL2NDFeature_inValidMonth/WithPossibleMonthsHasNoCodes/Feature/")

data_dir2  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
data_dir3  <- paste0(proj_dir, "10H_Selected_Grps/WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, "11A_ModelReady_GrpFeature/WithPossibleMonthsHasNoCodes/")

################################################################################
#1.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir2,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

################################################################################
#2.Final model ready grps
################################################################################
modelready_grps_df1 <- read.xlsx(paste0(data_dir3,"Selected_CCSDiag_Unique_Grps.xlsx"),sheet = 1)
modelready_grps_df2 <- read.xlsx(paste0(data_dir3,"Selected_CCSProc_Unique_Grps.xlsx"),sheet = 1)
modelready_grps_df3 <- read.xlsx(paste0(data_dir3,"Selected_VAL2ndDrug_Unique_Grps.xlsx"),sheet = 1)

modelready_Diag_features <- sort(modelready_grps_df1[,1])
modelready_Proc_features <- sort(modelready_grps_df2[,1])
#modelready_Drug_features <- sort(modelready_grps_df3[,1])
#'@NIMPORTANT #fix the issue of colume names conversion replace space with "." when read xlsx
modelready_Drug_features <- gsub(" ",".",sort(modelready_grps_df3[,1])) 

#All features
modelready_grps_features <- sort(c(modelready_Diag_features,modelready_Proc_features,modelready_Drug_features))
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
  
  curr_file1 <- paste0("ID",curr_id,"_Month_CCS_DIAG_Feature.xlsx")
  curr_file2 <- paste0("ID",curr_id,"_Month_CCS_PROC_Feature.xlsx")
  curr_file3 <- paste0("ID",curr_id,"_Month_VAL_2ND_Feature.xlsx")

  #old per month groups df
  old_perMonth_df1 <- read.xlsx(paste0(data_dir_diag,curr_file1),sheet = 1)
  old_perMonth_df2 <- read.xlsx(paste0(data_dir_proc,curr_file2),sheet = 1)
  old_perMonth_df3 <- read.xlsx(paste0(data_dir_drug,curr_file3),sheet = 1)
  
  #Match rows
  old_perMonth_df2 <- old_perMonth_df2[match(old_perMonth_df2[,"Enrolled_Month"],old_perMonth_df1[,"Enrolled_Month"]),]
  old_perMonth_df3 <- old_perMonth_df3[match(old_perMonth_df3[,"Enrolled_Month"],old_perMonth_df1[,"Enrolled_Month"]),]
  
  #Comm three df
  old_perMonth_df <- cbind(old_perMonth_df1,old_perMonth_df2,old_perMonth_df3)
  
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

