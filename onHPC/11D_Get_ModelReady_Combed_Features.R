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
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "11A_ModelReady_GrpFatures/WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "11B_ModelReady_CharFatures/WithPossibleMonthsHasNoCodes/")
data_dir3  <- paste0(proj_dir, "11C_ModelReady_TransformFatures/WithPossibleMonthsHasNoCodes/")
data_dir4  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "11D_ModelReady_CombFatures/WithPossibleMonthsHasNoCodes/")

################################################################################
#1.Load all char feature df
################################################################################
All_Char_df <- read.csv(paste0(data_dir2,"All_Binary_Chars.csv"),stringsAsFactors = F)

################################################################################
#1.get grp feature and trans feature files
################################################################################
grpfeature_files <- list.files(data_dir1)
transfeature_files <- list.files(data_dir2)
################################################################################
#2.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir4,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_Comb_Features.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))


########################################################################################################################
#For each pt, generate a dataframe with all selected group feature as columns
########################################################################################################################
foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_Selected_Grp_Features.xlsx")
  
  #groups feature df
  curr_grp_f_df <- read.xlsx(paste0(data_dir1,curr_file),sheet = 1)
  
  #get transformation data
  system.time(curr_transf_df <- apply_code_transforamtion_func(curr_grp_f_df))
  
  #remove redudant month index and others from three transforamtion func
  index_toremove <- which(colnames(curr_transf_df) %in% c("study_id","Month_Start","Month_Index"))
  index_toremove <- index_toremove[3:length(index_toremove)]
  curr_transf_df <- curr_transf_df[,-index_toremove]
  
  write.xlsx(curr_transf_df,paste0(outdir,"ID",curr_id,"_Transf_Features.xlsx"))
  
}
