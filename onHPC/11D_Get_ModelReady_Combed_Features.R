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
feature_set_name <- "CCSandDM3SPE"  #choose from CCSandDM3SPE , CCSandVAL2nd
data_dir1  <- paste0(proj_dir, "11A_ModelReady_GrpFeature_", feature_set_name,"/WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "11B_ModelReady_CharFeature/WithPossibleMonthsHasNoCodes/")
data_dir3  <- paste0(proj_dir, "11C_ModelReady_TransformFeatures_", feature_set_name,"/WithPossibleMonthsHasNoCodes/")
data_dir4  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

newout <- paste0("11D_ModelReady_CombFatures_",feature_set_name,"/WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)
################################################################################
#1.Load all char feature df
################################################################################
All_Char_df <- read.csv(paste0(data_dir2,"All_Binary_Chars_WithSurgPrimSite_V1.csv"),stringsAsFactors = F, check.names = FALSE)
#All_Char_df <- read.csv(paste0(data_dir2,"All_Binary_Chars_WithSurgPrimSite_V2.csv"),stringsAsFactors = F,check.names = FALSE)


################################################################################
#2.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir4,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1,sep.names = " ")
analysis_IDs <- Final_ID_df[,"study_id"]

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_Comb_Features.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))

if (length(analysis_IDs) > 0){
    ########################################################################################################################
    #For each pt, generate a dataframe with all selected group feature as columns
    ########################################################################################################################
    foreach (i = 1: length(analysis_IDs)) %dopar% {
      curr_id <- analysis_IDs[i]
      
      #groups feature df
      curr_grpfile <- paste0("ID",curr_id,"_Selected_Grp_Features.xlsx")
      curr_grpf_df <- read.xlsx(paste0(data_dir1,curr_grpfile),sheet = 1,sep.names = " ")
      
      #get transformation feature df 
      curr_transffile <- paste0("ID",curr_id,"_Transf_Features.xlsx")
      curr_transf_df <- read.xlsx(paste0(data_dir3,curr_transffile),sheet = 1,sep.names = " ")
      
      #get char feature df
      curr_idxes      <- which(All_Char_df[,"study_id"] == curr_id)
      curr_charf_df   <- All_Char_df[curr_idxes,]
      
      #match all the time rows
      all_time_inOrder <- sort(curr_grpf_df[,"Month_Start"],decreasing = F)
      curr_grpf_df     <- curr_grpf_df[match(all_time_inOrder, curr_grpf_df[,"Month_Start"]),]
      curr_transf_df   <- curr_transf_df[match(all_time_inOrder, curr_transf_df[,"Month_Start"]),]
      curr_charf_df    <- curr_charf_df[match(all_time_inOrder, curr_charf_df[,"Month_Start"]),]
      
      #comb df
      curr_comb_df <- cbind(curr_charf_df,curr_grpf_df,curr_transf_df)
      
      #change rowname to sample ID (ptID + rowID)
      curr_comb_df$sample_id <- paste0("ID",curr_id,"@",all_time_inOrder)
      
      #remove redudant columns,keep the 1st Id and month start
      indexes_toremove <- which(colnames(curr_comb_df) %in% c("study_id","Month_Start"))
      curr_comb_df <- curr_comb_df[, -indexes_toremove]
      
      #reorder column, put sample ID first
      sample_ID_idxes <- ncol(curr_comb_df)
      curr_comb_df <- curr_comb_df[, c(sample_ID_idxes,1:(sample_ID_idxes - 1))]
      write.xlsx(curr_comb_df,paste0(outdir,"ID",curr_id,"_Comb_Features.xlsx"))
      
    }
  
}
