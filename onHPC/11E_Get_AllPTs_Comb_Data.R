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
feature_set_name <- "CCSandVAL2nd"  #choose from CCSandDM3SPE , CCSandVAL2nd
drug_feature_colnames <- "VAL_2ND"    #choose from "DM3_SPE","VAL_2ND"
data_dir  <- paste0(proj_dir, "11D_ModelReady_CombFatures_",feature_set_name, "/WithPossibleMonthsHasNoCodes/")

outdir0 <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/",feature_set_name,"/")

newout <- paste0("11E_AllPTs_ModelReadyData/",feature_set_name,"/All_Samples/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

newout2 <- paste0("11E_AllPTs_ModelReadyData/",feature_set_name,"/Samples_HasAtLeastOneCodeGrpFeature/")
outdir2   <- paste0(proj_dir, newout2)
dir.create(file.path(proj_dir, newout2), recursive = TRUE)
######################################################################################################## 
#1. Load and combine all patient model ready data
######################################################################################################## 
pt_files <-list.files(data_dir,full.names = T)
model_data <- do.call(rbind,mclapply(pt_files, mc.cores= numCores, function(z){read.xlsx(z, sheet = 1,sep.names = " ")}))

#Add a column for original study ID 
original_IDs <- strsplit(model_data$sample_id,split = "@")
model_data$study_id <- sapply(original_IDs, "[[", 1)

#Missingness check
missing_tb <- get_missing_rate_table(model_data,colnames(model_data))
write.csv(missing_tb,paste0(outdir0, "missing_table_before.csv"))
          
#@NOTE The missing for model_data is due to:
#For surg prim site v1: the version does not cover these values:43,44,45,46,47,48,49, 75 and 76
#Since the categorical features created a NA category, so recode the other columns as 0 here
surg_prim_site_features_indexes <- which(grepl("surg_prim_site_V1",colnames(model_data)))
for (j in 1:length(surg_prim_site_features_indexes)){
  curr_f_index <- surg_prim_site_features_indexes[j]
  na_row_index <- which(is.na(model_data[,curr_f_index])==T)
  if (length(na_row_index) > 0){
    model_data[na_row_index,curr_f_index] <- 0
  }
}

#Check Missing again
missing_tb <- get_missing_rate_table(model_data,colnames(model_data))
write.csv(missing_tb,paste0(outdir0, "missing_table_after.csv"))

########################################################################
#'@ADDED091122 Output
#A.Output all samples
#B.Output samples if all diag/proc/drug(DM3 or Val2nd) is not 0
########################################################################
#A.Output all samples (Some samples has code group feature 0)
save(model_data, file=paste0(outdir, "All_PTS_ModelReadyData.rda"))

#B.Output samples if all diag/proc/drug(DM3 or Val2nd) is not 0
drug_grp_count_cols <- colnames(model_data)[grepl(paste0("^", drug_feature_colnames),colnames(model_data))]
diag_grp_count_cols <- colnames(model_data)[grepl("^CCS_DIAG",colnames(model_data))]
proc_grp_count_cols <- colnames(model_data)[grepl("^CCS_PROC",colnames(model_data))]

#Exclude samples has no group count feature
model_data_excluded <- model_data[rowSums(model_data[,c(drug_grp_count_cols,
                                                        diag_grp_count_cols,
                                                        proc_grp_count_cols)]) > 0,] #excluded 329802 in DME3
save(model_data_excluded, file=paste0(outdir2, "All_PTS_ModelReadyData.rda"))

id_df_excluded <- as.data.frame(model_data_excluded[,c("sample_id","study_id")])
write.csv(id_df_excluded,paste0(outdir2, "SampleIDs_HasAtLeastOneCodeGrpFeature.csv"))