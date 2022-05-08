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
data_dir  <- paste0(proj_dir, "11D_ModelReady_CombFatures_WithSurgPrimSite_V1/WithPossibleMonthsHasNoCodes/")

newout <- "11E_AllPTs_ModelReadyData/WithPossibleMonthsHasNoCodes/"
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

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
write.csv(missing_tb,paste0(outdir, "missing_table_before.csv"))
          
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
write.csv(missing_tb,paste0(outdir, "missing_table_after.csv"))

#Output
save(model_data, file=paste0(outdir, "All_PTS_ModelReadyData.rda"))
