source("Recapse_Ultility.R")
#This script update test prediction by assign prediciton of obv neg as 0
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
data_dir1        <- paste0(proj_dir, "15_XGB_Input/")
data_dir2        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/Use_ImportantFs_Performance/")
outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/Use_ImportantFs_Performance/")


#User input
ds_index <- 1

################################################################################
#2. Load test data
################################################################################
load(file = paste0(data_dir1, "test_data.rda"))


################################################################################
#3. Identify obv neg Use criteria find by train data 
################################################################################
best_th1 <- 5.5
best_th2<-  39
final_sample_idxes <- which(test_data[,"cumul_ratio_CCS_PROC_202"] == -1 |
                              test_data[,"cumul_ratio_CCS_PROC_227"] > best_th1 | 
                              test_data[,"months_since_dx"] < best_th2 )
obv_neg_IDs <- test_data[final_sample_idxes,"sample_id"]


################################################################################
#3. Load current prediction 
################################################################################
test_prediction_df <- read.csv(paste0(data_dir2,"train_DS",ds_index,"/BeforeSmoothed", "/16_Prediction_Table_DS", ds_index, ".csv"))

################################################################################
#4.Updated obv neg prediction as 0
################################################################################
ob_neg_indexes <- which(test_prediction_df$sample_id %in% obv_neg_IDs)
test_prediction_df[ob_neg_indexes,"pred"] <- 0

write.csv(test_prediction_df,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/16_Updated_Prediction_Table_DS",ds_index,".csv"),row.names = F)

