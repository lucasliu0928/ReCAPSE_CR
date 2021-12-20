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
data_dir2   <- paste0(proj_dir, "12D_ExclusionSamples/WithPossibleMonthsHasNoCodes/Test/")
data_dir3        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1217updated/Use_ImportantFs_Performance/")

outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1217updated/Use_ImportantFs_Performance/")


#User input
ds_index <- 1

################################################################################
#2. Load test data
################################################################################
load(file = paste0(data_dir1, "test_data.rda"))

################################################################################
#3.Load obv neg,pos and non obv sample IDs
################################################################################
test_data_neg <- read.csv(paste0(data_dir2,"ObviousNeg_Samples_Test.csv"),stringsAsFactors = F)
test_data_pos <- read.csv(paste0(data_dir2,"ObviousPos_Samples_Test.csv"),stringsAsFactors = F)
test_data_nonobv <- read.csv(paste0(data_dir2,"NON_Obvious_Samples_Test.csv"),stringsAsFactors = F)

neg_ids <- test_data_neg$sample_id
pos_ids <- test_data_pos$sample_id
nonobv_ids <- test_data_nonobv$sample_id

################################################################################
#3.Prediction method 1, use optimal model
################################################################################
test_prediction_df <- read.csv(paste0(data_dir3,"train_DS",ds_index,"/BeforeSmoothed", "/16_Prediction_Table_DS", ds_index, ".csv"))
colnames(test_prediction_df)[2] <- "Pred_UseOptimalModel"

################################################################################
#4.Add Prediction method 2 results, 
#1. predicted as 0 or 1 for neg and pos
#2. keep model prediction for non-obv
################################################################################
test_prediction_df[,"pred"] <- NA
test_prediction_df[which(test_prediction_df$sample_id %in% neg_ids),"pred"] <- 0
test_prediction_df[which(test_prediction_df$sample_id %in% pos_ids),"pred"] <- 1
test_prediction_df[which(test_prediction_df$sample_id %in% nonobv_ids),"pred"] <- test_prediction_df[which(test_prediction_df$sample_id %in% nonobv_ids),"Pred_UseOptimalModel"]


write.csv(test_prediction_df,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed/","/16_Updated_Prediction_Table_DS",ds_index,".csv"),row.names = F)

