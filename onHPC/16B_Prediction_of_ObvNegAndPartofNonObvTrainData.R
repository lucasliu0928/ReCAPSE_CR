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
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/Use_ImportantFs_Performance/")
data_dir2        <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/WithPossibleMonthsHasNoCodes/")
data_dir3        <- paste0(proj_dir,"12D_ExclusionSamples/WithPossibleMonthsHasNoCodes/")


outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/Prediction_ObvNegAndPartOfNonObv_TrainData/")
ds_index <- 1

################################################################################
#1. Load optimal model parameters
################################################################################
#1A. Get optimal model features
mod_optimal <- readRDS(paste0(data_dir1,"train_DS",ds_index, "/BeforeSmoothed/model", ds_index, ".rds"))
features    <- mod_optimal$feature_names
#2A. Get optiaml model paramters
optimal_para <- read.csv(paste0(data_dir1,"train_DS",ds_index, "/BeforeSmoothed/" , "16_OptimalModelParam.csv"),stringsAsFactors = F)

################################################################################
#2. Load all model data
################################################################################
load(file = paste0(data_dir2, "All_PTS_ModelReadyData.rda"))

################################################################################ 
#3.Get obvious neg data as part of validation
################################################################################ 
obv_neg_train_ID_df   <- read.csv(paste0(data_dir3,"ObviousNeg_Samples.csv"),stringsAsFactors = F)
part_validation_ID1    <- obv_neg_train_ID_df[,"sample_id"]
valid_data1 <- model_data[which(model_data$sample_id %in% part_validation_ID1),]

################################################################################ 
#4.From non-obvious neg data, get 90% for train, 10% for validation
################################################################################ 
nonobv_neg_train_ID_df   <- read.csv(paste0(data_dir3,"non_ObviousNeg_Samples.csv"),stringsAsFactors = F)
train_sampleID_nonobv    <- nonobv_neg_train_ID_df[,"sample_id"]

#90% for re-train, #10% for part of validation
total_n <- length(train_sampleID_nonobv)
set.seed(123)
part_validation_ID2   <- sample(train_sampleID_nonobv,0.1*total_n)
valid_data2 <- model_data[which(model_data$sample_id %in% part_validation_ID2),]

train_ID        <- train_sampleID_nonobv[-which(train_sampleID_nonobv %in% part_validation_ID2)]
train_data      <- model_data[which(model_data$sample_id %in% train_ID),]

################################################################################ 
#clear memory
################################################################################
rm("model_data")
gc()

################################################################################ 
#Combine non-obvious validation and obious validation data
################################################################################ 
all_valid_data <- rbind(valid_data1,valid_data2)


################################################################################ 
#xgb input : train and valid data
################################################################################ 
train_label      <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
train_data_part  <- train_data[,features] 
dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

#All validation
valid_label       <- as.numeric(all_valid_data[,"y_PRE_OR_POST_2ndEvent"])
valid_data_part   <- all_valid_data[,features] 
dvalid            <- xgb.DMatrix(data = as.matrix(valid_data_part), label = valid_label)

#non-obv validatiion 
valid_label_nonob       <- as.numeric(valid_data2[,"y_PRE_OR_POST_2ndEvent"])
valid_data_part_nonob   <- valid_data2[,features] 
dvalid_nonob            <- xgb.DMatrix(data = as.matrix(valid_data_part_nonob), label = valid_label_nonob)


################################################################################ 
#re-train
################################################################################
optimal_para_list <- list(etc = as.numeric(optimal_para['etc']),
                     max_depth = as.numeric(optimal_para['max_depth']),
                     min_child_weight = as.numeric(optimal_para['min_child_weight']),
                     subsample = as.numeric(optimal_para['subsample']),
                     colsample_by_tree = as.numeric(optimal_para['colsample_by_tree']))

new_mod <- xgb.train(objective="binary:logistic",
                     params=optimal_para_list, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                     watchlist= list(train = dtrain, eval = dvalid), verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")




################################################################################ 
#5.Prediction for all validation data:
#Method 1: predict use optimal model for all samples
#Method 2: predict as negative for all obvious neg samples + model to predict non-obvious samples
################################################################################ 
pred_df <- as.data.frame(matrix(NA, nrow = nrow(all_valid_data),ncol = 5))
colnames(pred_df) <- c("study_id","sample_id","y_PRE_OR_POST_2ndEvent","pred_asNEG_and_model","pred_onlyuseOptimalModel")
pred_df$study_id                  <- all_valid_data$study_id
pred_df$sample_id                 <- all_valid_data$sample_id
pred_df$y_PRE_OR_POST_2ndEvent    <- all_valid_data$y_PRE_OR_POST_2ndEvent

#method 1:
pred_df[,"pred_onlyuseOptimalModel"] <- predict(new_mod, dvalid)

#method 2:
#for non-obv neg, use the model
pred_nonob <- predict(new_mod, dvalid_nonob)
pred_nonob_tb <- data.frame("sample_id" = valid_data2[,"sample_id"] ,"pred"= pred_nonob )

#for obvious neg, assign 0 
pred_ob_tb <- data.frame("sample_id" = valid_data1[,"sample_id"] ,"pred"= 0)

#final pred
pred_all <- rbind(pred_ob_tb,pred_nonob_tb)

#match id order
pred_all <- pred_all[match(pred_df[,"sample_id"],pred_all[,"sample_id"]),]
pred_df[,"pred_asNEG_and_model"] <- pred_all[,"pred"]

write.csv(pred_df,paste0(outdir,"train_DS",ds_index,"/pred_tb.csv"))

#Comare perforamnce
perf1 <- compute_binaryclass_perf_func(pred_df[,"pred_asNEG_and_model"],pred_df$y_PRE_OR_POST_2ndEvent)
perf2 <- compute_binaryclass_perf_func(pred_df[,"pred_onlyuseOptimalModel"],pred_df$y_PRE_OR_POST_2ndEvent)

all_perf <- rbind(perf1,perf2)
rownames(all_perf) <- c("pred_asNEG_and_model","pred_onlyuseOptimalModel")
write.csv(all_perf,paste0(outdir,"train_DS",ds_index,"/perf_tb.csv"))
