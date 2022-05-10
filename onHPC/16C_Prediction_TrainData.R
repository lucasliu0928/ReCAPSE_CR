source("Recapse_Ultility.R")

prediction_2method_func <- function(model,valid_data,obv_type){
  # model      <- mod_optimal
  # valid_data <- valid_data1
  # obv_type <- "NEG"
  # 
  #xgb input
  valid_label       <- as.numeric(valid_data[,"y_PRE_OR_POST_2ndEvent"])
  valid_data_part   <- valid_data[,features] 
  dvalid            <- xgb.DMatrix(data = as.matrix(valid_data_part), label = valid_label)
  
  pred_df <- as.data.frame(matrix(NA, nrow = nrow(valid_data),ncol = 5))
  colnames(pred_df) <- c("study_id","sample_id","y_PRE_OR_POST_2ndEvent",paste0("pred_as_",obv_type),"pred_useOptimalModel")
  pred_df$study_id                  <- valid_data$study_id
  pred_df$sample_id                 <- valid_data$sample_id
  pred_df$y_PRE_OR_POST_2ndEvent    <- valid_data$y_PRE_OR_POST_2ndEvent
  
  #method 1:
  pred_df[,"pred_useOptimalModel"] <- predict(mod_optimal, dvalid)
  
  #method 2:
  if (obv_type == "NEG"){
    pred_df[,paste0("pred_as_",obv_type)] <- 0
  }else if (obv_type == "POS"){
    pred_df[,paste0("pred_as_",obv_type)] <- 1
  }
  
  return(pred_df)
}


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
data_dir2        <- paste0(proj_dir, "16B_Trained_ImportantFeatureModel/")

newout <- "16B_Trained_ImportantFeatureModel/"
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

ds_index <- 1

################################################################################
#1. Load optimal model parameters
################################################################################
#1A. Get optimal model features
mod_optimal <- readRDS(paste0(data_dir1,"train_DS",ds_index, "/BeforeSmoothed/model", ds_index, ".rds"))
features    <- mod_optimal$feature_names

################################################################################
#2. Load all model data
################################################################################
load(file = paste0(data_dir2, "All_PTS_ModelReadyData.rda"))

################################################################################ 
#3.Get obvious neg data as part of validation
################################################################################ 
obv_neg_train_ID_df   <- read.csv(paste0(data_dir3,"ObviousNeg_Samples.csv"),stringsAsFactors = F)
validation_ID1    <- obv_neg_train_ID_df[,"sample_id"]
valid_data1 <- model_data[which(model_data$sample_id %in% validation_ID1),]

################################################################################ 
#3.Get obvious pos data as part of validation
################################################################################ 
obv_pos_train_ID_df   <- read.csv(paste0(data_dir3,"ObviousPos_Samples.csv"),stringsAsFactors = F)
validation_ID2    <- obv_pos_train_ID_df[,"sample_id"]
valid_data2 <- model_data[which(model_data$sample_id %in% validation_ID2),]

################################################################################ 
#clear memory
################################################################################
rm("model_data")
gc()



################################################################################ 
#5.Prediction for obv neg 
#Method 1: predict use optimal model 
#Method 2: predict as negative 
################################################################################ 
#Prediction
pred_df_neg <- prediction_2method_func(mod_optimal,valid_data1,"NEG")
write.csv(pred_df_neg,paste0(outdir,"train_DS",ds_index,"/pred_tb_obvNEG.csv"))
#Comare perforamnce
actual    <- as.factor(pred_df_neg[,"y_PRE_OR_POST_2ndEvent"])
pred1      <- pred_df_neg[,"pred_as_NEG"]
pred2      <- pred_df_neg[,"pred_useOptimalModel"]
perf1 <- compute_binaryclass_perf_func(pred1,actual)
perf2 <- compute_binaryclass_perf_func(pred2,actual)
all_perf <- rbind(perf1,perf2)
rownames(all_perf) <- c("pred_as_NEG","pred_useOptimalModel")
write.csv(all_perf,paste0(outdir,"train_DS",ds_index,"/perf_tb_obvNEG.csv"))


################################################################################ 
#5.Prediction for obv pos 
#Method 1: predict use optimal model 
#Method 2: predict as postives 
################################################################################ 
#Prediction
pred_df_pos <- prediction_2method_func(mod_optimal,valid_data2,"POS")
write.csv(pred_df_pos,paste0(outdir,"train_DS",ds_index,"/pred_tb_obvPOS.csv"))

#Comare perforamnce
actual    <- as.factor(pred_df_pos[,"y_PRE_OR_POST_2ndEvent"])
pred1      <- pred_df_pos[,"pred_as_POS"]
pred2      <- pred_df_pos[,"pred_useOptimalModel"]
perf1 <- compute_binaryclass_perf_func(pred1,actual)
perf2 <- compute_binaryclass_perf_func(pred2,actual)
all_perf <- rbind(perf1,perf2)
rownames(all_perf) <- c("pred_as_POS","pred_useOptimalModel")
write.csv(all_perf,paste0(outdir,"train_DS",ds_index,"/perf_tb_obvPOS.csv"))
