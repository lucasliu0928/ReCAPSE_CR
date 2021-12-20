source("Recapse_Ultility.R")
library("SHAPforxgboost")
library("ggplot2")
library("xgboost")
library("data.table")
library("here")


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
data_dir1        <- paste0(proj_dir, "15_XGB_Input/")
data_dir2        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1217updated/Use_ImportantFs_Performance/")
data_dir3        <- paste0(proj_dir,"12D_ExclusionSamples/WithPossibleMonthsHasNoCodes/")
outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1217updated/SHAP/")

#Downsampled index
ds_index <- 1

################################################################################
#Load train and test
################################################################################
load(file = paste0(data_dir1, "train_data_DS", ds_index, ".rda"))
load(file = paste0(data_dir1, "test_data.rda"))

################################################################################ 
#2.Load obvious neg/pos and non-obvious test sample IDs
################################################################################ 
obv_neg_test_ID_df <- read.csv(paste0(data_dir3,"Test/ObviousNeg_Samples_Test.csv"),stringsAsFactors = F)
obv_pos_test_ID_df <- read.csv(paste0(data_dir3,"Test/ObviousPOS_Samples_Test.csv"),stringsAsFactors = F)
NON_obv_test_ID_df <- read.csv(paste0(data_dir3,"Test/NON_Obvious_Samples_Test.csv"),stringsAsFactors = F)

test_sampleID_obvNeg    <- obv_neg_test_ID_df[,"sample_id"]
test_sampleID_obvPos   <- obv_pos_test_ID_df[,"sample_id"]
test_sampleID_nonobv    <- NON_obv_test_ID_df[,"sample_id"]

test_data_neg <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_obvNeg),]
test_data_pos <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_obvPos),]
test_data_nonobv <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_nonobv),]

################################################################################
#Load important features for each DS
################################################################################
important_f_df <- read.csv(paste0(data_dir2,"train_DS",ds_index,"/BeforeSmoothed/16_importance_matrix_DS", ds_index ,".csv"), stringsAsFactors = F)
important_f_df <- important_f_df[order(important_f_df[,"Gain"],decreasing = T),]
top_fs <- important_f_df[1:50,"Feature"]

################################################################################
#Create xgb input
################################################################################
train_label      <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
train_data_part  <- train_data[,top_fs] #top 50 features
dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

test_label       <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part   <- test_data[,top_fs] #top 50 features
dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)

test_data_neg <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_obvNeg),]
test_data_pos <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_obvPos),]
test_data_nonobv <- test_data[which(test_data[,"sample_id"] %in% test_sampleID_nonobv),]


#Load Optimal model
mod_optimal <- readRDS(paste0(data_dir2,"train_DS",ds_index, "/BeforeSmoothed/model", ds_index, ".rds"))

#Create directory
dir.create(file.path(paste0(outdir, "train_DS",ds_index)))


################################################################################
#SHAP for Train
################################################################################
shap_input_df <- as.matrix(train_data_part[,mod_optimal$feature_names]) ##make sure the feature is the same order as the ones in the model

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
outfile_name <- paste0(outdir, "train_DS",ds_index,"/","SHAP_train.png")
png(outfile_name)
print(shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10))
dev.off()

################################################################################
#SHAP for Test
################################################################################
shap_input_df <- as.matrix(test_data_part[,mod_optimal$feature_names]) ##make sure the feature is the same order as the ones in the model

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
outfile_name <- paste0(outdir, "train_DS",ds_index,"/","SHAP_test.png")
png(outfile_name)
print(shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10))
dev.off()


################################################################################
#SHAP for Test obv neg,obv pos, and no-obv, seperately
################################################################################
test_data_list <- list(test_data_neg, test_data_pos, test_data_nonobv)
test_data_type <- c("obvNEG","obvPOS","nonobv")
for (i in 1:3){
  curr_data        <- test_data_list[[i]]
  curr_test_data_type   <- test_data_type[i]
  
  test_label       <- as.numeric(curr_data[,"y_PRE_OR_POST_2ndEvent"])
  test_data_part   <- curr_data[,top_fs] #top 50 features
  dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)
  
  shap_input_df <- as.matrix(test_data_part[,mod_optimal$feature_names]) ##make sure the feature is the same order as the ones in the model
  
  # To return the SHAP values and ranked features by mean|SHAP|
  shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
  # The ranked features by mean |SHAP|
  shap_values$mean_shap_score
  #Plot
  outfile_name <- paste0(outdir, "train_DS",ds_index,"/","SHAP_test_",curr_test_data_type ,".png")
  png(outfile_name)
  print(shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10))
  dev.off()
}




