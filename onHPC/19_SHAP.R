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
data_dir1 <- paste0(proj_dir, "15_XGB_Input/")
data_dir2 <- paste0(proj_dir, "16B_Trained_ImportantFeatureModel/")

outdir <- paste0(proj_dir, "17_Performance/")

################################################################################ 
#User input
################################################################################ 
ds_index <- 3

#Create directory
ds_out <- paste0("DS",ds_index,"/SHAP/")
dir.create(file.path(outdir, ds_out), recursive = TRUE)

################################################################################
#Load train and test
################################################################################
load(file = paste0(data_dir1, "Train/train_nonobv_DS", ds_index, ".rda"))
load(file = paste0(data_dir1, "Test/test_pos_data.rda"))
load(file = paste0(data_dir1, "Test/test_neg_data.rda"))
load(file = paste0(data_dir1, "Test/test_nonobv_data.rda"))

################################################################################
#Load important features for each DS and optimal model
################################################################################
important_f_df <- read.csv(paste0(data_dir2,"importance_matrix_DS",ds_index,"_topf.csv"), stringsAsFactors = F)
top_fs <- important_f_df[,"Feature"]
#Load Optimal model
mod_optimal <- xgb.load(paste0(data_dir2,"train_DS",ds_index,"_topf",".model"))


################################################################################
#Create xgb input
################################################################################
train_data <- train_nonobv_ds_df
test_data <- rbind(test_pos_df,test_neg_df,test_nonobv_df)
  
train_label      <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
train_data_part  <- train_data[,top_fs] #top 50 features
dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

test_label       <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part   <- test_data[,top_fs] #top 50 features
dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)

test_data_neg <- test_data[which(test_data[,"sample_id"] %in% test_neg_df$sample_id),]
test_data_pos <- test_data[which(test_data[,"sample_id"] %in% test_pos_df$sample_id),]
test_data_nonobv <- test_data[which(test_data[,"sample_id"] %in% test_nonobv_df$sample_id),]


################################################################################
#SHAP for Train
################################################################################
shap_input_df <- as.matrix(train_data_part)

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)

outfile_name <- paste0(outdir, ds_out,"SHAP_train.png")
png(outfile_name,width = 1000,height = 500,res=100)
print(p)
dev.off()

################################################################################
#SHAP for Test
################################################################################
shap_input_df <- as.matrix(test_data_part) ##make sure the feature is the same order as the ones in the model

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score

#Plot
p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)
outfile_name <- paste0(outdir, ds_out,"SHAP_test.png")
png(outfile_name,width = 1000,height = 500,res=100)
print(p)
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
  
  shap_input_df <- as.matrix(test_data_part) ##make sure the feature is the same order as the ones in the model
  
  # To return the SHAP values and ranked features by mean|SHAP|
  shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
  # The ranked features by mean |SHAP|
  shap_values$mean_shap_score
  #Plot
  p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)
  outfile_name <- paste0(outdir, ds_out,"SHAP_test_",curr_test_data_type,".png")
  png(outfile_name,width = 1000,height = 500,res=100)
  print(p)
  dev.off()
}




