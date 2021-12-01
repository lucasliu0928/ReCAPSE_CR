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
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/")
data_dir2        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/Use_ImportantFs_Performance/")
outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/SHAP/")


#my_model <- readRDS("model1.rds")source("Recapse_Ultility.R")

#Downsampled index
ds_index <- 1

################################################################################
#Load train and test
################################################################################
load(file = paste0(data_dir1, "XGB_Input/train_data_DS", ds_index, ".rda"))
load(file = paste0(data_dir1, "XGB_Input/test_data.rda"))

################################################################################
#Load important features for each DS
################################################################################
important_f_df <- read.csv(paste0(data_dir1,"All_DS_Performance/train_DS",ds_index,"/BeforeSmoothed/16_importance_matrix_DS", ds_index ,"_posweight0.5.csv"), stringsAsFactors = F)
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


#Load Optimal model
mod_optimal <- readRDS(paste0(data_dir2,"train_DS",ds_index, "/BeforeSmoothed/model", ds_index, ".rds"))

#Create directory
dir.create(file.path(paste0(outdir, "train_DS",ds_index)))

################################################################################
#SHAP for Train
################################################################################
check <- as.matrix(train_data_part[1:500,])

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = check)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
outfile_name <- paste0(outdir, "train_DS",ds_index,"/","SHAP_train.png")
png(outfile_name)
print(shap.plot.summary.wrap1(model = mod_optimal, X = as.matrix(train_data_part[1:200,]),top_n = 10))
dev.off()

################################################################################
#SHAP for Test
################################################################################
# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = as.matrix(test_data_part))
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
outfile_name <- paste0(outdir, "train_DS",ds_index,"/","SHAP_test.png")
png(outfile_name)
print(shap.plot.summary.wrap1(model = mod_optimal, X = as.matrix(test_data_part)))
dev.off()
