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

SBCE_col    <- "SBCE_Excluded_DeathLabel" #choose SBCE or SBCE_Excluded_DeathLabel
feature_set_name <- "CCSandVAL2nd"
if (SBCE_col == "SBCE"){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
}

#data dir
data_dir1 <- paste0(proj_dir,"15_XGB_Input/",feature_set_name,"/",SBCE_col,"/")
data_dir2 <- paste0(proj_dir,"16B_Trained_ImportantFeatureModel/",feature_set_name,"/",SBCE_col,"/")

newout <- paste0("16C_Predictions/",feature_set_name,"/",SBCE_col,"/Train/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

#For each training set model
for (ds_index in 0:10){
  #Create out dir for each ds index
  ds_out <- paste0(proj_dir, newout,"train_DS",ds_index,"/")
  dir.create(file.path(ds_out))
  ################################################################################
  #1. Load data
  ################################################################################
  load(file = paste0(data_dir1, "Train/train_neg_data.rda")) #obs neg
  load(file = paste0(data_dir1, "Train/train_pos_data.rda")) #obs pos
  load(file = paste0(data_dir1, "Train/train_nonobv_DS",ds_index,".rda")) #all non_obvs
  
  #Add a obv label
  train_neg_df[,"OBV_CLASS"] <- "NEG"
  train_pos_df[,"OBV_CLASS"] <- "POS"
  train_nonobv_ds_df[,"OBV_CLASS"] <- "nonOBV"
  
  ################################################################################
  #1. Load optimal model and features
  ################################################################################
  #1A. Get optimal model 
  mod_optimal <- xgb.load(paste0(data_dir2,"train_DS",ds_index,"_topf",".model"))
  
  #B. Get optimal model features
  features_df    <- read.csv(paste0(data_dir2,"importance_matrix_DS", ds_index ,"_topf", ".csv"), stringsAsFactors = F)
  features <- features_df$Feature
  
  ################################################################################ 
  #3.Prediction
  #Method 1: predict use AI model for all
  #Method 2: hybrid methods: predict as negative/pos/, for non_obv, use AI
  ################################################################################ 
  #Prediction
  pred_df_neg <- prediction_2method_func(train_neg_df,features,mod_optimal,"NEG",label_col)
  pred_df_pos <- prediction_2method_func(train_pos_df,features,mod_optimal,"POS",label_col)
  pred_df_nonobv <- prediction_2method_func(train_nonobv_ds_df,features,mod_optimal,"nonOBV",label_col)
  pred_df_all <- rbind(pred_df_neg,pred_df_pos,pred_df_nonobv)
  write.csv(pred_df_all,paste0(ds_out, "pred_tb_all.csv"))
  
  ################################################################################ 
  #4.Compute Performance for obv neg and obv pos for two methods
  ################################################################################ 
  perf_df_neg <- compare_obvs_samples_2methods_perf(pred_df_neg,"OBVNEG",label_col)
  perf_df_pos <- compare_obvs_samples_2methods_perf(pred_df_pos,"OBVPOS",label_col)
  perf_df_all <- rbind(perf_df_neg,perf_df_pos)
  write.csv(perf_df_all,paste0(ds_out,"perf_comparison_obvs.csv"))
  
}
