source("Recapse_Ultility.R")
#this script make predictions for test data using 2 methods:
#1. Method1: AI methods: AI models to predict all test
#2. Method2: hybrid methods: AI model to predict non-obv, obv_neg/pos are predicted as neg/pos 
#3. For the results of method1 and method2, then do curve fitting to fine-tune the results

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

newout <- paste0("16C_Predictions/",feature_set_name,"/",SBCE_col,"/Test/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

#For each training set model
for (ds_index in 0:10){
  #Create out dir for each ds index
  ds_out <- paste0("DS",ds_index,"/Sample_Prediction_Table/")
  dir.create(file.path(outdir, ds_out), recursive = TRUE)

  ################################################################################
  #1. Load data
  ################################################################################
  load(file = paste0(data_dir1, "Test/test_neg_data.rda")) #obs neg
  load(file = paste0(data_dir1, "Test/test_pos_data.rda")) #obs pos
  load(file = paste0(data_dir1, "Test/test_nonobv_data.rda")) #all non_obvs
  
  #Add a obv label
  test_neg_df[,"OBV_CLASS"] <- "NEG"
  test_pos_df[,"OBV_CLASS"] <- "POS"
  test_nonobv_df[,"OBV_CLASS"] <- "nonOBV"
  
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
  #Method 1: predict use AI model for all samples
  #Method 2: hybrid methods : predict as negative/pos/, for non_obv(use AI) 
  #Curve fitting for method1 and method2 results
  ################################################################################ 
  #Prediction
  pred_df_neg <- prediction_2method_func(test_neg_df,features,mod_optimal,"NEG",label_col)
  pred_df_pos <- prediction_2method_func(test_pos_df,features,mod_optimal,"POS",label_col)
  pred_df_nonobv <- prediction_2method_func(test_nonobv_df,features,mod_optimal,"nonOBV",label_col)
  pred_df_all <- rbind(pred_df_neg,pred_df_pos,pred_df_nonobv)

  #Curve-fitting on the two methods' results
  pred_df_all <- curve_fitting_func(pred_df_all)
  #Add momth
  res <- strsplit(as.character(pred_df_all[,"sample_id"]),split = "@")
  pred_df_all[,"month_start"] <- sapply(res, "[[", 2)
  pred_df_all[,"month_start"] <- ymd(pred_df_all[,"month_start"])
  
  #Separate prediction for each model and compute predicted class for threshold 0.1.0.2...0.8
  common_cols <- c("study_id", "sample_id", label_col,"OBV_CLASS","month_start")
  ths <- seq(0.1,0.9,0.1)
  
  #Hybrid model 
  pred_col <- "pred_Method_Hybrid"
  pred_df_m1 <- pred_df_all[,c(common_cols,pred_col)]
  pred_df_m1 <- add_predicted_class_byThreshold(pred_df_m1,ths,pred_col)
  write.csv(pred_df_m1,paste0(outdir, ds_out, "pred_tb_Hybrid.csv"))
  
  #AI model 
  pred_col <- "pred_Method_AI"
  pred_df_m2 <- pred_df_all[,c(common_cols,pred_col)]
  pred_df_m2 <- add_predicted_class_byThreshold(pred_df_m2,ths,pred_col)
  write.csv(pred_df_m2,paste0(outdir, ds_out, "pred_tb_AI.csv"))
  
  #Hybrid + Curve fitting model 
  pred_col <- "pred_Method_HybridCurveFit"
  pred_df_m3 <- pred_df_all[,c(common_cols,pred_col)]
  pred_df_m3 <- add_predicted_class_byThreshold(pred_df_m3,ths,pred_col)
  write.csv(pred_df_m3,paste0(outdir, ds_out, "pred_tb_HybridCurveFit.csv"))
  
  #AI + Curve fitting model 
  pred_col <- "pred_Method_AICurveFit"
  pred_df_m4 <- pred_df_all[,c(common_cols,pred_col)]
  pred_df_m4 <- add_predicted_class_byThreshold(pred_df_m4,ths,pred_col)
  write.csv(pred_df_m4,paste0(outdir, ds_out, "pred_tb_AICurveFit.csv"))
  
}





