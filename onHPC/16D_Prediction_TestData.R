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

#data dir
data_dir1        <- paste0(proj_dir, "15_XGB_Input/")
data_dir2       <- paste0(proj_dir, "16B_Trained_ImportantFeatureModel/")

newout <- "16C_Predictions/Test/"
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
  #Method 1: predict use AI model 
  #Method 2: predict as negative/pos/non_obv(use AI) 
  #Curve fitting for method1 and method2 results
  ################################################################################ 
  #Prediction
  pred_df_neg <- prediction_2method_func(test_neg_df,features,mod_optimal,"NEG")
  pred_df_pos <- prediction_2method_func(test_pos_df,features,mod_optimal,"POS")
  pred_df_nonobv <- prediction_2method_func(test_nonobv_df,features,mod_optimal,"nonOBV")
  pred_df_all <- rbind(pred_df_neg,pred_df_pos,pred_df_nonobv)
  #'@Check
  check <- compare_obvs_samples_2methods_perf(pred_df_all,"nonOBV")
  print(check)
  #Curve-fitting on the two methods' results
  pred_df_all <- curve_fitting_func(pred_df_all)
  
  write.csv(pred_df_all,paste0(outdir, ds_out, "pred_tb_all.csv"))
  
}





