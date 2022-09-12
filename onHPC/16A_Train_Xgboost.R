source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

################################################################################
#Data dira
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


feature_set_name  <- "CCSandDM3SPE"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_col          <- "SBCE" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"

if ((SBCE_col == "SBCE") | (SBCE_col == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
}


#data dir
data_dir1        <- paste0(proj_dir,"15_XGB_Input/",feature_set_name,"/",sample_name,"/", SBCE_col,"/")

newout <- paste0("16A_Trained_FullModel/",feature_set_name,"/",sample_name,"/",SBCE_col,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

#Run XGBoost 10 times for 10 Downsampled Training data and the none DS training data
for (i in 0:10){
  ################################################################################
  #Load train data
  ################################################################################
  load(file = paste0(data_dir1, "Train/train_nonobv_DS", i, ".rda"))
  train_data <- train_nonobv_ds_df
  print("# training non-obv Samples:")
  print(nrow(train_data))
  
  ################################################################################
  #Create xgb input
  ################################################################################
  train_label      <- as.numeric(train_data[,label_col])
  train_data_part  <- train_data[,!(names(train_data) %in% c("study_id","sample_id","y_PRE_OR_POST_2ndEvent","y_PRE_OR_POST_2ndEvent_ExcludedDeath"))]
  dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

  ######################################################################################################## 
  # Run XGboost          Teresa's code
  ######################################################################################################## 
  optimal_results <- BayesianOptimization(xgb_cv_bayes,
                                          bounds=list(eta=c(0.001, 0.3),
                                                      max_depth=c(3L, 10L),
                                                      min_child_weight=c(0L, 20L),
                                                      subsample=c(0.3, 0.9)), 
                                                      #colsample_bytree=c(0.2, 0.8)),
                                          init_points=10,
                                          n_iter=10)
  #Get best paramters from CV
  current_best <- list(eta = as.numeric(optimal_results$Best_Par['eta']),
                       max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                       min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                       subsample = as.numeric(optimal_results$Best_Par['subsample']))
                       #colsample_bytree = as.numeric(optimal_results$Best_Par['colsample_bytree']))
                       #scale_pos_weight = 0.5)
  #Optimal model
  mod_optimal <- xgb.train(objective="binary:logistic",
                           params=current_best,
                           data=dtrain, 
                           nrounds=10, 
                           early_stopping_rounds=100, 
                           maximize=TRUE,
                           watchlist= list(train = dtrain),
                           verbose=TRUE, print_every_n=10, eval_metric="error")
  
  #Output model
  xgb.save(mod_optimal, paste0(proj_dir, newout,"train_DS",i,".model"))
  
  #Importantant matrix
  importance_matrix <- xgb.importance(model = mod_optimal)
  write.csv(importance_matrix,paste0(proj_dir, newout,"importance_matrix_DS", i , ".csv"),row.names = F)

}



