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

#data dir
data_dir1        <- paste0(proj_dir, "15_XGB_Input/")
data_dir2        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/All_DS_Performance/")
outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1208updated/Use_ImportantFs_Performance/")

#Run XGBoost 10 times for 10 Downsampled Training data and the none DS training data
for (i in 1:10){#0:10
  ################################################################################
  #Load train and test
  ################################################################################
  load(file = paste0(data_dir1, "train_data_DS", i, ".rda"))
  load(file = paste0(data_dir1, "test_data.rda"))
  
  ################################################################################
  #Load important features for each DS
  ################################################################################
  important_f_df <- read.csv(paste0(data_dir2,"train_DS",i,"/BeforeSmoothed/16_importance_matrix_DS", i ,".csv"), stringsAsFactors = F)
  important_f_df <- important_f_df[order(important_f_df[,"Gain"],decreasing = T),]
  top_fs <- important_f_df[1:50,"Feature"]
  
  #important_f_df[,"Normed_Gain"] <- (important_f_df[,"Gain"]- min(important_f_df[,"Gain"]))/(max(important_f_df[,"Gain"])- min(important_f_df[,"Gain"]))
  #top_fs <- important_f_df[which(important_f_df[,"Normed_Gain"] >= 0.02),"Feature"]
  
  ################################################################################
  #Create xgb input
  ################################################################################
  train_label      <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
  train_data_part  <- train_data[,top_fs] #top 50 features
  dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  
  test_label       <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
  test_data_part   <- test_data[,top_fs] #top 50 features
  dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)
  
  
  ######################################################################################################## 
  # Run XGboost          Teresa's code
  ######################################################################################################## 
  optimal_results <- BayesianOptimization(xgb_cv_bayes,
                                          bounds=list(eta=c(0.001, 0.3),
                                                      max_depth=c(3L, 10L),
                                                      min_child_weight=c(0L, 20L),
                                                      subsample=c(0.3, 0.9), colsample_by_tree=c(0.2, 0.8)),
                                          init_points=10,
                                          n_iter=10)
  #Get best paramters
  #pos_weight <- 2
  current_best <- list(etc = as.numeric(optimal_results$Best_Par['eta']),
                       max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                       min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                       subsample = as.numeric(optimal_results$Best_Par['subsample']),
                       colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']))
                       #scale_pos_weight = pos_weight) #for weight more on pos samples
  #Optimal model
  mod_optimal <- xgb.train(objective="binary:logistic",
                           params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                           watchlist= list(train = dtrain, eval = dtest), verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")
  #Create subdirectory
  dir.create(file.path(paste0(outdir, "train_DS",i)))
  dir.create(file.path(paste0(outdir, "train_DS",i,"/BeforeSmoothed")))
  
  #output optimal paramter
  current_best_df <- as.data.frame(current_best)
  write.csv(current_best_df,paste0(outdir,"train_DS",i,"/BeforeSmoothed/16_OptimalModelParam.csv"), row.names = F)
  
  #Save model
  saveRDS(mod_optimal, paste0(outdir,"train_DS",i, "/BeforeSmoothed/model",i,".rds"))
  #my_model <- readRDS("model1.rds")
  
  #Prediction table
  pred   <- predict(mod_optimal, dtest)
  actual <- test_label
  prediction_tb <- cbind.data.frame(sample_id = test_data[,"sample_id"],pred,actual)
  write.csv(prediction_tb,paste0(outdir,"train_DS",i, "/BeforeSmoothed/16_Prediction_Table_DS",i,".csv"),row.names = F)
  
  #Performance table 
  perf <- compute_binaryclass_perf_func(pred,actual)
  print(perf)
  write.csv(perf,paste0(outdir,"train_DS",i,"/BeforeSmoothed/16_Performance_Table_DS", i,".csv"),row.names = F)
  
  #Importantant matrix
  importance_matrix <- xgb.importance(model = mod_optimal)
  write.csv(importance_matrix,paste0(outdir,"train_DS",i,"/BeforeSmoothed/16_importance_matrix_DS", i ,".csv"),row.names = F)
  
}

# 
# # #check 
#bst <- xgboost(data = dtrain,nrounds = 10, params = list(scale_pos_weight = 9),objective = "binary:logistic")
#pred <- predict(my_model, dtest)
# perf <- compute_binaryclass_perf_func(pred,actual)
# print(perf)


