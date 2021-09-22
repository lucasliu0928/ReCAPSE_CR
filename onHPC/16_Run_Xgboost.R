source("Recapse_Ultility.R")
Data_Sampling_Func <- function(upsample_flag,train_data,label_col_name,seed_num,random_perc = 0.8){
  # upsample_flag <- 0
  # train_data <- train_data
  # label_col_name <- "y_PRE_OR_POST_2ndEvent"
  # seed_num <- 1
  
  #Get label col index
  label_col_index <- which(colnames(train_data) == label_col_name)
  
  #Sampling
  if(upsample_flag==1){ #upsampling
    set.seed(seed_num)
    up_train <- upSample(x = train_data[, -label_col_index],
                         y = as.factor(train_data[,label_col_name]), yname = label_col_name)  
    sampled_train_data <- up_train
    
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_num)
    down_train <- downSample(x = train_data[, -label_col_index],
                             y = as.factor(train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
    
  }else if(upsample_flag==2){ #random sample 90% of orignal data
    set.seed(seed_num)
    sampled_indxes <- sample(nrow(train_data), nrow(train_data)*random_perc, replace = TRUE, prob = NULL)
    sampled_train_data <- train_data[sampled_indxes,]
  }else if (upsample_flag == 3){ #random sample then down sample
    set.seed(seed_num)
    sampled_indxes <- sample(nrow(train_data), nrow(train_data)*random_perc, replace = TRUE, prob = NULL)
    randomsampled_train_data <- train_data[sampled_indxes,]
    down_train <- downSample(x = randomsampled_train_data[, -label_col_index],
                             y = as.factor(randomsampled_train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
  }else{
    original_train <- train_data
    sampled_train_data <- original_train
  }
  
  return(sampled_train_data)
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
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "12_TrainTestIDs/")
data_dir2        <- paste0(proj_dir, "11D_ModelReady_CombFatures/WithPossibleMonthsHasNoCodes/")
outdir           <- paste0(proj_dir, "16_Performance/")

#User input
sampling_flag    <- "Down"

######################################################################################################## 
#1. Load and combine all patient model ready data
######################################################################################################## 
pt_files <-list.files(data_dir2,full.names = T)
#model_data <- do.call(rbind, lapply(pt_files,read.xlsx))
model_data <- do.call(rbind,mclapply(pt_files, mc.cores= numCores, function(z){read.xlsx(z, sheet = 1)}))

print("Original pre vs post samples: ")
table(model_data$y_PRE_OR_POST_2ndEvent)

#Add a column for original study ID 
original_IDs <- strsplit(model_data$sample_id,split = "@")
model_data$study_id <- sapply(original_IDs, "[[", 1)

################################################################################ 
#2. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
train_ID_df <- read.xlsx(paste0(data_dir1,"train_ID_withLabel.xlsx"),sheet = 1)
test_ID_df  <- read.xlsx(paste0(data_dir1,"test_ID_withLabel.xlsx"),sheet = 1)
print("Train non-SBCE vs SBCE : ")
table(train_ID_df$SBCE) #13542  1049 
print("Test non-SBCE vs SBCE : ")
table(test_ID_df$SBCE)  #3362  285 

train_ID <- paste0("ID", train_ID_df$study_id)
test_ID  <- paste0("ID", test_ID_df$study_id)

######################################################################################################## 
#3.Get model data for train and test
######################################################################################################## 
#I. Get data
#1. Train
if (sampling_flag ==  "None"){
  #1. Train data without down sampling
  train_data <- model_data[which(model_data[,"study_id"] %in% train_ID),]
  print("Training Pre vs Post:")
  table(train_data$y_PRE_OR_POST_2ndEvent) 
}else if (sampling_flag ==  "Down"){
  #2. Train data with down sampling
  seed_num <- 123
  train_data <- model_data[which(model_data[,"study_id"] %in% train_ID),]
  train_data <- Data_Sampling_Func(0,train_data,"y_PRE_OR_POST_2ndEvent",seed_num)
  train_data$y_PRE_OR_POST_2ndEvent <- as.numeric(train_data$y_PRE_OR_POST_2ndEvent) -1
  print("Training DownSampled Pre vs Post:")
  table(train_data$y_PRE_OR_POST_2ndEvent) 
}
#2. Test
test_data <- model_data[which(model_data[,"study_id"] %in% test_ID),]
print("Test: Pre vs Post:")
table(test_data$y_PRE_OR_POST_2ndEvent) 

#II. Create xgb input
train_label      <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
train_data_part  <- train_data[,!(names(train_data) %in% c("study_id","sample_id","y_PRE_OR_POST_2ndEvent"))]
dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

test_label       <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part   <- test_data[,!(names(test_data) %in% c("study_id","sample_id","y_PRE_OR_POST_2ndEvent"))]
dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)


# ######################################################################################################## 
#           Teresa's code
# ######################################################################################################## 
optimal_results <- BayesianOptimization(xgb_cv_bayes,
                                        bounds=list(eta=c(0.001, 0.3),
                                                    max_depth=c(3L, 10L),
                                                    min_child_weight=c(0L, 20L),
                                                    subsample=c(0.3, 0.9), colsample_by_tree=c(0.2, 0.8)),
                                        init_points=10,
                                        n_iter=10)
pos_weight <- 0.5
current_best <- list(etc = as.numeric(optimal_results$Best_Par['eta']),
                     max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                     min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                     subsample = as.numeric(optimal_results$Best_Par['subsample']),
                     colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']),
                     scale_pos_weight = pos_weight) #for weight more on pos samples
mod_optimal <- xgb.train(objective="binary:logistic",
                         params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                         watchlist= list(train = dtrain, eval = dtest), verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")
#Prediction table
pred   <- predict(mod_optimal, dtest)
actual <- test_label
prediction_tb <- cbind.data.frame(sample_id = test_data[,"sample_id"],pred,actual)
write.csv(prediction_tb,paste0(outdir,"16_Prediction_Table","_posweight",pos_weight,".csv"),row.names = F)

#Performance table 
perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)
write.csv(perf,paste0(outdir,"16_Performance_Table","_posweight",pos_weight,".csv"),row.names = F)

#Importantant matrix
importance_matrix <- xgb.importance(model = mod_optimal)
write.csv(importance_matrix,paste0(outdir,"16_importance_matrix","_posweight",pos_weight,".csv"),row.names = F)


# 
# # #check 
# bst <- xgboost(data = dtrain,nrounds = 10, params = list(scale_pos_weight = 9),objective = "binary:logistic")
# pred <- predict(bst, dtest)
# perf <- compute_binaryclass_perf_func(pred,actual)
# print(perf)


