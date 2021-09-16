source("Recapse_Ultility.R")
#This script:
#for each patient for selected features(By freq) combines:
#1.CodeCount feature
#2.CodeTrans feature
#3.BinaryChar feature

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


#onHPC
project_dir            <- "/recapse/intermediate_data/"
modelReady_dir         <- paste0(project_dir,"12_ModelReadyData/")
outdir                 <- paste0(project_dir,"16_Performance/")

# #local
# project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# modelReady_dir         <- paste0(project_dir,"12_ModelReadyData/")
# outdir                 <- paste0(project_dir,"16_Performance/")


######################################################################################################## 
#1. Load and combine all patient data
######################################################################################################## 
pt_files <-list.files(modelReady_dir,full.names = T)
model_data <- do.call(rbind, lapply(pt_files,read.xlsx))
Final_ID <- unique(model_data$study_id)
print("Original pre vs post : ")
table(model_data$y_PRE_OR_POST_2ndEvent)

################################################################################ 
#2. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(project_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% Final_ID),] #only keep char for final ID
print("Original non-SBCE vs SBCE : ")
table(pts_level_char_df$SBCE)

################################################################################ 
#3. Get SBCE and non-SBCE IDs
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids))

##########START HERE##########
######################################################################################################## 
#### Data reprocessing 0521
### make sure no overlapping in original Ids in train,validation and test
######################################################################################################## 
nonrecurrent_pts_data <- model_data[which(model_data$study_id %in% nosbce_pt_Ids),]
n_no <- nrow(nonrecurrent_pts_data) #619991
recurrent_pts_data <- model_data[which(model_data$study_id %in% sbce_pt_Ids),]
n_yes<- nrow(recurrent_pts_data) # 72670

#1.Testing : Randomly choose 100 SBCE original pts data, and 100*8 noSBCE original pt Data
set.seed(123)
n <- 100
test_ID_SBCE <- sample(sbce_pt_Ids,n)
test_ID_noSBCE <- sample(nosbce_pt_Ids,n*original_noSBCE_toSBCEratio)
test_IDs <- c(test_ID_SBCE,test_ID_noSBCE)
print(paste0("# Test Original IDs: ", length(test_IDs)))
#remove test ID from 
remaining_ID <- Final_ID[which(!Final_ID %in% test_IDs)]

#1. Training 80% of the remaining_ID
training_ID <- sample(remaining_ID,length(remaining_ID)*0.8)
print(paste0("# Train Original IDs: ", length(training_ID)))

#2. validation 20% of the remaining_ID
validation_ID <- remaining_ID[which(!remaining_ID %in% training_ID)]
print(paste0("# Validation Original IDs: ", length(validation_ID)))


####Get data
train_data <- model_data[which(model_data$study_id %in% training_ID),]
print("Training:")
table(train_data$y_PRE_OR_POST_2ndEvent) 
#Down sampling
upsample_flag <- 0
seed_num <- 123
train_data <- Data_Sampling_Func(upsample_flag,train_data,"y_PRE_OR_POST_2ndEvent",seed_num)
print("Down Sampled:")
table(train_data$y_PRE_OR_POST_2ndEvent) 

#Create xgb input
train_label <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])-1
train_data_part<-train_data[,!(names(train_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

validation_data <- model_data[which(model_data$study_id %in% validation_ID),]
print("Validation:")
table(validation_data$y_PRE_OR_POST_2ndEvent) 
validation_label <- as.numeric(validation_data[,"y_PRE_OR_POST_2ndEvent"])
validation_data_part<-validation_data[,!(names(validation_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dvalidation <- xgb.DMatrix(data = as.matrix(validation_data_part), label = validation_label)

test_data <- model_data[which(model_data$study_id %in% test_IDs),]
print("Test:")
table(test_data$y_PRE_OR_POST_2ndEvent) 
test_label <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part<-test_data[,!(names(test_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)


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
pos_weight <- 10 #for weight more on pos samples
current_best <- list(etc = as.numeric(optimal_results$Best_Par['eta']),
                     max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                     min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                     subsample = as.numeric(optimal_results$Best_Par['subsample']),
                     colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']),
                     scale_pos_weight = pos_weight)
#We do not have to use this watch list and we do not have do create valdiation set, cuz xgb_cv_bayes set cross-validation set internally
watchlist <- list(train = dtrain, eval = dvalidation)

mod_optimal <- xgb.train(objective="binary:logistic",
                         params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                         watchlist=watchlist, verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")
#Prediction table
pred   <- predict(mod_optimal, dtest)
actual <- test_label
prediction_tb <- cbind.data.frame(study_id = test_data$study_id,Month_Start = test_data$Month_Start,pred,actual)
write.csv(prediction_tb,paste0(outdir,"16_prediction_tb_withDS_POSweight",pos_weight,".csv"),row.names = F)
#Performance table 
perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)
write.csv(perf,paste0(outdir,"16_perf_withDS_POSweight",pos_weight,".csv"),row.names = F)

#Importantant matrix
importance_matrix <- xgb.importance(model = mod_optimal)
write.csv(importance_matrix,paste0(outdir,"16_importance_matrix_withDS_POSweight",pos_weight,".csv"),row.names = F)



# #check 
# bst <- xgboost(data = dtrain,nrounds = 10, params = list(scale_pos_weight = 9),objective = "binary:logistic")
# pred <- predict(bst, dtest)
# perf <- compute_binaryclass_perf_func(pred,actual)
# print(perf)
# 

