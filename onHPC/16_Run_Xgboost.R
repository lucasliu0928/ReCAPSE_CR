source("Recapse_Ultility.R")
library(rBayesianOptimization) 
library(xgboost) 
library(Matrix)
library(fastDummies)
compute_binaryclass_perf_func <- function(predicted_prob,actual_label){
  #compute ROC-AUC
  auc_res <- compute_auc_func(predicted_prob,actual_label)
  auc_score <- auc_res[[1]]
  pred_threhold <- auc_res[[2]] #threhold at cutoff point for ROC curve
  
  #convert to predicted labels
  predicted_labels <- convert_prediction_function(predicted_prob,pred_threhold)
  
  #Match label factor levels
  matched_res <- match_label_levels_func(predicted_labels,actual_label)
  final_pred <- matched_res[[1]]
  final_actual <- matched_res[[2]]
  
  #Class 0 
  cm<-confusionMatrix(final_pred, final_actual, positive = "0", dnn = c("Prediction", "Actual"),mode = "prec_recall")
  class0_prec<-cm$byClass[5]
  class0_recall<-cm$byClass[6]
  class0_f1<-cm$byClass[7]
  
  #class 1
  cm1<-confusionMatrix(final_pred, final_actual, positive = "1", dnn = c("Prediction", "Actual"),mode = "prec_recall")
  class1_prec<-cm1$byClass[5]
  class1_recall<-cm1$byClass[6]
  class1_f1<-cm1$byClass[7]
  
  acc<-cm$overall[1]
  auc<-as.numeric(auc_score)
  
  performance_table <- round(cbind.data.frame(auc,acc,class0_prec,class0_recall,class0_f1,class1_prec,class1_recall,class1_f1),2)
  
  return(performance_table)
}

xgb_cv_bayes <- function(eta, max_depth, min_child_weight, subsample, colsample_by_tree){
  print(paste("eta:", eta))
  print(paste("max_depth:", max_depth))
  print(paste("min_child_weight:", min_child_weight)) 
  print(paste("subsample:", subsample))
  print(paste("colsample_by_tree:", colsample_by_tree))
  cv <- xgb.cv(params=list(booster="gbtree", eta=eta, max_depth=max_depth,
                           min_child_weight=min_child_weight,
                           subsample=subsample,
                           olsample_by_tree=colsample_by_tree,
                           lambda=1, alpha=0,
                           #nthread=ncores, n_jobs=ncores,
                           objective="binary:logistic", eval_metric="auc"),
               data=dtrain, nround=5,nfold = 10,
               prediction=TRUE, showsd=TRUE, early_stopping_rounds=100,
               stratified=FALSE, maximize=TRUE)
  print(paste("cv:", cv))
  list(Score=cv$evaluation_log[, max(test_auc_mean)], Pred=cv$pred)
}


#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# #local
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

######################################################################################################## 
#load data
######################################################################################################## 
model_data <- read.csv(paste0(outdir,"15_ModelReadyData.csv"),stringsAsFactors = F)

################################################################################ 
#2. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
final_ID <- unique(model_data$study_id)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% final_ID),]
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids))


##########START HERE##########
######################################################################################################## 
#### Data reprocessing 0521
### make sure no overlapping in original Ids in train,validation and test
######################################################################################################## 
nonrecurrent_pts_data <- model_data[which(model_data$study_id %in% nosbce_pt_Ids),]
nrow(nonrecurrent_pts_data) #619991
recurrent_pts_data <- model_data[which(model_data$study_id %in% sbce_pt_Ids),]
nrow(recurrent_pts_data) # 72670

####SAmple IDs
#1.Testing : Randomly choose 200 SBCE original pts data, and 200*8 noSBCE original pt Data
set.seed(123)
test_ID_SBCE <- sample(sbce_pt_Ids,200)
test_ID_noSBCE <- sample(nosbce_pt_Ids,200*8)
test_IDs <- c(test_ID_SBCE,test_ID_noSBCE)

#remove test ID from 
remaining_ID <- final_ID[which(!final_ID %in% test_IDs)]

#1. Training 80% of the remaining_ID
training_ID <- sample(remaining_ID,length(remaining_ID)*0.8)

#2. validation 20% of the remaining_ID
validation_ID <- remaining_ID[which(!remaining_ID %in% training_ID)]


####Get data
train_data <- model_data[which(model_data$study_id %in% training_ID),]
table(train_data$y_PRE_OR_POST_2ndEvent) #375925  18359 

validation_data <- model_data[which(model_data$study_id %in% validation_ID),]
table(validation_data$y_PRE_OR_POST_2ndEvent) #92792  4319

test_data <- model_data[which(model_data$study_id %in% test_IDs),]
table(test_data$y_PRE_OR_POST_2ndEvent) #192381  8885 




# ######################################################################################################## 
#           Teresa's code
# ######################################################################################################## 
train_label <- train_data[,"y_PRE_OR_POST_2ndEvent"]
train_label <- as.numeric(train_label)
train_data_part<-train_data[,!(names(train_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

validation_label <- validation_data[,"y_PRE_OR_POST_2ndEvent"]
validation_label <- as.numeric(validation_label)
validation_data_part<-validation_data[,!(names(validation_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dvalidation <- xgb.DMatrix(data = as.matrix(validation_data_part), label = validation_label)


test_label <- test_data[,"y_PRE_OR_POST_2ndEvent"]
test_label <- as.numeric(test_label)
test_data_part<-test_data[,!(names(test_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)




optimal_results <- BayesianOptimization(xgb_cv_bayes, 
                                        bounds=list(eta=c(0.001, 0.3),
                                                    max_depth=c(3L, 10L),
                                                    min_child_weight=c(0L, 20L),
                                                    subsample=c(0.3, 0.9), colsample_by_tree=c(0.2, 0.8)),
                                        init_points=10,
                                        n_iter=10)


current_best <- list(etc = as.numeric(optimal_results$Best_Par['eta']), 
                     max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                     min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                     subsample = as.numeric(optimal_results$Best_Par['subsample']),
                     colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']),
                     scale_pos_weight = 9)
watchlist <- list(train = dtrain, eval = dvalidation)

mod_optimal <- xgb.train(objective="binary:logistic", 
                         params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                         watchlist=watchlist, verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")
pred <- predict(mod_optimal, dtest)
actual <-test_label

perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)

