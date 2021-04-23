#'This is code to generalize XGboost Classficaition 
#' and train/validation with sampling method
#'Code developed by Lucas (Jing) Liu @ Univeristy of Kentucky
#'Date: 09/06/2020
#'Updated 09/10/20  : Add functions for external validation
#' @MainFunctionInput: 
# 1. Data Input: Rows are IDs(rownames), 
#                Columns are features, 
#                Label must be in [0, num_class)
# 2. features_to_select: list of features and label for the model
# 3. label_col_name: name of label column
# 4. top_feature_flag: 1 for only using top features, 0 for using all features for validation
# 5. important_weight_threshold: threshold weight for selecting top features (Linear coeff for regression, info gain for classification)
# 6.  upsample_flag:  1 for upsampling, 0 for downsampling, others for nosampling for modeling
# 7.  num_sampling:   N of times for sampling
# 8.  xgb_params:  list of paramters: booster,Objective,etc (etc: any paramters in XGBoost document)
#    A. Choose classifer: booster = "gbtree", booster = "gblinear"
#       a .if gbtree, gain is the metric for importance  (nonlinear relations)
#       b. if gblinear, weight is the metric for importance (Linear relations)
#   B.Choose Objective function: 
#     a.  Regression Model:          "objective" = "reg:squarederror" #output prediction value of label col 
#     b.  Binary classification:      "objective" = "reg:logistic" #output probability 
#     c.  Multi-class classification: "objective" = "multi:softmax" #output classificaiton label
# 9.  num_rounds:  N of rounds for boosting
#' Additional @main_external_FunctionInput: 
#' validation_data: external validation data
#' important_features: found from main_func
#' 
library(datasets)
library(xgboost)
library(ggplot2)
library(pROC)
library(openxlsx)
library(dplyr)
library(caTools)
source("XGBUtilities.R")

####MAIN Functions: 
#This function returns 1. importance matrix
#                      2. LOOCV performance  
#                      3. Critical features
main_func<-function(data_input,features_to_select,label_col_name, top_feature_flag,important_weight_threshold,upsample_flag,num_sampling,xgb_params,num_rounds){
  #reorder "Label column to the end"
  analysisdata <- data_input[,features_to_select]
  analysisdata <- analysisdata %>% select(-label_col_name,label_col_name) 
  
  #Rank in imporatnce matrix
  importantce_Matrix<- Find_ImportanceRank_func(analysisdata,label_col_name,xgb_params,num_rounds,upsample_flag)
  important_features <- Find_critical_features_func(xgb_params,importantce_Matrix,important_weight_threshold)

  #Validate important features
  if(top_feature_flag==1){ #use only important features
    LOOCV_validation_data<-analysisdata[,c(important_features,label_col_name)]
  }else{
    LOOCV_validation_data<-analysisdata
    
  }
  
  
  predict_acutal_table_list<-Validation_function(LOOCV_validation_data,label_col_name,xgb_params,num_rounds,upsample_flag,num_sampling)
  performace_table_list<-list()
  for(r in 1:length(predict_acutal_table_list)){
    current_predict_df<-predict_acutal_table_list[[r]]
    actual<-current_predict_df$actual
    predicted<-current_predict_df$pred #original prediciton
    
    
    #Check if multi-class or binary class classification
    curr_task <- xgb_params$objective
    if (curr_task == "multi:softmax"){ #perforamnce with no AUC
      performace_table_list[[r]] <- compute_multiclass_perf_func(predicted,actual)
    }else{
      performace_table_list[[r]] <- compute_binaryclass_perf_func(predicted,actual)
    }
  }
  
  performance_table_all<-do.call(rbind,performace_table_list)
  performance_table_AVG_5DS<-colMeans(performance_table_all,na.rm = T) #average of sampling times
  performance_table_AVG_5DS
  
  return(list(importantce_Matrix,performance_table_AVG_5DS,important_features))
}


#This function is for prediction of external validation
#returns average performance and predicted values
main_external_func<-function(train_data,validation_data,features_to_select,label_col_name, top_feature_flag,important_features,upsample_flag,num_sampling,xgb_params,num_rounds){
  #reorder "Label column to the end"
  train_data <- train_data[,features_to_select]
  train_data <- train_data %>% select(-label_col_name,label_col_name) 
  
  validation_data <- validation_data[,features_to_select]
  validation_data <- validation_data %>% select(-label_col_name,label_col_name) 

  #Validate important features
  if(top_feature_flag==1){ #use only important features
    validation_data <- validation_data[,c(important_features,label_col_name)]
    train_data <- train_data[,c(important_features,label_col_name)]
  }else{
    validation_data <-  validation_data
    train_data <- train_data
  }
  
  
  #External Validation 
  predict_acutal_table_list_ext <- External_Validation_function(validation_data,train_data,label_col_name,upsample_flag,num_sampling)
  performace_table_list_ext <- compute_performance_function(predict_acutal_table_list_ext)
  perf_tb_all_ext<-do.call(rbind,performace_table_list_ext)
  perf_tb_AVG_5DS_ext<-colMeans(perf_tb_all_ext,na.rm = T) #average of sampling times
  perf_tb_AVG_5DS_ext
  

  
  return(list(perf_tb_AVG_5DS_ext,predict_acutal_table_list_ext))
}



##########START HERE##########
######################################################################################################## 
###############              Data preprocessing
###############     Note: Make sure label range: [0,num_class-1]
######################################################################################################## 
data(iris)
data_input <- iris
data_input[,"Species"] <- as.numeric(factor(data_input[,"Species"]))-1  #Labeling (label range: [0,num_class-1])

#Make sure every column is numeric
for (j in 1:ncol(data_input)){
  data_input[,j]<-as.numeric(data_input[,j])
}


#Split train and test
set.seed(123)   
sample <- sample.split(data_input,SplitRatio = 0.8) # 0.8 for training
train_data <- subset(data_input,sample ==TRUE) 
train_data<- train_data[-c(1:20,110:117),] #manually remove some labels to create unbalanced data for demonstration of sampling diffference
external_validation_data <- subset(data_input, sample==FALSE)
table(train_data$Species)

######################################################################################################## 
############               Final run                                          
######################################################################################################## 
##User input Pamameters
label_col_name <- "Species"
features_to_select <- c("Sepal.Length","Sepal.Width","Petal.Length","Petal.Width", label_col_name)
n_class <- 3
important_weight_threshold <- 0.2
top_feature_flag <- 1 
upsample_flag <- 1
num_rounds <- 10
num_sampling <- 5

#For multi-class classification #for multi-class, num_class needs to be specified:
xgb_params <- list(booster = "gbtree","objective" = "multi:softmax",num_class = n_class) 
#For binary classification:
#xgb_params <- list(booster = "gblinear","objective" = "reg:logistic")


##LOOCV
LOOCV_res<-main_func(train_data,features_to_select,label_col_name, top_feature_flag,important_weight_threshold,upsample_flag,num_sampling,xgb_params,num_rounds)
importantce_Matrix<-LOOCV_res[[1]]
LOOCV_AVG_performance<-round(LOOCV_res[[2]],2)
critical_features <- LOOCV_res[[3]]
#Print LOOCV performance
print(LOOCV_AVG_performance)

#Plot top feature importance
final_top_p <- Plot_FeatureImportance_func(importantce_Matrix,1,critical_features)
print(final_top_p)
#Plot all feature importance
final_p <- Plot_FeatureImportance_func(importantce_Matrix,0,critical_features)
print(final_p)

#External Validation
external_res<-main_external_func(train_data,external_validation_data,features_to_select,label_col_name, top_feature_flag,critical_features,upsample_flag,num_sampling,xgb_params,num_rounds)
external_AVG_performance<-round(external_res[[1]],2)
external_predicted_list <- external_res[[2]] #this variable contains list of tables of predicted values in each sampling 
#Print external performance
print(external_AVG_performance)
#Print external predicted values
example_prediction <- external_predicted_list[[1]]
print(example_prediction)

