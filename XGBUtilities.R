#I. Plot Functions 
Plot_FeatureImportance_func <- function(importantce_M,plot_top_flag,critical_features_list){
  #Rank important features by weight or Gain
  curr_classifer <- xgb_params$booster
  if (curr_classifer == "gbtree" ){
    importance_col <- "Gain"
  }else if (curr_classifer == "gblinear"){
    importance_col <- "Weight"
  }
  
  #group by sign of weight
  neg_indexes<-which(as.data.frame(importantce_M)[,importance_col]<0)
  pos_indexes<-which(as.data.frame(importantce_M)[,importance_col]>=0)
  importantce_M$Group<-NA
  importantce_M$Group[neg_indexes]<-"neg"
  importantce_M$Group[pos_indexes]<-"pos"
  
  #top
  if(plot_top_flag==1){
    critical_feature_idxes <- which(as.data.frame(importantce_M)[,"Feature"] %in% critical_features_list)
    importantce_M_top<-importantce_M[critical_feature_idxes,]
    
    gp = xgb.ggplot.importance(importantce_M_top,n_clusters = 2)
    final_p <- gp +geom_bar(stat="identity",position = position_dodge(width=10))+aes(fill =Group) +
      theme_bw()+
      theme(legend.position = "none",
            #panel.grid.major = element_blank(), #remove grid
            plot.title = element_text(size= 22),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title = element_text(size=20)) +
      ggtitle("")+
      scale_fill_manual(values=c("brown1","brown1", "#F8716F","#30B0BE"))
  }else{
    gp = xgb.ggplot.importance(importantce_M,n_clusters = 2)
    final_p <- gp +geom_bar(stat="identity", position = position_dodge(width=50))+aes(fill =Group)+
      theme_bw()+
      theme(legend.position = "none",
            #panel.grid.major = element_blank(),
            plot.title = element_text(size= 22),
            axis.text.x = element_text(size = 20),
            axis.text.y = element_text(size = 20),
            axis.title = element_text(size=20))+
      ggtitle("")+
      scale_fill_manual(values=c("brown1","brown1", "#F8716F","#30B0BE"))
    
  }
  return(final_p)
}



#II. Model related functions
#1.max_min normalization
range01 <- function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}

#2.Rank Feature Imporatnce by training all data
#return traingined model and importnce matrix
Find_ImportanceRank_func<-function(data_to_analysis,label_col_name,xgb_params,num_rounds,upsample_flag){
  #Reorder "Label column to the end"
  data_to_analysis <- data_to_analysis %>% select(-label_col_name,label_col_name) 
  
  #Min Max Normalization
  col_range <- 1:(ncol(data_to_analysis)-1) #exclude Label col
  if (length(col_range) > 1){
    data_to_analysis_norm<-as.data.frame(apply(data_to_analysis[,col_range], 2, range01))
  }else{ #if only one feature
    data_to_analysis_norm <- data_to_analysis
    data_to_analysis_norm[,col_range] <- range01(data_to_analysis_norm[,col_range])
  }
  #attach label column
  data_to_analysis_norm[,label_col_name] <- data_to_analysis[,label_col_name]
  
  #factorize the label column only if using "reg:squarederror"
  curr_objective <- xgb_params$objective
  if (curr_objective == "reg:squarederror"){
    data_to_analysis_norm[,label_col_name] <- as.factor(data_to_analysis_norm[,label_col_name])
  }
  

  train_data <- data_to_analysis_norm
  #Sampling for one time to get one model using entire data for training
  samp_res <- Model_sampling_func(upsample_flag,train_data,label_col_name,123) #random seed r
  train_matrix <- samp_res[[1]]
  trained_model <- xgb.train(params = xgb_params,data = train_matrix,nrounds = num_rounds)
  
  # get the feature real names
  if (length(col_range) > 1){
    names <-  samp_res[[2]]
  }else{ #if only one feature
    names <- colnames(data_to_analysis_norm)[col_range]
  }
  
  #Compute feature importance matrix
  importance_matrix = xgb.importance(feature_names = names, model = trained_model)

  return(importance_matrix[,1:2])
  
}


#3.Find critical features by important_weight_threshold
Find_critical_features_func <- function(xgb_params,importantce_M,important_weight_threshold){

  #Rank important features by weight or Gain
  curr_classifer <- xgb_params$booster
  if (curr_classifer == "gbtree" ){
    importance_col <- "Gain"
  }else if (curr_classifer == "gblinear"){
    importance_col <- "Weight"
  }
  
  importantce_M <- as.data.frame(importantce_M)   #convert data table to dataframe to select column eaiser
  critical_features_indexes<-which(abs(importantce_M[,importance_col])>important_weight_threshold)
  critical_features<-importantce_M[critical_features_indexes,"Feature"]
  return(critical_features)
}

#4.over or down sampling for training part
Model_sampling_func <- function(upsample_flag,train_data,label_col_name,seed_num){
  #upsampling
  library(caret)
  if(upsample_flag==1){
    set.seed(seed_num)
    up_train <- upSample(x = train_data[, -ncol(train_data)],
                         y = as.factor(train_data[,label_col_name]), yname = label_col_name)                         
    table(up_train[,label_col_name]) 
    
    train_label <- up_train[,label_col_name]
    train_label<-as.numeric(train_label)-1
    train_data_part<-up_train[,!(names(up_train) %in% label_col_name)]
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_num)
    down_train <- downSample(x = train_data[, -ncol(train_data)],
                             y = as.factor(train_data[,label_col_name]), yname = label_col_name)                         
    table(down_train[,label_col_name]) 
    
    train_label <- down_train[,label_col_name]
    train_label<-as.numeric(train_label)-1
    train_data_part<-down_train[,!(names(down_train) %in% label_col_name)]
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  }else{
    train_label <- train_data[,label_col_name]
    train_data_part<-train_data[,!(names(train_data) %in% label_col_name)]
    train_matrix <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)
  }
  
  #also return real feature name since train_matrix do not include the names
  real_feature_names <- colnames(train_data_part)
  
  return(list(train_matrix,real_feature_names))
}

#5. LOOCV with sampling for training
Validation_function<-function(validation_data,label_col_name,xgb_params,num_rounds,upsample_flag,num_sampling){
  #Reorder "Label column to the end"
  validation_data <- validation_data %>% select(-label_col_name,label_col_name) 
  
  
  #Min Max Normalization
  col_range <- 1:(ncol(validation_data)-1) #exclude Label col
  if (length(col_range) > 1){
    Validation_norm<-as.data.frame(apply(validation_data[,col_range], 2, range01))
  }else{ #if only one feature
    Validation_norm <- validation_data
    Validation_norm[,col_range] <- range01(Validation_norm[,col_range])
  }
  
  #attach label column
  Validation_norm[,label_col_name] <- validation_data[,label_col_name]
  
  
  predict_acutal_table_list<-list()
  for(r in 1:num_sampling){
    #manually loocv
    pred<-NA
    actual<-NA
    ID<-NA
    for(i in 1:nrow(Validation_norm)){
      test_data<-Validation_norm[i,]
      test_data_Id<-rownames(test_data)
      test_label<-test_data[,label_col_name]
      test_data_part <- test_data[,!(names(test_data) %in% label_col_name)]
      dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
      
      #remove test data from train sets
      train_data_Id_index<-which(rownames(Validation_norm) != test_data_Id)
      train_data<-Validation_norm[train_data_Id_index,]
      
      #sampling to generate balanced training set
      samp_res <- Model_sampling_func(upsample_flag,train_data,label_col_name,r)
      train_matrix <- samp_res[[1]]
      
      bst_model <- xgb.train(params = xgb_params,
                             data = train_matrix,
                             nrounds = num_rounds)
      
      pred[i] <- predict(bst_model, dtest)
      actual[i]<-test_label
      ID[i]<-test_data_Id
    }
    
    
    
    predict_acutal_table_list[[r]]<-cbind.data.frame(ID,actual,pred)
    
  }
  return(predict_acutal_table_list)
}


#6. External validation with sampling for training
External_Validation_function<-function(validation_data,train_data,label_col_name,upsample_flag,num_sampling){
  #Reorder "Label column to the end"
  validation_data <- validation_data %>% select(-label_col_name,label_col_name) 
  train_data <- train_data %>% select(-label_col_name,label_col_name) 
  
  
  #Min Max Normalization
  col_range <- 1:(ncol(validation_data)-1) #exclude Label col
  if (length(col_range) > 1){
    Validation_norm<-as.data.frame(apply(validation_data[,col_range], 2, range01))
    train_norm<-as.data.frame(apply(train_data[,col_range], 2, range01))
  }else{ #if only one feature
    Validation_norm <- validation_data
    Validation_norm[,col_range] <- range01(Validation_norm[,col_range])
    train_norm <- train_data
    train_norm[,col_range] <- range01(train_norm[,col_range])
  }
  
  #attach label column
  Validation_norm[,label_col_name] <- validation_data[,label_col_name]
  train_norm[,label_col_name] <- train_data[,label_col_name]
  
  ##form XGBoost format
  test_data  <- Validation_norm
  test_data_Id <- rownames(test_data)
  test_label <- test_data[,label_col_name]
  test_data_part <- test_data[,!(names(test_data) %in% label_col_name)]
  dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label=test_label)
  
  
  #sampling to generate balanced training set for num_sampling times
  predict_acutal_table_list<-list()
  for(r in 1:num_sampling){
    samp_res <- Model_sampling_func(upsample_flag,train_norm,label_col_name,r)
    train_matrix <- samp_res[[1]]
    bst_model <- xgb.train(params = xgb_params,
                           data = train_matrix,
                           nrounds = num_rounds)
    
    pred <- predict(bst_model, dtest)
    actual<-test_label
    ID<-test_data_Id
    
    predict_acutal_table_list[[r]]<-cbind.data.frame(ID,actual,pred)
    
  }
  return(predict_acutal_table_list)
}




#III. Performance related functions
#5.match levels of label vector in actual and predicted
match_label_levels_func <- function(predicted_classes,actual_classes){
  #Make `actual` and `predicted`  be factors with the same levels. 
  original_levels <- unique(actual_classes)
  actual_classes <- factor(actual_classes,levels = original_levels)
  predicted_classes <- factor(predicted_classes,levels = original_levels)
  return(list(predicted_classes,actual_classes))
}


#6. compute perforamnce for multiclass classification, No AUC
compute_multiclass_perf_func <- function(predicted_classes,actual_classes){
  #Make `actual` and `predicted`  be factors with the same levels. 
  matched_res <- match_label_levels_func(predicted_classes,actual_classes)
  predicted_classes <- matched_res[[1]]
  actual_classes <- matched_res[[2]]
  cm <- confusionMatrix(predicted_classes, actual_classes, dnn = c("Prediction", "Actual"),mode = "prec_recall")
  
  #By class peroframnce
  n_of_class <- length(unique(actual_classes))
  class_names <- paste0("C",seq(0,n_of_class-1))
  by_class_perf <- cm$byClass[,c("Precision", "Recall", "F1")]
  rownames(by_class_perf) <- class_names
  
  metric_names <- list(NA)
  for(m in 1:length(class_names)){
     metric_names[[m]] <- paste0(class_names[m],"_",c("Prec", "Recall", "F1"))
  }
  metric_names_list <- unlist(metric_names)
  
  n_of_metrics <- 3*n_of_class + 1 # 3:Prec, Recall, F1, 1: ACC
  all_class_perfs <- as.data.frame(matrix(NA, nrow = 1,ncol = n_of_metrics))
  colnames(all_class_perfs) <-c("ACC",metric_names_list)
  for (c in 1:nrow(by_class_perf)){
    curr_class <- rownames(by_class_perf)[c]
    perf_idxes <- which(grepl(curr_class,colnames(all_class_perfs)) ==T)
    all_class_perfs[,perf_idxes] <- by_class_perf[c,] 
  }
  
  all_class_perfs[,"ACC"] <- cm$overall[1]
  return(all_class_perfs)
}


#7. compute AUC
#return AUC score and cutoff threhold 
compute_auc_func <- function(predicted_prob,actual_label){
  roc_obj <- roc(actual_label, predicted_prob,quiet = T,direction = "<") #direction = "<"
  auc_score <-auc(roc_obj)
  
  #compute performance at cutoff point of ROC curve
  cutoff_results<-coords(roc_obj, "best", ret=c("threshold", "specificity", "sensitivity", "accuracy","precision", "recall"), transpose = FALSE)
  
  ###best threshold for ROC curve might happend at multiple point, so choose the one with max acc
  max_index<-which(cutoff_results$accuracy == max(cutoff_results$accuracy))
  cut_off_thres <- cutoff_results$threshold[max_index[1]]  #choose the first one for max acc
  pred_threhold<-cut_off_thres
  return(list(auc_score,pred_threhold))
}

#8.convert predicted prob to predicted labels results
#return predicted labels
convert_prediction_function <- function(predicted_prob,pred_threhold){
  prediceted_labels <- NA
  for(i in 1: length(predicted_prob)){
    current_prediction<-predicted_prob[i]
    if(current_prediction>pred_threhold){
      prediceted_labels[i]<-1
    }else {
      prediceted_labels[i]<-0
    }
  }
  return(prediceted_labels)
}

#9.Compute perforamnce for binary class
#return perforamcne table of AUC, ACC, both class recall, precision, F1
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



#10. Compute performance for each sampling result
#return list of performances tables 
compute_performance_function <-function(predict_acutal_table_list){
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
  return(performace_table_list)
}


