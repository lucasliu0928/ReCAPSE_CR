add_predicted_class_byThreshold <- function(prediction_df,thres_list){
  
  predicted_class_df <- as.data.frame(matrix(NA, nrow = nrow(prediction_df),ncol = length(thres_list)))
  colnames(predicted_class_df) <- gsub("\\.","", paste0("PredictedClass_Thres_",thres_list))
  for (j in 1:length(thres_list)){
    curr_thres <- thres_list[j]
    curr_col_name <- gsub("\\.","",paste0("PredictedClass_Thres_",curr_thres))
    predicted_class_df[,curr_col_name] <- convert_prediction_function(prediction_df[,"pred_Method2_AIMODEL"],curr_thres)
  }
  
  #Add predicted classes to prediction df
  comb_df <- cbind(prediction_df,predicted_class_df)
  
  return(comb_df)
}

compute_binaryclass_perf_func2 <- function(prediction_df,thresholdClass_col){
  #prediction_df <- analysis_df
  #thresholdClass_col <- thres_class_cols[i]
  
  predicted_prob  <- prediction_df[,"pred"]
  predicted_class <- prediction_df[,thresholdClass_col]
  actual_label    <- prediction_df[,"actual"]
  
  #compute ROC-AUC
  auc_res <- compute_auc_func(predicted_prob,actual_label)
  auc_score <- as.numeric(auc_res[[1]])
  
  #Match label factor levels
  matched_res   <- match_label_levels_func(predicted_class,actual_label)
  final_pred    <- matched_res[[1]]
  final_actual  <- matched_res[[2]]
  
  cm<-confusionMatrix(final_pred, final_actual, positive = "1", dnn = c("Prediction", "TrueLabels"),mode = "everything")
  #Manually get TN, FP, TP, FN
  
  TN <- length(which(final_pred==0 & final_actual==0))
  FP <- length(which(final_pred==1 & final_actual==0))
  TP <- length(which(final_pred==1 & final_actual==1))
  FN <- length(which(final_pred==0 & final_actual==1))
  
  #cm_tb <- cm$table
  # TN <- cm_tb[1,1]
  # FP <- cm_tb[2,1]
  # TP <- cm_tb[2,2]
  # FN <- cm_tb[1,2]
  
  #class 1
  performance_table <- cm$byClass[c("Sensitivity","Specificity",
                                    "Pos Pred Value","Neg Pred Value",
                                    "Precision", "Recall","F1")]
  performance_table["Accuracy"] <- cm$overall[1]
  performance_table["AUC"]      <- auc_score
  performance_table["TNR_Specificity"] <- TN/(TN + FP) #True negative rate = specificity
  performance_table["FPR"] <- FP/(TN + FP) #False postive rate
  performance_table["TPR_Sensitivity_Recall"] <- TP/(TP + FN) #True postive rate = sensitivity = recall
  performance_table["FNR"] <- FN/(TP + FN) #False negative rate 
  
  performance_table <- round(performance_table,2)
  
  return(performance_table)
}
get_perf_table_func<- function(analysis_df,pts_level_char_df){
  #Get pre or post numbers
  n_pre               <- length(which(analysis_df[,"actual"]==0))
  n_post              <- length(which(analysis_df[,"actual"]==1))
  
  #Get SBCE or not patient IDs
  analysis_char_df    <- pts_level_char_df[which(pts_level_char_df[,"study_id"] %in% analysis_df[,"study_id"]),]
  n_nonrecurrent_pt   <- length(which(analysis_char_df[,"SBCE"]==0))
  n_recurrent_pt      <- length(which(analysis_char_df[,"SBCE"]==1))
  
  #Get performance for each threshold
  thres_class_cols <- colnames(analysis_df)[which(grepl("Thres",colnames(analysis_df))==T)]
  final_perf_df<- as.data.frame(matrix(NA, nrow = length(thres_class_cols),ncol = 14))
  colnames(final_perf_df) <- c("Threshold","N_NonRecurrent","N_Recurrent","N_Pre","N_Post",
                               "AUC","Accuracy","Recall/Sensitivity/TPR",
                               "Specificity/TNR",
                               "Precision/PPV","F1",
                               "NPV","FPR","FNR")
  for (i in 1:length(thres_class_cols)){
    final_perf_df[i,"Threshold"]          <- thres_class_cols[i]
    final_perf_df[i,"N_NonRecurrent"]     <- n_nonrecurrent_pt
    final_perf_df[i,"N_Recurrent"]        <- n_recurrent_pt
    final_perf_df[i,"N_Pre"]              <- n_pre
    final_perf_df[i,"N_Post"]             <- n_post
    
    curr_perf <- compute_binaryclass_perf_func2(analysis_df,thres_class_cols[i])
    
    final_perf_df[i,"AUC"]                    <- curr_perf["AUC"]
    final_perf_df[i,"Accuracy"]               <- curr_perf["Accuracy"]
    
    final_perf_df[i,"Recall/Sensitivity/TPR"] <- curr_perf["Recall"]
    final_perf_df[i,"Specificity/TNR"]        <- curr_perf["Specificity"]
    final_perf_df[i,"Precision/PPV"]          <- curr_perf["Precision"]
    final_perf_df[i,"F1"]                     <- curr_perf["F1"]
    final_perf_df[i,"NPV"]                    <- curr_perf["Neg Pred Value"]
    final_perf_df[i,"FPR"]                    <- curr_perf["FPR"]
    final_perf_df[i,"FNR"]                    <- curr_perf["FNR"]
    
  }
  
  return(final_perf_df)
}

proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
pred_df <- read.csv(paste0(proj_dir, "16C_Predictions/Test/DS5/Prediction_Table/pred_tb_all.csv"),stringsAsFactors = F)

#Get predicted Class by different threshold
ths <- seq(0.1,0.8,0.1)
test_prediction_df <- add_predicted_class_byThreshold(pred_df,ths)
colnames(test_prediction_df)[which(colnames(test_prediction_df) == "pred_Method2_AIMODEL")] <- "pred"
colnames(test_prediction_df)[which(colnames(test_prediction_df) == "y_PRE_OR_POST_2ndEvent")] <- "actual"
data_dir2        <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)

perf_tb_alltest <- get_perf_table_func(test_prediction_df,pts_level_char_df)
perf_tb_alltest[5,]
