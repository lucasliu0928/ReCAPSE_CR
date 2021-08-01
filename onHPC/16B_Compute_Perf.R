source("Recapse_Ultility.R")
#This script compute perfoermance for test data for different NEG:POS ratio
#1)POS: NEG = 1:1 
#2)POS: NEG = 1:2
#3)POS: NEG = 1:5

add_predicted_class_byThreshold <- function(prediction_df,thres_list){
  
  predicted_class_df <- as.data.frame(matrix(NA, nrow = nrow(prediction_df),ncol = length(thres_list)))
  colnames(predicted_class_df) <- gsub("\\.","", paste0("PredictedClass_Thres_",thres_list))
  for (j in 1:length(thres_list)){
    curr_thres <- thres_list[j]
    curr_col_name <- gsub("\\.","",paste0("PredictedClass_Thres_",curr_thres))
    predicted_class_df[,curr_col_name] <- convert_prediction_function(prediction_df[,"pred"],curr_thres)
  }
  
  #Add predicted classes to prediction df
  comb_df <- cbind(prediction_df,predicted_class_df)
  
  return(comb_df)
}


get_SampledPrediction_byPOSvsNEG_Ratio <- function(predition_df, acutal_label_col,NEGtoPOS_Ratio){
  #Get pos and neg indexes
  pos_idxes <- which(predition_df[,acutal_label_col] == 1)
  neg_idxes <- which(predition_df[,acutal_label_col] == 0)
  
  n_pos <- length(pos_idxes)
  n_neg_samples <- n_pos*NEGtoPOS_Ratio
  #Random sample N = n_pos *NEGtoPOS_Ratio  negative samples
  sampled_neg_indexes <- sample(neg_idxes,n_neg_samples)
  sampled_pred_df <- predition_df[c(pos_idxes,sampled_neg_indexes),]
  
  return(sampled_pred_df)
}

compute_binaryclass_perf_func2 <- function(prediction_df,thresholdClass_col){
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
  cm_tb <- cm$table
  TN <- cm_tb[2,2]
  FP <- cm_tb[1,2]
  TP <- cm_tb[1,1]
  FN <- cm_tb[2,1]
  
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

get_avg_perf_5times_sampling_func <-function(predition_df,NEGtoPOS_ratio,pts_level_char_df,threshold_list){
  # predition_df <- test_prediction_df
  # NEGtoPOS_ratio <- 1
  # threshold_list <- ths
  # 
  perf_list <- list(NA)
  for (i in 1:5){#random saple 5 times and compute perforamcne
    set.seed(i)
    pred_df <- get_SampledPrediction_byPOSvsNEG_Ratio(predition_df,"actual",NEGtoPOS_ratio)
    perf_list[[i]] <- get_perf_table_func(pred_df,pts_level_char_df)
    
  }
  perf_tb <- do.call(rbind,perf_list)
  
  threholds_names <- gsub("\\.","",paste0("PredictedClass_Thres_",threshold_list))
  
  avg_perf_list <- list(NA)
  for (i in 1: length(threholds_names)){ #averg 5 times performance for each threshold
    curr_th <- threholds_names[i]
    curr_th_indexes <- which(perf_tb[,"Threshold"] == curr_th)
    curr_th_pref <- perf_tb[curr_th_indexes,]
    avg_perf_list[[i]] <- colMeans(curr_th_pref[,2:ncol(curr_th_pref)])
  }
  avg_perf <- round(do.call(rbind,avg_perf_list),2)
  rownames(avg_perf) <- threholds_names
  return(avg_perf)
}



#onHPC
project_dir            <- "/recapse/intermediate_data/"
perfdir                 <- paste0(project_dir,"16_Performance/")

#local
project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
perfdir                 <- paste0(project_dir,"16_Performance/")

######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(perfdir,"16_prediction_tb_withDS.csv"))

#Get predicted Class by different threshold
ths <- seq(0.1,0.8,0.1)
test_prediction_df <- add_predicted_class_byThreshold(test_prediction_df,ths)


################################################################################ 
#3. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(project_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)


######################################################################################################## 
#Compute performance 5 timmes sampling for each
# POS:NEG = 1:1 
# POS:NEG = 1:2 
# POS:NEG = 1:5 
######################################################################################################## 
#random sample 5 times
perf_tb_pos1_neg1 <-  get_avg_perf_5times_sampling_func(test_prediction_df,1,pts_level_char_df,ths)
perf_tb_pos1_neg2 <-  get_avg_perf_5times_sampling_func(test_prediction_df,2,pts_level_char_df,ths)
perf_tb_pos1_neg5 <-  get_avg_perf_5times_sampling_func(test_prediction_df,5,pts_level_char_df,ths)


write.csv(perf_tb_pos1_neg1,paste0(perfdir,"perf_tb_pos1_neg1",".csv"),row.names = T)
write.csv(perf_tb_pos1_neg2,paste0(perfdir,"perf_tb_pos1_neg2",".csv"),row.names = T)
write.csv(perf_tb_pos1_neg5,paste0(perfdir,"perf_tb_pos1_neg5",".csv"),row.names = T)
