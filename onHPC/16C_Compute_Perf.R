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

get_pt_level_pred_df <- function(analysis_df,pts_level_char_df){
  
  unique_test_ptIDs <- unique(analysis_df[,"study_id"])  
  thres <- c("01","02","03","04","05","06","07","08")
  
  pt_pred_df<- as.data.frame(matrix(NA, nrow = length(unique_test_ptIDs),ncol = 19))
  colnames(pt_pred_df) <- c("study_id","SBCE","Acutal_SBCEMonth", 
                            "PredictedSBCEMonth_Thres_01","PredictedClass_Thres_01",
                            "PredictedSBCEMonth_Thres_02","PredictedClass_Thres_02",
                            "PredictedSBCEMonth_Thres_03","PredictedClass_Thres_03",
                            "PredictedSBCEMonth_Thres_04","PredictedClass_Thres_04",
                            "PredictedSBCEMonth_Thres_05","PredictedClass_Thres_05",
                            "PredictedSBCEMonth_Thres_06","PredictedClass_Thres_06",
                            "PredictedSBCEMonth_Thres_07","PredictedClass_Thres_07",
                            "PredictedSBCEMonth_Thres_08","PredictedClass_Thres_08")
  for (i in 1:length(unique_test_ptIDs)){
    if (i %% 500 == 0) {print(i)}
    curr_pt_id <- unique_test_ptIDs[i]
    curr_pred_df <- analysis_df[analysis_df[,"study_id"] == curr_pt_id,]
    curr_char_df <- pts_level_char_df[pts_level_char_df[,"study_id"] == curr_pt_id, ]
    
    pt_pred_df[i , "SBCE"]             <- curr_char_df[,"SBCE"]
    pt_pred_df[i , "study_id"]         <- curr_pt_id
    
    curr_2nd_event_date <- mdy(curr_char_df[,"Date_2nd_Event"])
    if (is.na(curr_2nd_event_date) == F){
      curr_2nd_event_year  <- year(curr_2nd_event_date)
      curr_2nd_event_month <- month(curr_2nd_event_date)
      curr_acutal_SBCEMonth <- paste0(curr_2nd_event_year, "-",curr_2nd_event_month, "-", "01") #use the first day as the month
    }else {
      curr_acutal_SBCEMonth <- "NONE"
    }
    pt_pred_df[i , "Acutal_SBCEMonth"] <- curr_acutal_SBCEMonth
    
    #sort by month
    curr_pred_df <- curr_pred_df[order(curr_pred_df$month_start),]
    
    
    for (j in 1:length(thres)){
      curr_thres <- thres[j]
      #Get col index of threshold in sample pred df
      curr_col_idxes_inpreddf <- which(grepl(paste0("PredictedClass_Thres_",curr_thres),colnames(curr_pred_df)))
      curr_all_idxes <- which(curr_pred_df[,curr_col_idxes_inpreddf] == 1) #all index predicted higher than threhold
      if (length(curr_all_idxes) > 0 ){ #if there is any predicted month ?= threhold
        curr_pred_month <- curr_pred_df[curr_all_idxes[1],"month_start"] #1st index predicted higher or equal to the threhold
        curr_pred_class <- 1
      }else {
        curr_pred_month <- "NONE"
        curr_pred_class <- 0
      }
      #get col idex of threhold in patient pred df
      curr_col_idxes_inptdf1 <- which(grepl(paste0("PredictedSBCEMonth_Thres_",curr_thres),colnames(pt_pred_df)))
      curr_col_idxes_inptdf2 <- which(grepl(paste0("PredictedClass_Thres_",curr_thres),colnames(pt_pred_df)))
      
      pt_pred_df[i,curr_col_idxes_inptdf1] <- curr_pred_month
      pt_pred_df[i,curr_col_idxes_inptdf2] <- curr_pred_class
      
    }
  }
  
  return(pt_pred_df)
}

#There is no AUC in this function
compute_binaryclass_perf_func2_PTLEVEL <- function(prediction_df,thresholdClass_col){
  predicted_class <- prediction_df[,thresholdClass_col]
  actual_label    <- prediction_df[,"SBCE"]
  
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
  performance_table["TNR_Specificity"] <- TN/(TN + FP) #True negative rate = specificity
  performance_table["FPR"] <- FP/(TN + FP) #False postive rate
  performance_table["TPR_Sensitivity_Recall"] <- TP/(TP + FN) #True postive rate = sensitivity = recall
  performance_table["FNR"] <- FN/(TP + FN) #False negative rate 
  
  performance_table <- round(performance_table,2)
  
  return(performance_table)
}



get_perf_table_PTLEVEL_func<- function(analysis_df){
  #Get SBCE or nonSBCE numbers
  n_nonSBCE           <- length(which(analysis_df[,"SBCE"]==0))
  n_SBCE              <- length(which(analysis_df[,"SBCE"]==1))
  
  #Get performance for each threshold
  thres <- c("01","02","03","04","05","06","07","08")
  thres_class_cols <- paste0("PredictedClass_Thres_",thres)
  
  final_perf_df<- as.data.frame(matrix(NA, nrow = length(thres_class_cols),ncol = 11))
  colnames(final_perf_df) <- c("Threshold","N_NonRecurrent","N_Recurrent",
                               "Accuracy","Recall/Sensitivity/TPR",
                               "Specificity/TNR",
                               "Precision/PPV","F1",
                               "NPV","FPR","FNR")
  for (i in 1:length(thres_class_cols)){
    final_perf_df[i,"Threshold"]          <- thres_class_cols[i]
    final_perf_df[i,"N_NonRecurrent"]     <- n_nonSBCE
    final_perf_df[i,"N_Recurrent"]        <- n_SBCE
    
    curr_perf <- compute_binaryclass_perf_func2_PTLEVEL(analysis_df,thres_class_cols[i])
    
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


get_stats_month_diff <- function(analysis_df){
  #analysis_df <- ptlevel_monthdiff_df
  
  #get abs diff first
  analysis_df[,2:ncol(analysis_df)] <- abs(analysis_df[,2:ncol(analysis_df)])
  
  thres <- c("01","02","03","04","05","06","07","08","CP")
  diff_stat_df<- as.data.frame(matrix(NA, nrow = length(thres),ncol = 9))
  colnames(diff_stat_df) <- c("Threshold","Num_PTs_HasPredictedSBCEMonth","Mean_Diff_InMonth",
                              "Std_Diff_InMonth", "Median_Diff_InMonth",
                              "Q1_Diff_InMonth","Q3_Diff_InMonth",
                              "Min_Diff_InMonth", "Max_Diff_InMonth")
  for (i in 1:length(thres)){
    curr_th <- thres[i]
    curr_th_col <- paste0("DiffinMonth_Thres_",curr_th)
    
    curr_col_vals <- analysis_df[,curr_th_col] 
    
    #Get indxes of diff is inf
    curr_indxes_INF <- which(curr_col_vals == Inf)
    #exclude INF
    if(length(curr_indxes_INF) >0 ){
      curr_col_vals_excINF <- curr_col_vals[-curr_indxes_INF]
    }else{
      curr_col_vals_excINF <- curr_col_vals
    }
    
    diff_stat_df[i,"Threshold"]                       <- curr_th
    diff_stat_df[i,"Num_PTs_HasPredictedSBCEMonth"]   <- length(curr_col_vals_excINF)
    diff_stat_df[i,"Mean_Diff_InMonth"]               <- mean(curr_col_vals_excINF)
    diff_stat_df[i,"Std_Diff_InMonth"]                <- sd(curr_col_vals_excINF)
    diff_stat_df[i,"Median_Diff_InMonth"]             <- median(curr_col_vals_excINF)
    diff_stat_df[i,"Min_Diff_InMonth"]                <- min(curr_col_vals_excINF)
    diff_stat_df[i,"Max_Diff_InMonth"]                <- max(curr_col_vals_excINF)
    diff_stat_df[i,"Q1_Diff_InMonth"]                <- quantile(curr_col_vals_excINF,0.25)
    diff_stat_df[i,"Q3_Diff_InMonth"]                <- quantile(curr_col_vals_excINF,0.75)
    
    
    
  }
  return(diff_stat_df)
  
}

compute_month_diff <- function(analysis_df){
  #analysis_df <- ptlevel_pred_df_SBCE
  
  thres <- c("01","02","03","04","05","06","07","08","CP")
  
  pt_diff_df<- as.data.frame(matrix(NA, nrow = nrow(analysis_df),ncol = 9))
  colnames(pt_diff_df) <- c("study_id","DiffinMonth_Thres_01","DiffinMonth_Thres_02",
                            "DiffinMonth_Thres_03", "DiffinMonth_Thres_04",
                            "DiffinMonth_Thres_05", "DiffinMonth_Thres_06",
                            "DiffinMonth_Thres_07","DiffinMonth_Thres_08")
  for (i in 1:nrow(analysis_df)){
    curr_id <- analysis_df[i,"study_id"]
    curr_acutal_month <- ymd(analysis_df[i,"Acutal_SBCEMonth"])
    pt_diff_df[i,"study_id"] <- curr_id
    for (j in 1:length(thres)){
      curr_th <- thres[j]
      curr_th_col <- paste0("PredictedSBCEMonth_Thres_", curr_th)
      curr_pred_month <- analysis_df[i,curr_th_col]
      if (curr_pred_month == "NONE"){
        curr_diff <- Inf
      }else{
        curr_diff <- as.numeric(difftime(ymd(curr_pred_month),curr_acutal_month, units = "days"))/30
      }
      curr_th_diff_col <- paste0("DiffinMonth_Thres_", curr_th)
      pt_diff_df[i,curr_th_diff_col] <- curr_diff
      
    }
    
  }
  
  return(pt_diff_df)
}

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/All_DS_Performance/")
data_dir2        <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")

outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/All_DS_Performance/")

#User input
ds_index <- 0
######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed", "/16_Prediction_Table_DS", ds_index, ".csv"))

#Get predicted Class by different threshold
ths <- seq(0.1,0.8,0.1)
test_prediction_df <- add_predicted_class_byThreshold(test_prediction_df,ths)

#Add study_id and month start
original_IDs <- strsplit(as.character(test_prediction_df$sample_id),split = "@")
test_prediction_df$study_id    <- gsub("ID","",sapply(original_IDs, "[[", 1))
test_prediction_df$month_start <- sapply(original_IDs, "[[", 2)

################################################################################ 
#3. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)

################################################################################ 
#4. Load change point analysis results
################################################################################ 
changepoint_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed","/16_ChangePoint",".csv"),stringsAsFactors = F)


######################################################################################################## 
#4. Sample-level performance
######################################################################################################### 
#4.1 Compute performance 5 timmes sampling for each
# POS:NEG = 1:1 
# POS:NEG = 1:2 
# POS:NEG = 1:5 
#random sample 5 times
perf_tb_pos1_neg1 <-  get_avg_perf_5times_sampling_func(test_prediction_df,1,pts_level_char_df,ths)
perf_tb_pos1_neg2 <-  get_avg_perf_5times_sampling_func(test_prediction_df,2,pts_level_char_df,ths)
perf_tb_pos1_neg5 <-  get_avg_perf_5times_sampling_func(test_prediction_df,5,pts_level_char_df,ths)


write.csv(perf_tb_pos1_neg1,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/perf_tb_pos1_neg1",".csv"),row.names = T)
write.csv(perf_tb_pos1_neg2,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/perf_tb_pos1_neg2",".csv"),row.names = T)
write.csv(perf_tb_pos1_neg5,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/perf_tb_pos1_neg5",".csv"),row.names = T)

#4.2 Compute performance for all samples
perf_tb_alltest <- get_perf_table_func(test_prediction_df,pts_level_char_df)
write.csv(perf_tb_alltest,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/perf_tb_alltest",".csv"),row.names = T)


######################################################################################################## 
#5. Patient-level performance for all samples
#'@NOTE: These four pts ID: "14813" "24100" "31815" "36077" has death as 2nd event, but the prediction per-month data end a couple month before death date,
#'# so for these pts, there is no month labeled as SBCE
#'Might need to filter out these pts in predictionWinwdow.R file, need to make sure has data within 3 month before/after SBCE
########################################################################################################
#1.1.For each patient, get the first month that the prediction is great or equal to the threhold
#if there is a first month that prediction is great/equal to the threhold, the prediction for this patient = SBCE
# and the first month is when SBCE occurs
ptlevel_pred_df_alltest <- get_pt_level_pred_df(test_prediction_df,pts_level_char_df)

#1.2.Report all test pt level perforamnce
ptlevel_perf_tb_alltest <- get_perf_table_PTLEVEL_func(ptlevel_pred_df_alltest)
write.csv(ptlevel_perf_tb_alltest,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/Patient_Level_Perf_tb_alltest",".csv"),row.names = T)

#2.For SBCE patient, 
ptlevel_pred_df_SBCE <- ptlevel_pred_df_alltest[which(ptlevel_pred_df_alltest[,"SBCE"]==1), ]
#2.1 add predicted month using change points
ptlevel_pred_df_SBCE$PredictedSBCEMonth_Thres_CP <- NA
for (i in 1:nrow(ptlevel_pred_df_SBCE)){
  curr_id <- ptlevel_pred_df_SBCE[i,"study_id"]
  curr_cp <- changepoint_df[which(changepoint_df[,"study_id"] == curr_id),"changepoint_month"]
  ptlevel_pred_df_SBCE[i, "PredictedSBCEMonth_Thres_CP"] <- curr_cp
}

#2.2 compute the difference between predicted SBCE month and actual SBCE month
ptlevel_monthdiff_df <- compute_month_diff(ptlevel_pred_df_SBCE)

#2.3 month diff
monthdiff_stats  <- get_stats_month_diff(ptlevel_monthdiff_df)
write.csv(monthdiff_stats,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/Predicted_Monthdiff_stats",".csv"),row.names = T)
