source("Recapse_Ultility.R")
get_sample_preds_byNtoPratio <- function(predition_df, samplelabel_col,NEGtoPOS_Ratio){
  #Get pos and neg indexes
  pos_idxes <- which(predition_df[,samplelabel_col] == 1)
  neg_idxes <- which(predition_df[,samplelabel_col] == 0)
  
  n_pos <- length(pos_idxes)
  n_neg_samples <- n_pos*NEGtoPOS_Ratio
  #Random sample N = n_pos *NEGtoPOS_Ratio  negative samples
  sampled_neg_indexes <- sample(neg_idxes,n_neg_samples)
  sampled_pred_df <- predition_df[c(pos_idxes,sampled_neg_indexes),]
  
  return(sampled_pred_df)
}


compute_binaryclass_perf_func2 <- function(prediction_df,predprob_col,samplelabel_col ,thresholdClass_col){
  #prediction_df <- ds_pred_df
  #thresholdClass_col <- "Pred_Class_Thres_01"
  # predprob_col <- "pred_Method_AI"
  # samplelabel_col <- "y_PRE_OR_POST_2ndEvent"
  
  
  predicted_prob  <- prediction_df[,predprob_col]
  predicted_class <- prediction_df[,thresholdClass_col]
  actual_label    <- prediction_df[,samplelabel_col]
  
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

get_perf_table_func<- function(analysis_df,predprob_col,samplelabel_col,pts_level_char_df){
  #analysis_df <- pred_df
  
  #Get pre or post numbers
  n_pre               <- length(which(analysis_df[,samplelabel_col]==0))
  n_post              <- length(which(analysis_df[,samplelabel_col]==1))
  
  #Get SBCE or not patient IDs
  analysis_char_df    <- pts_level_char_df[which(pts_level_char_df[,"study_id"] %in% analysis_df[,"study_id"]),]
  n_nonrecurrent_pt   <- length(which(analysis_char_df[,"SBCE"]==0))
  n_recurrent_pt      <- length(which(analysis_char_df[,"SBCE"]==1))
  
  #Get performance for each threshold
  thres_class_cols <- colnames(analysis_df)[which(grepl("Thres",colnames(analysis_df))==T)]
  final_perf_df<- as.data.frame(matrix(NA, nrow = length(thres_class_cols),ncol = 14))
  colnames(final_perf_df) <- c("Threshold",
                               "N_NonRecurrent","N_Recurrent",
                               "N_Pre","N_Post",
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
    
    curr_perf <- compute_binaryclass_perf_func2(analysis_df,predprob_col,samplelabel_col,thres_class_cols[i])
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


#This function gets lists of random sample prediction df for K times
get_performance_of_random_samples_predictions <- function(K, predition_df,predprob_col, samplelabel_col,NtoP_ratio){
  #samplelabel_col <- "y_PRE_OR_POST_2ndEvent"
  
  perf_list <- list(NA)
  for (i in 1:K){#random sample K times and compute perforamcne
    set.seed(i)
    pred_df <- get_sample_preds_byNtoPratio(predition_df,samplelabel_col,NtoP_ratio)
    curr_pref <- get_perf_table_func(pred_df,predprob_col,samplelabel_col,pts_level_char_df)
    curr_pref$RANDOM_SAMPLE <- paste0("S",i)
    perf_list[[i]] <- curr_pref
    
  }
  perf_tb <- do.call(rbind,perf_list)
  
  return(perf_tb)
}

get_avgPerf_eachThres_overRandSample <-function(predition_df,predprob_col , samplelabel_col,NtoP_ratio,pts_level_char_df,threshold_list){
  # predition_df <- ds_pred_df
  # acutal_label_col <- "y_PRE_OR_POST_2ndEvent"
  # NtoP_ratio <- 1
  # threshold_list <- seq(1,9,1)
  
  #Get perforamnce of 5 random sample on each threhoslds
  perf_tb <- get_performance_of_random_samples_predictions(5, predition_df,predprob_col, samplelabel_col,NtoP_ratio)

  #Get average perforamnce for each threholds over 5 random samples
  threholds_names <- gsub("\\.","",paste0("Pred_Class_Thres_0",threshold_list))
  
  avg_perf_list <- list(NA)
  for (i in 1: length(threholds_names)){ #averg 5 times performance for each threshold
    curr_th <- threholds_names[i]
    curr_th_indexes <- which(perf_tb[,"Threshold"] == curr_th)
    curr_th_pref <- perf_tb[curr_th_indexes,]
    avg_perf_list[[i]] <- colMeans(curr_th_pref[,2:(ncol(curr_th_pref)-1)])
  }
  avg_perf <- round(do.call(rbind,avg_perf_list),2)
  rownames(avg_perf) <- threholds_names
  return(avg_perf)
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
data_dir1 <- paste0(proj_dir, "16C_Predictions/Test_0610/")
data_dir2        <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")

outdir <- paste0(proj_dir, "17_Performance_0610/")


################################################################################ 
#1. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df$study_id <- paste0("ID",pts_level_char_df$study_id)

################################################################################ 
#2. Compute performance
################################################################################ 
model_list <- c("Hybrid","AI","HybridCurveFit","AICurveFit")
#model_list <- c("AI")
ths <- seq(1,9,1)
samplelabel_col <- "y_PRE_OR_POST_2ndEvent"

for (ds_index in 0:10){
  #Create out dir for each ds 
  ds_out <- paste0("DS",ds_index,"/Sample_Level/")
  dir.create(file.path(outdir, ds_out), recursive = TRUE)
  
  for (model in model_list){
    model_pred_file <- paste0("pred_tb_",model,".csv")
    pred_prob_col <- paste0("pred_Method_",model)
    
    #1. Load all sample prediction table
    ds_in <- paste0(data_dir1,"DS",ds_index,"/Sample_Prediction_Table/")
    ds_pred_df <- read.csv(paste0(ds_in,model_pred_file),stringsAsFactors = F)
  
    #2. Get all neg/pos/nonobv sample prediction table
    ds_pred_df_neg <- ds_pred_df[ds_pred_df[,"OBV_CLASS"] == "NEG",]
    ds_pred_df_pos <- ds_pred_df[ds_pred_df[,"OBV_CLASS"] == "POS",]
    ds_pred_df_nonobv <- ds_pred_df[ds_pred_df[,"OBV_CLASS"] == "nonOBV",]
    
    ######################################################################################################### 
    #2. Get performance for each sets
    ######################################################################################################### 
    #2.1.Compute performance for all test samples
    perf_tb_alltest <- get_perf_table_func(ds_pred_df,pred_prob_col,samplelabel_col,pts_level_char_df)
    write.csv(perf_tb_alltest,paste0(outdir, ds_out, model,"_perf_tb_alltest",".csv"),row.names = T)
    
    #2.2 compute performance for all obv neg, obv pos, and obs nonobv seperately
    perf_tb_alltest_neg <- get_perf_table_func(ds_pred_df_neg,pred_prob_col,samplelabel_col,pts_level_char_df)
    write.csv(perf_tb_alltest_neg,paste0(outdir, ds_out, model,"_perf_tb_allneg",".csv"),row.names = T)
    
    perf_tb_alltest_pos <- get_perf_table_func(ds_pred_df_pos,pred_prob_col,samplelabel_col,pts_level_char_df)
    write.csv(perf_tb_alltest_pos,paste0(outdir, ds_out, model,"_perf_tb_allpos",".csv"),row.names = T)
    
    perf_tb_alltest_nonobv <- get_perf_table_func(ds_pred_df_nonobv,pred_prob_col,samplelabel_col,pts_level_char_df)
    write.csv(perf_tb_alltest_nonobv,paste0(outdir, ds_out, model,"_perf_tb_allnonobv",".csv"),row.names = T)
    
    
    #2.3 Compute average performance 5 timmes sampling
    perf_tb_pos1_neg1 <-  get_avgPerf_eachThres_overRandSample(ds_pred_df,pred_prob_col,samplelabel_col, 1,pts_level_char_df,ths)
    perf_tb_pos1_neg2 <-  get_avgPerf_eachThres_overRandSample(ds_pred_df,pred_prob_col,samplelabel_col, 2,pts_level_char_df,ths)
    perf_tb_pos1_neg5 <-  get_avgPerf_eachThres_overRandSample(ds_pred_df,pred_prob_col,samplelabel_col, 5,pts_level_char_df,ths)
    
    write.csv(perf_tb_pos1_neg1,paste0(outdir, ds_out, model,"_perf_tb_1vs1",".csv"),row.names = T)
    write.csv(perf_tb_pos1_neg2,paste0(outdir, ds_out, model,"_perf_tb_1vs2",".csv"),row.names = T)
    write.csv(perf_tb_pos1_neg5,paste0(outdir, ds_out, model,"_perf_tb_1vs5",".csv"),row.names = T)
  }
  
}