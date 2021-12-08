library(openxlsx)
library(data.table)
library(lubridate)
library(parallel) 
library(foreach)
library(doParallel)
library(stringr)
library(rBayesianOptimization) 
library(xgboost) 
library(Matrix)
library(fastDummies)
library(pROC)
library(caret)
library(dplyr)
library(sas7bdat)
library(ggplot2)
library(reshape2)
library(xgboost)
library(Rtsne)
library("FactoMineR")
library("factoextra")

compute_binaryclass_perf_func <- function(predicted_prob,actual_label){
  #compute ROC-AUC
  auc_res <- compute_auc_func(predicted_prob,actual_label)
  auc_score <- auc_res[[1]]
  pred_threhold <- 0.5 
  #pred_threhold <- auc_res[[2]] #threhold at cutoff point for ROC curve
  
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

match_label_levels_func <- function(predicted_classes,actual_classes){
  #Make `actual` and `predicted`  be factors with the same levels. 
  original_levels <- unique(actual_classes)
  actual_classes <- factor(actual_classes,levels = original_levels)
  predicted_classes <- factor(predicted_classes,levels = original_levels)
  return(list(predicted_classes,actual_classes))
}

convert_prediction_function <- function(predicted_prob,pred_threhold){
  prediceted_labels <- NA
  for(i in 1: length(predicted_prob)){
    current_prediction<-predicted_prob[i]
    if(current_prediction>=pred_threhold){
      prediceted_labels[i]<-1
    }else {
      prediceted_labels[i]<-0
    }
  }
  return(prediceted_labels)
}




#These fucntions get per day cleaned codes for all avaiable dates for one pateint
get_perDay_medicaid <- function(patient_ID,medicaid_heath_dir,medicaid_pharm_dir){
  #Codes columns for medicaid
  medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
  medicaid_proc_cols <- c("CDE_PROC_PRIM")
  medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
  
  #I. Load date and clean codes in orignal data
  #check if ID in the dir
  IDs_has_health_files <- as.numeric(gsub("_all_medicaid_healthclaims.xlsx|ID","",list.files(medicaid_heath_dir)))
  if (patient_ID %in% IDs_has_health_files){
    #1.read original health_claims to get proc and diag codes
    health_df <- read.xlsx(paste0(medicaid_heath_dir,"ID",patient_ID,"_all_medicaid_healthclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in health_claims
    cleaned_healthClaims <- clean_code_columns(health_df,c(medicaid_diag_cols,medicaid_proc_cols))
  }else{
    cleaned_healthClaims <- NULL
  }
  #2.get original pharmcy_claims to get drug codes
  IDs_has_pharm_files <- as.numeric(gsub("_all_medicaid_pharmclaims.xlsx|ID","",list.files(medicaid_pharm_dir)))
  if (patient_ID %in% IDs_has_pharm_files){
    pharm_df <- read.xlsx(paste0(medicaid_pharm_dir,"ID",patient_ID,"_all_medicaid_pharmclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in pharmcy_claims
    cleaned_PharmClaims <- clean_code_columns(pharm_df,medicaid_drug_cols)
  }else{
    cleaned_PharmClaims <- NULL
  }
  
  #II.get all dates
  if (is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == F){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_PharmClaims[,"DTE_FIRST_SVC"]))
  }else if (is.null(cleaned_PharmClaims) == T & is.null(cleaned_healthClaims) == F ){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    aval_dates <- unique(cleaned_healthClaims[,"DTE_FIRST_SVC"])
  }else if(is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == T){
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(cleaned_PharmClaims[,"DTE_FIRST_SVC"])
  }
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    #curr day code list and remove all blanks and NAs
    if (is.null(cleaned_healthClaims) == F){ #if has health claims
      curr_day_healthclaims <- cleaned_healthClaims[which(cleaned_healthClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_diag <- remove_NA_func(unique(unlist(curr_day_healthclaims[,medicaid_diag_cols])))
      curr_day_proc <- remove_NA_func(unique(unlist(curr_day_healthclaims[,medicaid_proc_cols])))
    }else{
      curr_day_diag <- NULL
      curr_day_proc <- NULL
    }
    
    if (is.null(cleaned_PharmClaims) == F){ #if has pharms claims
      curr_day_drugclaims   <- cleaned_PharmClaims[which(cleaned_PharmClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_drug <- remove_NA_func(unique(unlist(curr_day_drugclaims[,medicaid_drug_cols])))
    }else{
      curr_day_drug <- NULL
    }
    
    
    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}

get_perDay_medicare <- function(patient_ID,medicare_dir){
  #Codes columns for medicare
  medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
  medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
  medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")
  
  
  #I. Load date and clean codes in orignal data
  #1.read original claim to get proc and diag codes
  claim_df <- read.xlsx(paste0(medicare_dir,"ID",patient_ID,"_all_medicare_claims.xlsx"),sheet = 1,detectDates = T)
  claim_df$claims_date <- mdy(claim_df$claims_date)
  #clean codes in health_claims
  cleaned_Claims_df <- clean_code_columns(claim_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))
  
  #II.get all dates
  aval_dates <- unique(cleaned_Claims_df[,"claims_date"])
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    curr_day_claims<- cleaned_Claims_df[which(cleaned_Claims_df[,"claims_date"] == curr_date),]
    
    #curr day code list
    curr_day_diag <- unique(unlist(curr_day_claims[,medicare_diag_cols]))
    curr_day_proc <- unique(unlist(curr_day_claims[,medicare_proc_cols]))
    curr_day_drug <- unique(unlist(curr_day_claims[,medicare_drug_cols]))
    
    #remove all blanks and NAs
    curr_day_diag <- remove_NA_func(curr_day_diag)
    curr_day_proc <- remove_NA_func(curr_day_proc)
    curr_day_drug <- remove_NA_func(curr_day_drug)
    
    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}

get_perDay_both <- function(patient_ID,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir){
  #Codes columns for medicaid
  medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
  medicaid_proc_cols <- c("CDE_PROC_PRIM")
  medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
  #Codes columns for medicare
  medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
  medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
  medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")
  
  
  #I. Load date and clean codes in orignal data
  #check if ID in the dir
  IDs_has_health_files <- as.numeric(gsub("_all_medicaid_healthclaims.xlsx|ID","",list.files(medicaid_heath_dir)))
  if (patient_ID %in% IDs_has_health_files){
    #1.read original health_claims to get proc and diag codes
    health_df <- read.xlsx(paste0(medicaid_heath_dir,"ID",patient_ID,"_all_medicaid_healthclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in health_claims
    cleaned_healthClaims <- clean_code_columns(health_df,c(medicaid_diag_cols,medicaid_proc_cols))
  }else{
    cleaned_healthClaims <- NULL
  }
  #2.get original pharmcy_claims to get drug codes
  IDs_has_pharm_files <- as.numeric(gsub("_all_medicaid_pharmclaims.xlsx|ID","",list.files(medicaid_pharm_dir)))
  if (patient_ID %in% IDs_has_pharm_files){
    pharm_df <- read.xlsx(paste0(medicaid_pharm_dir,"ID",patient_ID,"_all_medicaid_pharmclaims.xlsx"),sheet = 1,detectDates = T)
    #clean codes in pharmcy_claims
    cleaned_PharmClaims <- clean_code_columns(pharm_df,medicaid_drug_cols)
  }else{
    cleaned_PharmClaims <- NULL
  }
  
  #3.read original medicare claim to get proc and diag codes
  claim_df <- read.xlsx(paste0(medicare_dir,"ID",patient_ID,"_all_medicare_claims.xlsx"),sheet = 1,detectDates = T)
  claim_df$claims_date <- mdy(claim_df$claims_date)
  #clean codes in health_claims
  cleaned_Claims_df <- clean_code_columns(claim_df,c(medicare_diag_cols,medicare_proc_cols,medicare_drug_cols))
  
  
  #II.get all dates, it is possible in medicaid, patient only has either pharm or health claims
  if (is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == F){ 
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_PharmClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
  }else if (is.null(cleaned_PharmClaims) == T & is.null(cleaned_healthClaims) == F ){
    cleaned_healthClaims$DTE_FIRST_SVC <- mdy(cleaned_healthClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_healthClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
    
  }else if(is.null(cleaned_PharmClaims) == F & is.null(cleaned_healthClaims) == T){
    cleaned_PharmClaims$DTE_FIRST_SVC <- dmy(cleaned_PharmClaims$DTE_FIRST_SVC)
    aval_dates <- unique(c(cleaned_PharmClaims[,"DTE_FIRST_SVC"],cleaned_Claims_df[,"claims_date"]))
  }
  
  #III.per day table
  per_day_table <- as.data.frame(matrix(NA, nrow = length(aval_dates),ncol = 5))
  colnames(per_day_table) <- c("study_id","claims_date","Diag_Codes","Proc_Codes","Drug_Codes")
  for (j in 1:length(aval_dates)){
    per_day_table[j,"study_id"] <- patient_ID
    
    curr_date <- aval_dates[j]
    per_day_table[j,"claims_date"] <- as.character(curr_date)
    
    #curr day code list and remove all blanks and NAs
    #medicare
    curr_day_claims<- cleaned_Claims_df[which(cleaned_Claims_df[,"claims_date"] == curr_date),]
    
    #medicaid
    if (is.null(cleaned_healthClaims) == F){ #if has health claims in medcaid and medicare
      curr_day_healthclaims <- cleaned_healthClaims[which(cleaned_healthClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_diag <- remove_NA_func(unique(c(unlist(curr_day_healthclaims[,medicaid_diag_cols]),unlist(curr_day_claims[,medicare_diag_cols]))))
      curr_day_proc <- remove_NA_func(unique(c(unlist(curr_day_healthclaims[,medicaid_proc_cols]),unlist(curr_day_claims[,medicare_proc_cols]))))
      
    }else{#only medicare
      curr_day_diag <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_diag_cols]))) 
      curr_day_proc <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_proc_cols])))
    }
    
    if (is.null(cleaned_PharmClaims) == F){ #if has pharms claims and medicare
      curr_day_drugclaims   <- cleaned_PharmClaims[which(cleaned_PharmClaims[,"DTE_FIRST_SVC"] == curr_date),]
      curr_day_drug <- remove_NA_func(unique(c(unlist(curr_day_drugclaims[,medicaid_drug_cols]),unlist(curr_day_claims[,medicare_drug_cols]))))
      
    }else{ #only medicare
      curr_day_drug <- remove_NA_func(unique(unlist(curr_day_claims[,medicare_drug_cols])))
    }
    
    
    #Concatenate all codes
    if (length(curr_day_diag) > 0){
      per_day_table[j,"Diag_Codes"] <- paste0(curr_day_diag,collapse = "$$$$")
    }
    if (length(curr_day_proc) > 0){
      per_day_table[j,"Proc_Codes"] <- paste0(curr_day_proc,collapse = "$$$$")
    }
    if (length(curr_day_drug) > 0){
      per_day_table[j,"Drug_Codes"] <- paste0(curr_day_drug,collapse = "$$$$")
    }
    
    
  }
  
  #remove the dates that does not have any type of codes
  na_ct_perDay <- rowSums(is.na(per_day_table[,c("Diag_Codes","Proc_Codes","Drug_Codes")]))
  indexes_toremove <- which(na_ct_perDay == 3)
  if (length(indexes_toremove) >0 ){
    per_day_table <- per_day_table[-indexes_toremove,]
  }
  
  return(per_day_table)
}


#This function read all claims, if not avaiabliable, return NULL
read_allClaims <- function(patient_ID,medicaid_heath_dir,medicaid_pharm_dir,medicare_dir){
  #Codes columns for medicaid
  medicaid_diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
  medicaid_proc_cols <- c("CDE_PROC_PRIM")
  medicaid_drug_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
  
  #Codes columns for medicare
  medicare_diag_cols <- paste0("DGNS_CD",seq(1,25))
  medicare_proc_cols <- c(paste0("PRCDRCD",seq(1,25)),"HCPCS_CD")
  medicare_drug_cols <- c("NDC_CD","PROD_SRVC_ID")
  medicare_drug_related <- c("GNN","BN")
  
  
  #1.read original health_claims to get proc and diag codes
  IDs_has_health_files <- as.numeric(gsub("_all_medicaid_healthclaims.xlsx|ID","",list.files(medicaid_heath_dir)))
  if (patient_ID %in% IDs_has_health_files){  #check if ID in the dir
    medicaid_health_df <- read.xlsx(paste0(medicaid_heath_dir,"ID",patient_ID,"_all_medicaid_healthclaims.xlsx"),sheet = 1,detectDates = T)
    medicaid_health_df[,"DTE_FIRST_SVC"] <- mdy(medicaid_health_df[,"DTE_FIRST_SVC"])
    medicaid_health_df <- medicaid_health_df[,c("study_id","Id_medicaid",
                                                "DTE_FIRST_SVC",
                                                medicaid_diag_cols,
                                                medicaid_proc_cols)]
  }else{
    medicaid_health_df <- NULL
  }
  
  #2.get original pharmcy_claims to get drug codes
  IDs_has_pharm_files <- as.numeric(gsub("_all_medicaid_pharmclaims.xlsx|ID","",list.files(medicaid_pharm_dir)))
  if (patient_ID %in% IDs_has_pharm_files){ # #check if ID in the dir
    medicaid_pharm_df <- read.xlsx(paste0(medicaid_pharm_dir,"ID",patient_ID,"_all_medicaid_pharmclaims.xlsx"),sheet = 1,detectDates = T)
    medicaid_pharm_df[,"DTE_FIRST_SVC"] <- dmy(medicaid_pharm_df[,"DTE_FIRST_SVC"])
    medicaid_pharm_df <- medicaid_pharm_df[,c("study_id","Id_medicaid","DTE_FIRST_SVC",
                                              medicaid_drug_cols)]
  }else{
    medicaid_pharm_df <- NULL
  }
  
  #3.Get original medicare_claims
  IDs_has_medicare_files <- as.numeric(gsub("_all_medicare_claims.xlsx|ID","",list.files(medicare_dir)))
  if (patient_ID %in% IDs_has_medicare_files){
    medicare_df <- read.xlsx(paste0(medicare_dir,"ID",patient_ID,"_all_medicare_claims.xlsx"),sheet = 1,detectDates = T)
    medicare_df[,"claims_date"] <- mdy(medicare_df[,"claims_date"])
    medicare_df <- medicare_df[,c("study_id","claims_date",
                                  medicare_diag_cols,
                                  medicare_proc_cols,
                                  medicare_drug_cols,
                                  medicare_drug_related)]
  }else{
    medicare_df <- NULL
  }
  
  return(list(medicaid_health_df,medicaid_pharm_df,medicare_df))
}


#This function get unique code in original claims data
get_unique_codes <- function(claims_df,code_columns,code_source,code_type){
  # claims_df <- cleaned_medicareClaims_df
  # code_columns <- medicare_diag_cols
  # code_source <- "ICD"
  # code_type <- "Diag"
  
  #get unique codes
  unique_codes_df <- as.data.frame(unique(unlist(claims_df[,code_columns])))
  colnames(unique_codes_df) <- paste0("Unique_", code_type,"_Code")
  
  #add code source info
  unique_codes_df$Code_Coumn  <-  paste0(unique(gsub("[[:digit:]]","",code_columns)),collapse = "$$$$")
  unique_codes_df$Code_Source <- code_source
  
  #remove NA
  na_idxes <- which(is.na(unique_codes_df[,1]) == T)
  unique_codes_df <- unique_codes_df[-na_idxes,]
  
  return(unique_codes_df)
}


######################################################################
#return unique codes
split_code_strings_to_unique_codes <- function(code_df, code_col){
  #this function split code strings in a column, and return unique codes
  code_strings <- code_df[,code_col]
  
  #remove NAs
  na_idxes <- which(is.na(code_strings)==T)
  if (length(na_idxes) > 0 ){
    code_strings <- code_strings[-na_idxes]
  }
  
  if (length(code_strings) >0){ #if remaining code length > 0
    unique_splited_codes <- unique(unlist(strsplit(code_strings,split= "$$$$",fixed = T)))
  }else{
    unique_splited_codes <- NULL
  }
  return(unique_splited_codes)
}

#return unique codes
split_andcombine_codes <- function(code_df,code_col){
  #This function split concatenated code strings in a data column into individual code, 
  #then concatednete all codes in a column into one string
   
  unique_splited_codes <- split_code_strings_to_unique_codes(code_df,code_col)

  if (is.null(unique_splited_codes) == F){
      combined_codes <- paste0(unique_splited_codes,collapse = "$$$$")
  }else{
      combined_codes <- NA
  }
  return(combined_codes)
}


#this one does not return unique codes
split_code_strings_to_non_unique_codes <- function(code_df, code_col){
  #this function split code strings in a column, and return unique codes
  code_strings <- code_df[,code_col]
  
  #remove NAs
  na_idxes <- which(is.na(code_strings)==T)
  if (length(na_idxes) > 0 ){
    code_strings <- code_strings[-na_idxes]
  }
  
  if (length(code_strings) >0){ #if remaining code length > 0
    unique_splited_codes <- unlist(strsplit(code_strings,split= "$$$$",fixed = T))
  }else{
    unique_splited_codes <- NULL
  }
  return(unique_splited_codes)
}


#this one combine all non-unique codes
split_andcombine_codes2 <- function(code_df,code_col){
  #This function split concatenated code strings in a data column into individual code, 
  #then concatednete all codes in a column into one string
  
  unique_splited_codes <- split_code_strings_to_non_unique_codes(code_df,code_col)
  
  if (is.null(unique_splited_codes) == F){
    combined_codes <- paste0(unique_splited_codes,collapse = "$$$$")
  }else{
    combined_codes <- NA
  }
  return(combined_codes)
}

#Compute missings
compute_missing_rate <- function(data_input,col_name){
  n_missing <- length(which(is.na(data_input[,col_name])==T))
  perc_missing <- round(n_missing*100/nrow(data_input),2)
  res <- paste0(n_missing," (",perc_missing, "%)")
  return(res)
}

get_missing_rate_table <- function(data_df,features){
  missing_table <- as.data.frame(matrix(NA, nrow = length(features), ncol = 2))
  colnames(missing_table) <- c("Feature","Missing_N_Perc")
  for (j in 1:length(features)){
    curr_col <- features[j]
    missing_table[j,"Feature"] <- curr_col
    missing_table[j,"Missing_N_Perc"]  <- compute_missing_rate(data_df,curr_col)
  }
  return(missing_table)
}


#######Construct code feature df
get_code_feature_df_func <- function(input_data,grouped_code_df,code_col_ingrpdf,code_col_ininputdf,group_col){
   # input_data <- All_data
   # grouped_code_df <- grouped_unique_proc_df
   # code_col_ingrpdf <- "Unique_Proc_Codes"
   # code_col_ininputdf <- "Proc_Codes"
   # group_col <- "Ritzwoller_Category"
   # 
   unique_groups <- sort(unique(unlist(strsplit(grouped_code_df[,group_col],split = "$$$$",fixed = T)))) #str in case the code belongs to two seperated groups
   
   feature_df <- as.data.frame(matrix(0, nrow = nrow(input_data), ncol = length(unique_groups)+2))
   colnames(feature_df) <- c("study_id","Month_Start",unique_groups)
    for (i in 1:nrow(input_data)){
      if (i %% 500 == 0){print(i)}
      curr_df <- input_data[i,code_col_ininputdf]
      feature_df[i,"study_id"] <- input_data[i,"study_id"]
      feature_df[i,"Month_Start"] <- input_data[i,"Month_Start"]
      
      if (is.na(curr_df) == F){
        curr_codes <- split_code_strings_to_non_unique_codes(input_data[i,],code_col_ininputdf)
        if (code_col_ininputdf == "Drug_Codes"){ #if drug codes, remove leading 0s due to omitting zeros in wrting unique_drug_df to csv
          curr_codes <- str_remove(curr_codes, "^0+")
        }
        curr_code_idxes <- match(curr_codes, grouped_code_df[,code_col_ingrpdf])
        curr_groups <- grouped_code_df[curr_code_idxes,group_col]
        curr_groups <- unlist(strsplit(curr_groups,split = "$$$$",fixed = T)) #strpslit in the case of one code in two groups (e.g,A9605 in proc codes in ritzwoller group)
        curr_groups_freq_tb <- data.frame(table(curr_groups))
        curr_feature_cols <- as.character(curr_groups_freq_tb[,"curr_groups"])
        feature_df[i,curr_feature_cols] <- curr_groups_freq_tb[,"Freq"]
      }
      
    }
  return(feature_df)
}

#######Construct code transformation feature df
read_codecount_df <- function(code_count_file,selected_codegrps){
  #read feature count data and selected features
  # code_count_file <- ccs_diag_file
  # selected_codegrps <- selected_Features
  # 
  counting_df    <- read.xlsx(code_count_file,sheet = 1)
  counting_df    <- counting_df[,which(colnames(counting_df) %in% c("study_id","Month_Start",selected_codegrps))]
  return(counting_df)
}
apply_code_transforamtion_func <- function(counting_df){

  time_since_df  <- add_time_since_func(counting_df)
  time_until_df  <- add_time_until_func(counting_df)
  cumul_ratio_df <- add_cumul_ratio_func(counting_df)
  
  transf_df <- cbind(time_since_df,time_until_df,cumul_ratio_df)
  
  return(transf_df)
}

add_time_since_func <-function(pt_perMonth_df){
  #the time since the most recent occurrence of this code group
  #pt_perMonth_df <- curr_grp_f_df
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  
  #get the month index as the actuall month difference to the first month,  #Use interger as month sequence for easier computation
  pt_perMonth_df$Month_Index <-   abs(interval(ymd(pt_perMonth_df[,"Month_Start"]), ymd(pt_perMonth_df[1,"Month_Start"])) %/% months(1)) + 1 
  
  #relocate cols
  pt_perMonth_df <- pt_perMonth_df %>% relocate(Month_Index, .after = Month_Start)
  
  #'@NEW Find  the columns has all 0s (no months of this pts has the code group)
  count_forallgrps    <- colSums(pt_perMonth_df[,4:ncol(pt_perMonth_df)])
  colnames_toskip     <- names(which(count_forallgrps == 0))  #columns to skip in the following computation
  colindex_toskip     <- which(colnames(pt_perMonth_df) %in% colnames_toskip)
  
  time_since_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(time_since_df) <- colnames(pt_perMonth_df)
  colnames(time_since_df)[4:ncol(time_since_df)] <- paste0("time_since_",colnames(time_since_df)[4:ncol(time_since_df)])
  
  #'@Switched 1201 the position of the following code
  time_since_df[,"study_id"]    <- pt_perMonth_df[,"study_id"]
  time_since_df[,"Month_Start"] <- pt_perMonth_df[,"Month_Start"]
  time_since_df[,"Month_Index"] <- pt_perMonth_df[,"Month_Index"]
  
  #code group col started at index 4
  for(j in 4:ncol(time_since_df)){ #for each code group 
    most_recent_month <- Inf #initial a most recent month as Future for each code group
    
    if (!j %in% colindex_toskip){ #'@NEW
          for (i in 1:nrow(pt_perMonth_df)){ #for each month
            curr_month_df <- pt_perMonth_df[i,]
            #time_since_df[i,"study_id"] <- curr_month_df[,"study_id"]
            #time_since_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
            #time_since_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
            
            curr_count <- curr_month_df[,j]
            curr_month <- curr_month_df[,"Month_Index"]
            if (curr_count >= 1){ #if count of group in current month is >=1
              time_since_df[i,j] <- 0
              most_recent_month <- curr_month #update most recent month
            }else { #if count og group in current moneth is 0
              time_since_df[i,j] <- curr_month - most_recent_month
            }
          }
     }
  }
   
  time_since_df[which(is.na(time_since_df)==T,arr.ind = T)] <- -1 #'@NEW recode NA to -1, NAs is from skiping columns
  time_since_df[which(time_since_df== "-Inf",arr.ind = T)] <- -1 #recode -INF to -1 for months that has never seen a code
  return(time_since_df)
}


add_time_until_func <-function(pt_perMonth_df){
  #the time since the soonest future occurrence of this code group
  #pt_perMonth_df <- curr_CCS_counting_df_diag
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  
  #get the month index as the actuall month difference to the first month,  #Use interger as month sequence for easier computation
  pt_perMonth_df$Month_Index <-   abs(interval(ymd(pt_perMonth_df[,"Month_Start"]), ymd(pt_perMonth_df[1,"Month_Start"])) %/% months(1)) + 1 
  
  #relocate cols
  pt_perMonth_df <- pt_perMonth_df %>% relocate(Month_Index, .after = Month_Start)
  
  #'@NEW Find  the columns has all 0s (no months of this pts has the code group)
  count_forallgrps    <- colSums(pt_perMonth_df[,4:ncol(pt_perMonth_df)])
  colnames_toskip     <- names(which(count_forallgrps == 0))  #columns to skip in the following computation
  colindex_toskip     <- which(colnames(pt_perMonth_df) %in% colnames_toskip)
  
  
  time_until_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(time_until_df) <- colnames(pt_perMonth_df)
  colnames(time_until_df)[4:ncol(time_until_df)] <- paste0("time_until_",colnames(time_until_df)[4:ncol(time_until_df)])
  
  #'@Switched 1201 the position of the following code
  time_until_df[,"study_id"]    <- pt_perMonth_df[,"study_id"]
  time_until_df[,"Month_Start"] <- pt_perMonth_df[,"Month_Start"]
  time_until_df[,"Month_Index"] <- pt_perMonth_df[,"Month_Index"]
  
  #code group col started at index 4
  for(j in 4:ncol(time_until_df)){ #for each code group 
    soonest_future_month <- -Inf #initial a soonest future month as past(-INF) for each code group
    if (!j %in% colindex_toskip){ #'@NEW
       for (i in nrow(pt_perMonth_df):1){ #for each month from latest to oldest
        curr_month_df <- pt_perMonth_df[i,]
        #time_until_df[i,"study_id"] <- curr_month_df[,"study_id"]
        #time_until_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
        #time_until_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
        
        curr_count <- curr_month_df[,j]
        curr_month <- curr_month_df[,"Month_Index"]
        if (curr_count >= 1){ #if count of group in current month is >=1
          time_until_df[i,j] <- 0
          soonest_future_month <- curr_month #update soonest future month 
        }else { #if count og group in current moneth is 0
          time_until_df[i,j] <- soonest_future_month - curr_month
        }
      }
    }
  }
  
  time_until_df[which(is.na(time_until_df)==T,arr.ind = T)] <- -1 #'@NEW recode NA to -1, NAs is from skiping columns
  time_until_df[which(time_until_df== "-Inf",arr.ind = T)] <- -1 #recode -INF to -1 for months that has never seen a code
  return(time_until_df)
}


add_cumul_ratio_func <-function(pt_perMonth_df){
  #the total number of occurrences in each patient up to the time in question of that grouping divided by total elapsed time
  #pt_perMonth_df <- curr_grp_f_df
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  #get the month index as the actuall month difference to the first month,  #Use interger as month sequence for easier computation
  pt_perMonth_df$Month_Index <-   abs(interval(ymd(pt_perMonth_df[,"Month_Start"]), ymd(pt_perMonth_df[1,"Month_Start"])) %/% months(1)) + 1 
  #relocate cols
  pt_perMonth_df <- pt_perMonth_df %>% relocate(Month_Index, .after = Month_Start)
  
  #'@NEW Find  the columns has all 0s (no months of this pts has the code group)
  count_forallgrps    <- colSums(pt_perMonth_df[,4:ncol(pt_perMonth_df)])
  colnames_toskip     <- names(which(count_forallgrps == 0))  #columns to skip in the following computation
  colindex_toskip     <- which(colnames(pt_perMonth_df) %in% colnames_toskip)
  
  
  cumul_ratio_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(cumul_ratio_df) <- colnames(pt_perMonth_df)
  colnames(cumul_ratio_df)[4:ncol(cumul_ratio_df)] <- paste0("cumul_ratio_",colnames(cumul_ratio_df)[4:ncol(cumul_ratio_df)])
  
  #'@Switched 1201 the position of the following code
  cumul_ratio_df[,"study_id"]    <- pt_perMonth_df[,"study_id"]
  cumul_ratio_df[,"Month_Start"] <- pt_perMonth_df[,"Month_Start"]
  cumul_ratio_df[,"Month_Index"] <- pt_perMonth_df[,"Month_Index"]
  
  
  #code group col started at index 4
  for(j in 4:ncol(cumul_ratio_df)){ #for each code group 
    curr_cum_count <- 0 #inital cumalitive count as 0 
    if (!j %in% colindex_toskip){ #'@NEW
        for (i in 1:nrow(pt_perMonth_df)){ #for each month
          #get month data and assign to the new dataframe
          curr_month_df <- pt_perMonth_df[i,]
          #cumul_ratio_df[i,"study_id"] <- curr_month_df[,"study_id"]
          #cumul_ratio_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
          #cumul_ratio_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
          
          #get count and month index
          curr_count <- curr_month_df[,j]
          curr_month <- curr_month_df[,"Month_Index"]
          
          #cumalative count 
          curr_cum_count <- curr_cum_count + curr_count
          cumul_ratio_df[i,j] <- round(curr_cum_count/curr_month,4)
        }
    }
  }
  
  cumul_ratio_df[which(is.na(cumul_ratio_df)==T,arr.ind = T)] <- -1 #'@NEW recode NA to -1, NAs is from skiping columns
  
  return(cumul_ratio_df)
}


####Count code freq
get_code_group_freq <- function(pt_files_dir,code_group_name){
  pt_files <- list.files(pt_files_dir,full.names = T)
  allpts_df <- do.call(rbind,lapply(pt_files, read.xlsx))
  code_groups <- unique(colnames(allpts_df)[which(grepl(code_group_name,colnames(allpts_df))==T)])
  grp_freq_tb <- as.data.frame(matrix(NA, nrow = length(code_groups), ncol = 3))
  colnames(grp_freq_tb) <- c("Code_Group","N_SamplesHASCode","Perc_SamplesHASCode")
  for (i in 1:length(code_groups)){
    curr_grp <- code_groups[i]
    
    n_HasCode_incurrGrp <- length(which(allpts_df[,curr_grp]>=1))
    perc_HasCode_incurrGrp <- n_HasCode_incurrGrp/nrow(allpts_df)
    
    grp_freq_tb[i, "Code_Group"] <- curr_grp
    grp_freq_tb[i, "N_SamplesHASCode"] <- n_HasCode_incurrGrp
    grp_freq_tb[i, "Perc_SamplesHASCode"] <- perc_HasCode_incurrGrp
  }
  
  return(grp_freq_tb)
}


get_grp_discription_func <- function(code_grp,grp_prefix,discrip_df,grp_col,grp_discrip_col){
  indxes <- which(discrip_df[,grp_col] == gsub(grp_prefix,"",code_grp))
  discrip <- gsub("[[:punct:]]"," ",discrip_df[indxes,grp_discrip_col])
  discrip <- unique(trimws(discrip, which = c("both"), whitespace = "[ \t\r\n]"))
  discrip <- paste0(discrip, collapse = "&&") #in the case one grp has multiple discriptions
  return(discrip)
}

##########################################################################################
# Functions for cleaning code and grouping code
##########################################################################################
######################################################################
#this function cleans codes in the original claims data by columns 
clean_code_columns <-function(claims_df, col_to_clean){
  #claims_df <- curr_health_df
  #col_to_clean <- c(diag_cols,proc_cols)
  for (j in 1:length(col_to_clean)){
    curr_col <- col_to_clean[j]
    claims_df[,curr_col] <- clean_code_func(claims_df[,curr_col])
  }  
  return(claims_df)
}

#remove NA from a list of code
remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}

#remove NA from dataframe col
remove_NA_from_df <- function(input_df,col_tocheck){
  na_indexes <- which(is.na(input_df[,col_tocheck])==T)
  if (length(na_indexes) > 0){
    input_df <- input_df[-na_indexes,]
  }
  return(input_df)
}


get_uniquecodes_onetype <-function(in_data,code_type, code_col,claim_source){
  #in_data <- data_df1
  #code_col <- HCPCS_proc_cols
  
  #Read code columns
  code_data      <-  data.frame(in_data[,code_col])
  colnames(code_data) <- code_col #for data only has one column
  
  #Convert integer columns to character
  for (i in 1:length(code_col)){
    curr_col <- code_col[i]
    code_data[,curr_col] <- as.character(code_data[,curr_col])
  }
  
  #Get non-NA  and non-Blanks unique codes
  non_na_or_blanks <- which(is.na(code_data) == F & code_data != "",arr.ind = T)
  unique_code_list <- unique(code_data[non_na_or_blanks])

  #Unique code df
  unique_code_df      <-  data.frame(paste0("CODE_",unique_code_list))
  colnames(unique_code_df) <- "CODE"
  unique_code_df$TYPE  <- code_type
  unique_code_df$CLAIM <- claim_source
  
  return(unique_code_df)
}

#This function prepend 0s or remove code (if non-numeric, remove)
reformat_codes_func <-function(code,min_length){
  if(is.na(as.numeric(code)==T)){ #if non-numeric (if NA when converting)
    updated_code <- NA #remove
  }else{ #it is numeric , then prepending "0"s to match the code minimum length
    nchar_code   <- nchar(code)
    num_0_needed <- min_length - nchar_code
    prepend_string <- paste0(rep("0",num_0_needed),collapse = "")
    updated_code <- paste0(prepend_string,code)
  }
  
  return(updated_code)
}


clean_code_func <-function(list_of_codes){
  #1.repalce any codes with non-alphanumeric characters with ""
  updated_list_of_codes<- gsub("[^[:alnum:]]", "", list_of_codes)
  
  #2.replace decimal with ""
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  
  #3. replace space with ""
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #4.Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #5.for codes less than 3 characters long
  #.if it is non-numeric, then exclude codes 
  # if it is numeric , then prepending '0'
  l3_idxes <- which(n_char< 3)
  l3_codes <- updated_list_of_codes[l3_idxes]
  if(length(l3_codes) > 0){
    updated_code <- NA
    for (c in 1:length(l3_codes)){
      cur_code <- l3_codes[c]
      if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric
        updated_code[c] <- NA #remove
      }else{ #it is numeric , then prepending '0'
        curr_nchar <- length(cur_code)
        if (length(curr_nchar) == 1){
          updated_code[c] <- paste0("00",cur_code)
        }else if (length(curr_nchar) == 2){
          updated_code[c] <- paste0("0",cur_code)
        }
      }
    }
    #'@NOTE: This might result in NAs for codess less than 3 nchar (from converting to numeric when it is char) from orignal 
    
    updated_list_of_codes[l3_idxes] <- updated_code
    
  }
  
  return(updated_list_of_codes)
}

#This function also consider the type of code
#For ICD code, 3 digit
#For HCPCS code, 5 digit
#'@NOTE: This might result in NAs for codess less than 3/5 nchar (from converting to numeric when it is char) from orignal 
clean_code_func2 <-function(list_of_codes,list_of_types){
  #1.repalce any codes with non-alphanumeric characters with ""
  updated_list_of_codes<- gsub("[^[:alnum:]]", "", list_of_codes)
  
  #2.replace decimal with ""
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  
  #3. replace space with ""
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #4. For each code, 
  #   #if HCPCS, reformat codes less than 5 char long
  #   #if ICD, reformat codes less than 3 char long
  for (i in 1:length(updated_list_of_codes)){
    curr_code <- updated_list_of_codes[i]
    curr_nchar <- nchar(curr_code)
    curr_type <-  list_of_types[i]
    
    if (is.na(curr_code) == T | curr_code == ""){
      curr_code <- NA
    }else if (grepl("HCPC",curr_type,ignore.case = T) == T){ #if HCPCS, reformat codes less than 5 char long
      if (curr_nchar < 5){
        curr_code <- reformat_codes_func(curr_code,5)
      }
    }else if (grepl("ICD",curr_type,ignore.case = T) == T){ #if ICD, reformat codes less than 3 char long
      if (curr_nchar < 3){
        curr_code <- reformat_codes_func(curr_code,3)
      }
    }
     updated_list_of_codes[i] <- curr_code
  }
  

  return(updated_list_of_codes)
}


####Grouping functions
#CCS1
load_and_clean_CSS_data<- function(file_dir){
  #Load four tables
  HCUP_Diag1_df <- read.csv(paste0(file_dir,"Code_Groups/New_HCUP_CCS_Data/icd9_dxref 2015.csv"),stringsAsFactors = F,skip = 1)
  HCUP_Diag2_df <- read.csv(paste0(file_dir,"Code_Groups/New_HCUP_CCS_Data/ccs_dx_icd10cm_2019_1.csv"),stringsAsFactors = F)
  HCUP_Proc1_df <- read.csv(paste0(file_dir,"Code_Groups/New_HCUP_CCS_Data/icd9_prref 2015.csv"),stringsAsFactors = F,skip = 1)
  HCUP_Proc2_df <- read.csv(paste0(file_dir,"Code_Groups/New_HCUP_CCS_Data/ccs_pr_icd10pcs_2020_1.csv"),stringsAsFactors = F)
  
  #Change col name to combine
  colnames(HCUP_Diag1_df) <- gsub("\\.$|X.","",colnames(HCUP_Diag1_df)) #remove the last . and X.
  colnames(HCUP_Diag2_df) <- gsub("\\.$|X.","",colnames(HCUP_Diag2_df))
  colnames(HCUP_Proc1_df) <- gsub("\\.$|X.","",colnames(HCUP_Proc1_df))
  colnames(HCUP_Proc2_df) <- gsub("\\.$|X.","",colnames(HCUP_Proc2_df))
  
  colnames(HCUP_Diag1_df)[which(colnames(HCUP_Diag1_df) == "ICD.9.CM.CODE")] <- "Code"
  colnames(HCUP_Diag2_df)[which(colnames(HCUP_Diag2_df) == "ICD.10.CM.CODE")] <- "Code"
  colnames(HCUP_Proc1_df)[which(colnames(HCUP_Proc1_df) == "ICD.9.CM.CODE")] <- "Code"
  colnames(HCUP_Proc2_df)[which(colnames(HCUP_Proc2_df) == "ICD.10.PCS.CODE")] <- "Code"
  
  
  
  HCUP_Diag1_df$CODE_TYPE <- "ICD9_Diag"
  HCUP_Diag2_df$CODE_TYPE <- "ICD10_Diag"
  HCUP_Proc1_df$CODE_TYPE <- "ICD9_Proc"
  HCUP_Proc2_df$CODE_TYPE <- "ICD10_Proc"
  
  HCUP_comb <- rbind(HCUP_Diag1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                     HCUP_Diag2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                     HCUP_Proc1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                     HCUP_Proc2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")])
  
  #3.clean code and category
  HCUP_comb[,"Code"]  <- clean_code_func2(HCUP_comb[,"Code"],HCUP_comb[,"CODE_TYPE"])
  HCUP_comb[,"CCS.CATEGORY"]  <- clean_code_func2(HCUP_comb[,"CCS.CATEGORY"],HCUP_comb[,"CCS.CATEGORY"]) #2nd entry does not matter
  
  
  #4. Remove all blanks and NAs   
  HCUP_comb <- remove_NA_from_df(HCUP_comb,"Code")
  
  #5. remove the unspecified codes, cuz they will result codes grped into multiple groups, (e.g, E8342 is a qulified codes in ICD10, but unspecified in ICD9)
  unspecified_idxes <- which(grepl("\\be codes|\\bExternal cause codes",HCUP_comb[,"CCS.CATEGORY.DESCRIPTION"],ignore.case = T)==T)
  HCUP_comb<- HCUP_comb[-unspecified_idxes,]
  return(HCUP_comb)
}

#CCS2
load_and_clean_CSS_ServicesP_data<- function(file_dir){
  #Load  tables
  HCUP_SProc_df <- read.csv(paste0(file_dir,"Code_Groups/New_HCUP_CCS_Data/CCS_ServicesProcedures_v2021-1/CCS_services_procedures_v2021-1.csv"),stringsAsFactors = F,skip = 1)
  
  #change col names
  colnames(HCUP_SProc_df)[which(colnames(HCUP_SProc_df) == "CCS.Label")] <- "CCS.CATEGORY.DESCRIPTION"
  colnames(HCUP_SProc_df)[which(colnames(HCUP_SProc_df) == "CCS")] <- "CCS.CATEGORY"
  
  #Split the ranges
  ranges_splited <- strsplit(HCUP_SProc_df[,"Code.Range"] ,split = "-")
  
  #Add type col
  HCUP_SProc_df[,"CODE_TYPE"] <- "HCPCS_Proc"
  
  #Store in df
  HCUP_SProc_df[,"Code_Range_Low"] <- sapply(ranges_splited, "[[", 1)
  HCUP_SProc_df[,"Code_Range_High"] <- sapply(ranges_splited, "[[", 2)
  
  #clean 
  HCUP_SProc_df[,"Code_Range_Low"]   <- clean_code_func2(HCUP_SProc_df[,"Code_Range_Low"],HCUP_SProc_df[,"CODE_TYPE"])
  HCUP_SProc_df[,"Code_Range_High"]  <- clean_code_func2(HCUP_SProc_df[,"Code_Range_High"],HCUP_SProc_df[,"CODE_TYPE"]) 
  HCUP_SProc_df[,"CCS.CATEGORY"]  <- clean_code_func2(HCUP_SProc_df[,"CCS.CATEGORY"],HCUP_SProc_df[,"CCS.CATEGORY"])   #2nd entry does not matter
  
  #4. Remove all blanks and NAs   
  HCUP_SProc_df <- remove_NA_from_df(HCUP_SProc_df,"Code_Range_Low")
  HCUP_SProc_df <- remove_NA_from_df(HCUP_SProc_df,"Code_Range_High")
  
  return(HCUP_SProc_df)
}


group_codes_into_CCS_func <- function(claim_code_df,CCS_ICD_df,CCS_HCPCS_df){
  claim_code_df$CCS_CATEGORY <- NA
  claim_code_df$CCS_CATEGORY_DESCRIPTION <- NA
  
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,"CODE"]
    curr_type <-  claim_code_df[i,"TYPE"]
    
    if (grepl("ICD",curr_type,ignore.case = T) == T){ #if ICD proc or ICD diag
        curr_ccs_idxes <- which(CCS_ICD_df[,"Code"] == curr_code)
      if (length(curr_ccs_idxes) > 0){
        curr_category <- CCS_ICD_df[curr_ccs_idxes,"CCS.CATEGORY"]
        curr_discrpt  <- CCS_ICD_df[curr_ccs_idxes,"CCS.CATEGORY.DESCRIPTION"]
      }else{
        curr_category <- NA
        curr_discrpt <- NA
      }
    }else if (curr_type == "PROC_HCPCS"){ #if HCPCS
        curr_ccs_idxes <- which(CCS_HCPCS_df[,"Code_Range_Low"] <= curr_code & CCS_HCPCS_df[,"Code_Range_High"] >= curr_code)
      if (length(curr_ccs_idxes) > 0){
        curr_category <- CCS_HCPCS_df[curr_ccs_idxes,"CCS.CATEGORY"]
        curr_discrpt  <- CCS_HCPCS_df[curr_ccs_idxes,"CCS.CATEGORY.DESCRIPTION"]
      }else{
        curr_category <- NA
        curr_discrpt <- NA
      }
    }
    
    claim_code_df[i,"CCS_CATEGORY"] <- curr_category
    claim_code_df[i,"CCS_CATEGORY_DESCRIPTION"] <- curr_discrpt
  }
  return(claim_code_df)
}



#Chubak
load_and_clean_Chubak_data<- function(file_dir){
  #Load four tables
  chubak_group_df <- read.csv(paste0(file_dir,"Code_Groups/BRAVA_lookup.20180502.edit.csv"),stringsAsFactors = F)
  length(unique(chubak_group_df$Type)) #216
  length(unique(chubak_group_df$Category)) #22
  
  #3.clean code 
  chubak_group_df[,"Code"]<- clean_code_func2(chubak_group_df[,"Code"],chubak_group_df[,"Code.type"])
  
  #4. Remove all blanks and NAs   
  chubak_group_df <- remove_NA_from_df(chubak_group_df,"Code")
  
  return(chubak_group_df)
}
group_codes_into_chubak_func <- function(claim_code_df,chubak_grp_df){
  claim_code_df[,"Chubak_Type"] <- NA
  claim_code_df[,"Chubak_Category"] <- NA
  claim_code_df[,"Chubak_Description"] <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,1]
    curr_idxes <- which(chubak_grp_df[,"Code"] == curr_code)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"Chubak_Type"]     <- paste0(unique(chubak_grp_df[curr_idxes,"Type"]),collapse = "$$$$")
      claim_code_df[i,"Chubak_Category"] <- paste0(unique(chubak_grp_df[curr_idxes,"Category"]),collapse = "$$$$")
      
      claim_code_df[i,"Chubak_Description"] <-  paste0(unique(chubak_grp_df[curr_idxes,"Code.Description"]),collapse = "$$$$")
      
    }
  }
  return(claim_code_df)
}

#Ritzwoller
load_and_clean_Ritzwoller_data<- function(file_dir){
  #Load four tables
  Ritzwoller_group_df <- read.csv(paste0(file_dir,"Code_Groups/Ritzwoller_code_table.edited.csv"),stringsAsFactors = F)
  length(unique(Ritzwoller_group_df$Category)) #6
  
  #3.clean code 
  Ritzwoller_group_df[,"Code"] <- clean_code_func2(Ritzwoller_group_df[,"Code"],Ritzwoller_group_df[,"Code.type"])
  
  #4. Remove all blanks and NAs   
  Ritzwoller_group_df <- remove_NA_from_df(Ritzwoller_group_df,"Code")
  
  return(Ritzwoller_group_df)
}

group_codes_into_Ritzwoller_func <- function(claim_code_df,Ritzwoller_grp_df){
  claim_code_df[,"Ritzwoller_Type"] <- NA
  claim_code_df[,"Ritzwoller_Category"] <- NA
  claim_code_df[,"Ritzwoller_Description"] <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,1]
    curr_idxes <- which(Ritzwoller_grp_df[,"Code"] == curr_code)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"Ritzwoller_Type"]     <- paste0(unique(Ritzwoller_grp_df[curr_idxes,"Code.type"]),collapse = "$$$$")
      claim_code_df[i,"Ritzwoller_Category"] <- paste0(unique(Ritzwoller_grp_df[curr_idxes,"Category"]),collapse = "$$$$")
      
      claim_code_df[i,"Ritzwoller_Description"] <-  paste0(unique(Ritzwoller_grp_df[curr_idxes,"Description"]),collapse = "$$$$")
      
    }
  }
  return(claim_code_df)
}
#DM3 grouping
load_and_clean_DM3_data<- function(file_dir){
  #Load data
  drug_group_df <- read.csv(paste0(file_dir,"Code_Groups/Drug Code Groups-DM3.sorted.csv"),stringsAsFactors = F)
  drug_group_df <- drug_group_df[,-1]
  
  # #Clean drug name by removing the source prefix
  drug_group_df[,"desc"] <- gsub("NC: |NH: |NO: |NS: ","",drug_group_df[,"desc"])
  drug_group_df[,"desc"] <- gsub("[[:punct:]]"," ",drug_group_df[,"desc"])
  drug_group_df[,"desc"] <- trimws(drug_group_df[,"desc"], which = c("both"), whitespace = "[ \t\r\n]")
  
  
  return(drug_group_df)
}

#this function use drug name to group
group_drugcodes_into_DM3_func <- function(claim_code_df,DM3_df){
  claim_code_df$specific_group <- NA
  claim_code_df$general_group <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,"DRUG_NAME"]
    curr_idxes <- which(DM3_df[,"desc"] == curr_code)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"specific_group"] <-  unique(DM3_df[curr_idxes,"specific_group"])
      claim_code_df[i,"general_group"]  <-  unique(DM3_df[curr_idxes,"general_group"])
    }
  }
  return(claim_code_df)
}

#this function use short_GNN to group
group_drugcodes_into_DM3_funcV2 <- function(claim_code_df,DM3_df){
  claim_code_df$specific_group <- NA
  claim_code_df$general_group <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_gnn <- claim_code_df[i,"short_GNN"]
    curr_idxes <- which(DM3_df[,"short_code"] == curr_gnn)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"specific_group"] <-  unique(DM3_df[curr_idxes,"specific_group"])[1] #if there is still multiple, choose the 1st one
      claim_code_df[i,"general_group"]  <-  unique(DM3_df[curr_idxes,"general_group"])[1]
    }
  }
  return(claim_code_df)
}

#3A_HPC_Get_PerMonth with clean codes
get_claims_inDateRange <- function(in_data,time_col,start_d, end_d){
  dat_inds <- which(in_data[,time_col] >= start_d & 
                      in_data[,time_col] < end_d)
  in_data <- in_data[dat_inds,]
  return(in_data)
}

clean_codes_inPerPtsData <- function(in_data, all_code_cols, ICD_cols,HCPCS_cols){
  # in_data <- medicaid_health_df
  # ICD_cols <- c(ICD_diag_cols1)
  # HCPCS_cols <- c(HCPCS_proc_cols1)
  # all_code_cols <- c(ICD_diag_cols1,HCPCS_proc_cols1)
  if (is.null(in_data) == T){
    in_data <- NULL
  }else{
    for (t in 1:length(all_code_cols)){
      curr_col <- all_code_cols[t]
      curr_codes_list <- in_data[,curr_col]
      
      if (curr_col %in% ICD_cols){
        curr_codes_type_list <- rep("ICD",length(curr_codes_list))
      }else if (curr_col %in% HCPCS_cols){
        curr_codes_type_list <- rep("HCPC",length(curr_codes_list))
      }else{
        curr_codes_type_list <- rep("Drug",length(curr_codes_list))
      }
      in_data[,curr_col] <- clean_code_func2(curr_codes_list,curr_codes_type_list)
      
    }
  }
  return(in_data)
}


get_uniquecodes_perMonth <- function(code_type, in_data1,in_data2,code_cols1,code_cols2){
  codes1 <- as.vector(unlist(in_data1[,code_cols1]))
  codes2 <- as.vector(unlist(in_data2[,code_cols2]))      
  all_codes <- unique(c(codes1,codes2))
  
  #remove NA or balnks
  na_orBlanks <- which(is.na(all_codes)==T | all_codes == "")
  if (length(na_orBlanks) != 0){
    all_codes <- all_codes[-na_orBlanks]
  }
  
  #Add type info
  if (length(all_codes) > 0){
    all_codes <- paste0(code_type,"_",all_codes)
  }else{
    all_codes <- NULL
  }
  
  return(all_codes)
}


#4_XXXXXXX.R codes functions
get_primary_site_date_func <- function(pt_kcr_df,CentralSequence_N) {
  pri_idx <- which(pt_kcr_df[,"CentralSequenceNumber"] %in% CentralSequence_N) #primary index
  if (length(pri_idx) > 0 ){  #if has primary idxes, it coulde be more than one
    pri_dates <- paste0(pt_kcr_df[pri_idx,"Date_dx"],collapse = "$$$")
    pri_sites <- paste0(pt_kcr_df[pri_idx,"PrimarySite"],collapse = "$$$")
    if (identical(CentralSequence_N,c(0,1)) == T){ #if it is 1st primary, get recurrence date if there is recurrece
      recur_date <- pt_kcr_df[pri_idx,"Date_1Recur"]
    }else{
      recur_date <- NA
    }
  }else{
    pri_dates <- NA
    pri_sites <- NA
    recur_date <- NA
  }
  
  return(list(pri_sites,pri_dates,recur_date))
}


get_cancer_info_func <- function(in_data, type_name){
  idx  <- which(grepl(type_name,in_data[,"Type"]) == T) #use grepl, due to merged process, as long as it contains First primary, we consider it, just ignore the other ones
  site <- in_data[idx,"Site"]
  type <- in_data[idx,"Type"]
  date <- in_data[idx,"Date"]
  return(list(idx,site,type,date))
}



#5_XXXXXX.R codes functions
#1.Convert column integer names to acutal date
convert_intCol_toDate <- function(enroll_df,month_col_indexes, min_date,max_date, date_unit){
  dates_seq <- seq(min_date,max_date,by = date_unit)
  colnames(enroll_df)[month_col_indexes] <- as.character(dates_seq)
  return(enroll_df)
}


##10_XXXXX.R functions
get_codes_func <- function(codes_colnames,code_type){
  colnames_indata <- codes_colnames[which(grepl(code_type,codes_colnames)==T)]
  codes   <- gsub(paste0(code_type,"_"),"",colnames_indata)
  if (length(colnames_indata) == 0){
    code_df <- NULL
  }else{
    code_df <- data.frame(COLNAMES = colnames_indata, CODE = codes,TYPE = code_type)
  }
  return(code_df)
}

find_individual_code_grp_func <- function(code ,codetype, grp_df,grptype){
  idx <- which(grp_df[,"CODE"] == code & grp_df[,"TYPE"] == codetype)
  if(length(idx) > 0){
    grp <- grp_df[idx,grptype]
  }else{
    grp <- paste0(grptype, "_" , NA) #if code cannot be found
  }
  return(grp)
}



find_listofcode_grp_func <- function(code_df,grp_type,grp_df){
  GRPs <- NA
  for (i in 1:nrow(code_df)){
    curr_code     <- as.character(code_df[i, "CODE"])
    curr_codetype <- as.character(code_df[i, "TYPE"])
    curr_grp <- find_individual_code_grp_func(curr_code,curr_codetype,grp_df,grp_type)
    GRPs[i] <- curr_grp
    
  }
  return(GRPs)
}


#create a group feature df
create_grp_feature_df_func <- function(perMonth_df,unique_grps, unique_codes_df){
  grp_feature_df <- perMonth_df[,1:4] #keep id and month
  grp_feature_df[,unique_grps] <- NA #new grp feature cols
  
  for (j in 5:ncol(grp_feature_df)){
    curr_grp             <- colnames(grp_feature_df)[j]
    curr_codes_ingrp     <- as.character(unique_codes_df[which(unique_codes_df[,"GRPS"] == curr_grp),"COLNAMES"])
    curr_col_idx_ingrps  <- which(colnames(perMonth_df) %in% curr_codes_ingrp)
    
    curr_df <- as.data.frame(perMonth_df[,curr_col_idx_ingrps])
    grp_feature_df[,j] <- rowSums(curr_df,na.rm = T)
  }
  
  return(grp_feature_df)
}


get_pts_list_ofgrps <- function(dat_dir,ID_list,code_grp_type){
  
  Unique_Grp_list <- list(NA)
  for (i in 1:length(ID_list)){
    if(i %% 1000 == 0){print(i)}
    curr_id <- ID_list[i]
    curr_file <- paste0("ID",curr_id, paste0("_Month_",code_grp_type,"_UniqueGrps.xlsx"))
    if (file.exists(paste0(dat_dir,curr_file)) == T){
      curr_df <- read.xlsx(paste0(dat_dir,curr_file),sheet = 1)
    }else{
      curr_df <- NULL
    }
    
    Unique_Grp_list[[i]] <- curr_df[,"unique_grps"]
  }
  return(Unique_Grp_list)
}

#count num and fraction of patient for each group feature
count_numPts_forGrp<- function(grp_list,unique_grps){
  total_num_pts <- length(grp_list)
  
  print(paste("Total Unique Grps:",length(unique_grps)))
  print(paste("Total Pts:",total_num_pts))
  
  count_df <- as.data.frame(matrix(NA, nrow = length(unique_grps), ncol = 3))
  colnames(count_df) <- c("Code_Grp","Num_PtsHasTheGrp","Frac_PtsHasTheGrp")
  for (i in 1:length(unique_grps)){
    if(i %% 50 == 0){print(i)}
    curr_grp <- unique_grps[i]
    
    #Find the index of list which contians current group
    pts_indexes <- which(sapply(grp_list, FUN=function(X) curr_grp %in% X))
    
    count_df[i,"Code_Grp"] <- curr_grp
    count_df[i,"Num_PtsHasTheGrp"]  <- length(pts_indexes)
    count_df[i,"Frac_PtsHasTheGrp"] <- length(pts_indexes) /total_num_pts
  }
  return(count_df)
}

#This function get count table for one code grp type
get_count_table_func <- function(data_dir,code_grp_type,SBCE_PTs,nonSBCE_PTs){
  #For SBCE pts
  grp_list_SBCE    <- get_pts_list_ofgrps(data_dir,SBCE_PTs,code_grp_type)
  
  #For nonSBCE pts
  grp_list_nonSBCE <- get_pts_list_ofgrps(data_dir,nonSBCE_PTs,code_grp_type)
  
  #From all pts, get unique grps
  all_unique_grps <- unique(c(unlist(grp_list_SBCE),unlist(grp_list_nonSBCE)))
  all_unique_grps  <- all_unique_grps[-which(all_unique_grps == "NONE")] #this is due to patient might not have any code of this type
  
  ################################################################################
  #For each unique grps feature in all data, count the number of pts who has it
  ################################################################################
  count_df_SBCE              <- count_numPts_forGrp(grp_list_SBCE,all_unique_grps)
  colnames(count_df_SBCE)    <- paste0(colnames(count_df_SBCE), "_SBCE")
  
  count_df_nonSBCE           <- count_numPts_forGrp(grp_list_nonSBCE,all_unique_grps)
  colnames(count_df_nonSBCE) <- paste0(colnames(count_df_nonSBCE),"_nonSBCE")
  
  comb_count_df <- cbind(count_df_SBCE,count_df_nonSBCE)
  
  colnames(comb_count_df)[1] <- "Code_Grp"
  comb_count_df <- comb_count_df[,-4] #remove duplicated group name col
  
  return(comb_count_df)
}

#This function add group discrption
add_grp_discrption_func <- function(analysis_count_tb,disrip_df,grp_col,grp_disrip_col){
  analysis_count_tb[,"Grp_Discrip"] <- NA
  for (i in 1:nrow(analysis_count_tb)){
    curr_grp <- analysis_count_tb[i,1]
    curr_grp <- gsub("CCS_DIAG_|CCS_PROC_|DM3_SPE_|DM3_GEN_|S_GNN_","",curr_grp)
    
    curr_idxes  <-  which(disrip_df[,grp_col] == curr_grp)
    curr_discrp <-  unique(disrip_df[curr_idxes,grp_disrip_col])
    
    if (length(curr_discrp) == 0 ){
      curr_discrp <- NA
    }else if (is.na(curr_discrp) == T){
      curr_discrp <- NA
    }else{
      longer_disp_idxes <- which(nchar(curr_discrp) == max(nchar(curr_discrp),na.rm = T))[1] ##use the longer name to get the longer name if multiple
      curr_discrp <- curr_discrp[longer_disp_idxes]
    }
    analysis_count_tb[i,"Grp_Discrip"] <- curr_discrp
  }
  return(analysis_count_tb)
}

#This func used in 18_CheckPTSFeatureTrajectory.R and 20_Importance Plot.R
find_ccs_discrption_func <-function(grp_df,curr_code){
  #grp_df <- Proc_grp
  #curr_code <- curr_ccs_code
  
  grp_idxes <- which(grp_df[,"CCS_CATEGORY"] == curr_code)
  unique_discrptions <- unique(grp_df[grp_idxes,"CCS_CATEGORY_DESCRIPTION"])
 
  unique_discrptions <-  gsub('[[:punct:] ]+',' ',unique_discrptions) 
  unique_discrptions <-  trimws(unique_discrptions) #trim white space and '
  
  n_char_discrip <- nchar(unique_discrptions)
  final_discrip <- unique_discrptions[which(n_char_discrip == max(n_char_discrip))]
  
  final_discrip <- gsub('[[:punct:] ]+',' ',final_discrip)
  final_discrip <- trimws(final_discrip) #trim white space and '
   
  return(final_discrip)
}

extract_ccs_typeAndcode <- function(feature_list){
  #feature_list <- important_f_df[,"Feature"]
  
  feature_list <- gsub("time_since_|time_until_|cumul_ratio_","",feature_list)
  
  ccs_type <- NA
  ccs_code <- NA
  for (i in 1:length(feature_list)){
    curr_f <- feature_list[i]
    
    if (grepl("CCS_",curr_f)==T){ #if CCS feature
      curr_f_splited <- strsplit(curr_f,split = "_")
      ccs_type[i] <- unlist(curr_f_splited)[[2]]
      ccs_code[i] <- unlist(curr_f_splited)[[3]]
    }else{
      ccs_type[i] <- NA
      ccs_code[i] <- NA
    }
  }
  
  return(list(ccs_type,ccs_code))
}

