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


######################################################################
#3_HPC_Get....R Functions:
clean_code_func <-function(list_of_codes){
  #list_of_codes <- HCUP_comb[,"Code"]
  
  #1.omitting any codes with non-alphanumeric characters,
  updated_list_of_codes<- gsub("[^[:alnum:]]", " ", list_of_codes)
  
  #2. space
  updated_list_of_codes <- trimws(updated_list_of_codes, which = c("both"), whitespace = "[ \t\r\n]")
  
  #3.decimal
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
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
        updated_code[c] <- paste0("0",cur_code)
      }
    }
    
    updated_list_of_codes[l3_idxes] <- updated_code
    
    #This might result in NAs(from converting to numeric when it is char) from orignal 
  }
  
  return(updated_list_of_codes)
}

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


####Grouping functions
load_and_clean_CSS_data<- function(file_dir){
  #Load four tables
  HCUP_Diag1_df <- read.csv(paste0(file_dir,"Code_Groups/HCUP_CCS_tables/CCS.ICD-9.diag_ref.edit.csv"),stringsAsFactors = F)
  HCUP_Diag2_df <- read.csv(paste0(file_dir,"Code_Groups/HCUP_CCS_tables/CCS.ICD-10.diag_ref.edit.csv"),stringsAsFactors = F)
  HCUP_Proc1_df <- read.csv(paste0(file_dir,"Code_Groups/HCUP_CCS_tables/CCS.ICD-9.proc_ref.edit.csv"),stringsAsFactors = F)
  HCUP_Proc2_df <- read.csv(paste0(file_dir,"Code_Groups/HCUP_CCS_tables/CCS.ICD-10.proc_ref.edit.csv"),stringsAsFactors = F)
  
  #Change col name to comb
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
  HCUP_comb[,"Code"]  <- clean_code_func(HCUP_comb[,"Code"])
  HCUP_comb[,"CCS.CATEGORY"]  <- clean_code_func(HCUP_comb[,"CCS.CATEGORY"]) #Clean category in HCUP
  
  
  #4. Remove all blanks and NAs   
  HCUP_comb <- remove_NA_from_df(HCUP_comb,"Code")
  
  #5. remove the unspecified codes, cuz they will result codes grped into multiple groups, (e.g, E8342 is a qulified codes in ICD10, but unspecified in ICD9)
  unspecified_idxes <- which(grepl("\\be codes|\\bExternal cause codes",HCUP_comb[,"CCS.CATEGORY.DESCRIPTION"],ignore.case = T)==T)
  HCUP_comb<- HCUP_comb[-unspecified_idxes,]
  return(HCUP_comb)
}

group_codes_into_CCS_func <- function(claim_code_df,CCS_df){
  claim_code_df$CCS_CATEGORY <- NA
  claim_code_df$CCS_CATEGORY_DESCRIPTION <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,1]
    curr_ccs_idxes <- which(CCS_df[,"Code"] == curr_code)
    if (length(curr_ccs_idxes) > 0){
      claim_code_df[i,"CCS_CATEGORY"] <- CCS_df[curr_ccs_idxes,"CCS.CATEGORY"]
      claim_code_df[i,"CCS_CATEGORY_DESCRIPTION"] <- CCS_df[curr_ccs_idxes,"CCS.CATEGORY.DESCRIPTION"]
    }
  }
  return(claim_code_df)
}

#Chuback
load_and_clean_Chuback_data<- function(file_dir){
  #Load four tables
  chuback_group_df <- read.csv(paste0(file_dir,"Code_Groups/BRAVA_lookup.20180502.edit.csv"),stringsAsFactors = F)
  length(unique(chuback_group_df$Type)) #216
  length(unique(chuback_group_df$Category)) #22
  
  #3.clean code 
  chuback_group_df[,"Code"]<- clean_code_func(chuback_group_df[,"Code"])
  
  #4. Remove all blanks and NAs   
  chuback_group_df <- remove_NA_from_df(chuback_group_df,"Code")
  
  return(chuback_group_df)
}
group_codes_into_Chuback_func <- function(claim_code_df,Chuback_grp_df){
  claim_code_df[,"Chuback_Type"] <- NA
  claim_code_df[,"Chuback_Category"] <- NA
  claim_code_df[,"Chuback_Description"] <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,1]
    curr_idxes <- which(Chuback_grp_df[,"Code"] == curr_code)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"Chuback_Type"]     <- paste0(unique(Chuback_grp_df[curr_idxes,"Type"]),collapse = "$$$$")
      claim_code_df[i,"Chuback_Category"] <- paste0(unique(Chuback_grp_df[curr_idxes,"Category"]),collapse = "$$$$")
      
      claim_code_df[i,"Chuback_Description"] <-  paste0(unique(Chuback_grp_df[curr_idxes,"Code.Description"]),collapse = "$$$$")
      
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
  Ritzwoller_group_df[,"Code"] <- clean_code_func(Ritzwoller_group_df[,"Code"])
  
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

group_drugcodes_into_DM3_func <- function(claim_code_df,DM3_df){
  claim_code_df$specific_group <- NA
  claim_code_df$general_group <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_code <- claim_code_df[i,"Drug_name"]
    curr_idxes <- which(DM3_df[,"desc"] == curr_code)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"specific_group"] <-  unique(DM3_df[curr_idxes,"specific_group"])
      claim_code_df[i,"general_group"]  <-  unique(DM3_df[curr_idxes,"general_group"])
    }
  }
  return(claim_code_df)
}

get_ccs_discription <- function(freq_tb,CCS_Diag_df,CCS_Proc_df){
  #for diag
  ccs_d_idxes <- which(grepl("CCS_D",freq_tb$Code_Group) == T)
  
  for (i in 1:length(ccs_d_idxes)){
    curr_ind <- ccs_d_idxes[i]
    curr_grp <- gsub("CCS_D_","",freq_tb[curr_ind,"Code_Group"])
    freq_tb[curr_ind,"DESCRIPTION"] <- unique(CCS_Diag_df[which(CCS_Diag_df[,"CCS.CATEGORY"] == curr_grp),"CCS.CATEGORY.DESCRIPTION"])[1]
  }
  
  #for proc
  ccs_p_idxes <- which(grepl("CCS_P",freq_tb$Code_Group) == T)
  
  for (i in 1:length(ccs_p_idxes)){
    curr_ind <- ccs_p_idxes[i]
    curr_grp <- gsub("CCS_P_","",freq_tb[curr_ind,"Code_Group"])
    freq_tb[curr_ind,"DESCRIPTION"] <- unique(CCS_Proc_df[which(CCS_Proc_df[,"CCS.CATEGORY"] == curr_grp),"CCS.CATEGORY.DESCRIPTION"])[1]
  }
  return(freq_tb)
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
  
  #pt_perMonth_df <- curr_CCS_counting_df_diag
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  pt_perMonth_df$Month_Index <- seq(1,nrow(pt_perMonth_df),1) #Use interger as month sequence for easier computation
  pt_perMonth_df <- pt_perMonth_df[,c(1,2,ncol(pt_perMonth_df),3:(ncol(pt_perMonth_df)-1))] #    #reorder columns
  
  time_since_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(time_since_df) <- colnames(pt_perMonth_df)
  colnames(time_since_df)[4:ncol(time_since_df)] <- paste0("time_since_",colnames(time_since_df)[4:ncol(time_since_df)])
  
  #code group col started at index 4
  for(j in 4:ncol(time_since_df)){ #for each code group 
    most_recent_month <- Inf #initial a most recent month as Future for each code group
    
    for (i in 1:nrow(pt_perMonth_df)){ #for each month
      curr_month_df <- pt_perMonth_df[i,]
      time_since_df[i,"study_id"] <- curr_month_df[,"study_id"]
      time_since_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
      time_since_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
      
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
  
  time_since_df[which(time_since_df== "-Inf",arr.ind = T)] <- -1 #recode -INF to -1 for months that has never seen a code
  return(time_since_df)
}


add_time_until_func <-function(pt_perMonth_df){
  #the time since the soonest future occurrence of this code group
  #pt_perMonth_df <- curr_CCS_counting_df_diag
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  pt_perMonth_df$Month_Index <- seq(1,nrow(pt_perMonth_df),1) #Use interger as month sequence for easier computation
  pt_perMonth_df <- pt_perMonth_df[,c(1,2,ncol(pt_perMonth_df),3:(ncol(pt_perMonth_df)-1))] #    #reorder columns
  
  time_until_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(time_until_df) <- colnames(pt_perMonth_df)
  colnames(time_until_df)[4:ncol(time_until_df)] <- paste0("time_until_",colnames(time_until_df)[4:ncol(time_until_df)])
  
  #code group col started at index 4
  for(j in 4:ncol(time_until_df)){ #for each code group 
    soonest_future_month <- -Inf #initial a soonest future month as past(-INF) for each code group
    
    for (i in nrow(pt_perMonth_df):1){ #for each month from latest to oldest
      curr_month_df <- pt_perMonth_df[i,]
      time_until_df[i,"study_id"] <- curr_month_df[,"study_id"]
      time_until_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
      time_until_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
      
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
  
  time_until_df[which(time_until_df== "-Inf",arr.ind = T)] <- -1 #recode -INF to -1 for months that has never seen a code
  return(time_until_df)
}


add_cumul_ratio_func <-function(pt_perMonth_df){
  #the total number of occurrences in each patient up to the time in question of that grouping divided by total elapsed time
  #pt_perMonth_df <- curr_CCS_counting_df_diag
  
  #reoder and add month sqeuence
  pt_perMonth_df <- pt_perMonth_df[order(ymd(pt_perMonth_df[,"Month_Start"])),]  #sort pt data by month
  pt_perMonth_df$Month_Index <- seq(1,nrow(pt_perMonth_df),1) #Use interger as month sequence for easier computation
  pt_perMonth_df <- pt_perMonth_df[,c(1,2,ncol(pt_perMonth_df),3:(ncol(pt_perMonth_df)-1))] #    #reorder columns
  
  cumul_ratio_df <-as.data.frame(matrix(NA, nrow = nrow(pt_perMonth_df),ncol =ncol(pt_perMonth_df)))
  colnames(cumul_ratio_df) <- colnames(pt_perMonth_df)
  colnames(cumul_ratio_df)[4:ncol(cumul_ratio_df)] <- paste0("cumul_ratio_",colnames(cumul_ratio_df)[4:ncol(cumul_ratio_df)])
  
  #code group col started at index 4
  for(j in 4:ncol(cumul_ratio_df)){ #for each code group 
    curr_cum_count <- 0 #inital cumalitive count as 0 
    for (i in 1:nrow(pt_perMonth_df)){ #for each month
      #get month data and assign to the new dataframe
      curr_month_df <- pt_perMonth_df[i,]
      cumul_ratio_df[i,"study_id"] <- curr_month_df[,"study_id"]
      cumul_ratio_df[i,"Month_Start"] <- curr_month_df[,"Month_Start"]
      cumul_ratio_df[i,"Month_Index"] <- curr_month_df[,"Month_Index"]
      
      #get count and month index
      curr_count <- curr_month_df[,j]
      curr_month <- curr_month_df[,"Month_Index"]
      
      #cumalative count 
      curr_cum_count <- curr_cum_count + curr_count
      cumul_ratio_df[i,j] <- round(curr_cum_count/curr_month,4)
    }
  }
  
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
