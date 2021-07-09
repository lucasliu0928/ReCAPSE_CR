library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(foreach)
library(doParallel)

split_code_strings <- function(code_df, code_col){
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

split_andcombine_codes <- function(code_df,code_col){
  #This function split concatenated code strings in a data column into individual code, 
  #then concatednete all codes in a column into one string
   
  unique_splited_codes <- split_code_strings(code_df,code_col)

  if (is.null(unique_splited_codes) == F){
      combined_codes <- paste0(unique_splited_codes,collapse = "$$$$")
  }else{
      combined_codes <- NA
  }
  # curr_codes <- code_df[,code_col]
  # #remove NAs
  # na_idxes <- which(is.na(curr_codes)==T)
  # if (length(na_idxes) > 0 ){
  #   curr_codes <- curr_codes[-na_idxes]
  # }
  # 
  # 
  # if (length(curr_codes) >0){ #if remaining code length > 0
  #   #split to remove duplciated from each day
  #   curr_unique_splited_codes <- unique(unlist(strsplit(curr_codes,split= "$$$$",fixed = T)))
  #   curr_combined_codes <- paste0(curr_unique_splited_codes,collapse = "$$$$")
  # }else{
  #   curr_combined_codes <- NA
  # }
  
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