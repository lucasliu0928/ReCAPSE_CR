library(openxlsx)
library(lubridate)

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

#onHPC
perday_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
perMonth_dir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"
outdir <- "/recapse/intermediate_data/"

#local
perday_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
perMonth_dir <- "/Users/lucasliu/Desktop/intermediate_data/6_perMonthData_inValidMonth_perPatientData/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

################################################################################ 
#4. Compute missing
################################################################################ 
updated_Patient_Char_df <- Patient_Char_df[which(Patient_Char_df$study_id %in% Final_IDs),]
missing_table <- get_missing_rate_table(updated_Patient_Char_df,colnames(updated_Patient_Char_df))



# ################################################################################ 
# #5. Combine code sand char
# ################################################################################ 
# comb_df <- cbind(All_perMonthData_df,month_level_char_df[,-1])
# 
# write.xlsx(comb_df,paste0(outdir,"10_PerMonthData_WithMonthChar_df.xlsx"))
# 
