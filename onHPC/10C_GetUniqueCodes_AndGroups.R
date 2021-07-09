source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

#local
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


################################################################################ 
#1. Load all per month with char data
################################################################################ 
All_data <- read.xlsx(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.xlsx"))


################################################################################ 
##2. Unique Codes in entire data
################################################################################ 
check_data <- All_data[1:3,]

split_code_strings(check_data,"Diag_Codes")

split_andcombine_codes(check_data,"Diag_Codes")
strsplit(All_data[,"Diag_Codes"])

# ################################################################################ 
# #4. Compute missing
# ################################################################################ 
# updated_Patient_Char_df <- Patient_Char_df[which(Patient_Char_df$study_id %in% Final_IDs),]
# missing_table <- get_missing_rate_table(updated_Patient_Char_df,colnames(updated_Patient_Char_df))



# ################################################################################ 
# #5. Combine code sand char
# ################################################################################ 
# comb_df <- cbind(All_perMonthData_df,month_level_char_df[,-1])
# 
# write.xlsx(comb_df,paste0(outdir,"10_PerMonthData_WithMonthChar_df.xlsx"))
# 
