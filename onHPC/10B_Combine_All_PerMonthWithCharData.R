library(openxlsx)

#onHPC
data_dir <- "/recapse/intermediate_data/10_perMonthData_withChar_V2_nonuniquecodes/"
outdir <- "/recapse/intermediate_data/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/10_perMonthData_withChar_V2_nonuniquecodes/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

################################################################################ 
#1. Load Anlaysis ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(outdir,"9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- unique(FinalID_df$study_id)

################################################################################ 
#1. Combine all per month with char data into one dataframe
################################################################################ 
All_Data_list <- list(NA)
for (i in 1:length(Final_IDs)){  #length(Final_IDs)
  curr_id <- Final_IDs[i]
  curr_perMonth_file <- paste0(data_dir,"ID",curr_id,"_PerMonthData_WithMonthChar_df.xlsx")
  
  if (file.exists(curr_perMonth_file) == T){
    curr_df <- read.xlsx(curr_perMonth_file,sheet = 1)
  }else{
    curr_df <- NULL
  }
  
  All_Data_list[[i]] <-curr_df
}

All_Data_df <- do.call(rbind,All_Data_list)

write.csv(All_Data_df,paste0(outdir,"10_All_PerMonthData_WithMonthChar_df.csv"),row.names = F)
