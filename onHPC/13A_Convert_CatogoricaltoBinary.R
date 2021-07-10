source("Recapse_Ultility.R")
library(fastDummies)

#onHPC
grp_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# # #local
# grp_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"



################################################################################ 
#1. Load all per month with char data
################################################################################ 
All_data <- read.csv(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.csv"), stringsAsFactors = F)
missing_table <- get_missing_rate_table(All_data,colnames(All_data))
table(All_data$y_PRE_OR_POST_2ndEvent) #661098,  1 :31563 

#remove missing > 70%
All_data <- All_data[, -which( colnames(All_data) %in% c("DAJCC_M","cs_tum_size","cs_tum_ext","cs_tum_nodes"))]

#remove redudant
All_data <- All_data[, -which( colnames(All_data) %in% c("months_to_second_event","Drug_Codes","Proc_Codes","Diag_Codes","Month_Start"))]
missing_table <- get_missing_rate_table(All_data,colnames(All_data))


################################################################################ 
#Recode feature to binary columns 
################################################################################
col_toconvert <- c("Race","Grade","Laterality","er_stat","pr_stat","her2_stat",
                   "surg_prim_site" ,"DAJCC_T" ,"DAJCC_N")
All_data_withBinary_Char <- dummy_cols(All_data, remove_first_dummy = FALSE,select_columns = col_toconvert)

#remove orignal column
All_data_withBinary_Char <- All_data_withBinary_Char[, -which( colnames(All_data_withBinary_Char) %in% col_toconvert)]
write.csv(All_data_withBinary_Char,paste0(outdir,"13_All_data_withBinary_Char.csv"),row.names = F)


