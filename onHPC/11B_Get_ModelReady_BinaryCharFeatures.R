source("Recapse_Ultility.R")
library(fastDummies)
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
data_dir1  <- paste0(proj_dir, "8_Characteristics2/Month_Level/MonthChar_WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "11B_ModelReady_CharFatures/WithPossibleMonthsHasNoCodes/")


################################################################################
#1.get original char files
################################################################################
perMonth_char_files <- list.files(data_dir1)
perMonth_char_IDs <- gsub("_MonthChar.xlsx|ID","",perMonth_char_files)

################################################################################
#2.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir2,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

################################################################################
#.Only select files for analysis IDs
################################################################################
perMonth_char_files <- perMonth_char_files[which(perMonth_char_IDs %in% analysis_IDs)]

########################################################################################################################
#1.Combine all pts binary char
#2. Select features
#3. Recode feature to binary columns 
#'@Updated 10/05 added surg_prim_site_V1
#'@Updated 10/05 added enrolled year
##'@updated 11/11: added "DAJCC_T" ,"DAJCC_M","DAJCC_N" 
##''@TODO ADD num_claims later
########################################################################################################################
selected_charfeatures <- c("Enrolled_year","Age","months_since_dx","Race" , "Site" , "Stage","Grade",
                           "Laterality" ,"er_stat","pr_stat",	"her2_stat",	
                           "surg_prim_site_V1",
                           "DAJCC_T" ,"DAJCC_M","DAJCC_N",
                           "reg_age_at_dx",	"reg_nodes_exam", 
                           "reg_nodes_pos",	"cs_tum_size", "cs_tum_ext", 
                           "cs_tum_nodes", "regional")
outcome_label <- "y_PRE_OR_POST_2ndEvent"
other_cols <- c("study_id","Month_Start")

col_toconvert <- c("Race","Site","Stage","Grade","Laterality","er_stat","pr_stat","her2_stat",
                   "surg_prim_site_V1") #note regional does not need to be convert cuz it is 0 or 1

#1.Combine all pts df
#all_char_df_list <- lapply(paste0(data_dir1,perMonth_char_files), read.xlsx)
all_char_df_list <- mclapply(paste0(data_dir1,perMonth_char_files), mc.cores= numCores, function(z){read.xlsx(z, sheet = 1)})
all_char_df <- do.call(rbind,all_char_df_list)


#2. Select features
updated_all_char_df <- all_char_df[,c(other_cols,selected_charfeatures,outcome_label)]

#3.covert to binary
all_binary_char_df <- dummy_cols(updated_all_char_df, remove_first_dummy = FALSE,select_columns = col_toconvert)

#remove original columns
all_binary_char_df <- all_binary_char_df[, -which(colnames(all_binary_char_df) %in% col_toconvert)]

#Save it as csv instead of xlsx cuz there is an error for xlsx when open the file due to large number of rows
write.csv(all_binary_char_df,paste0(outdir,"All_Binary_Chars_WithSurgPrimSite_V1.csv"),row.names = F)



########################################################################################################################
#1.Combine all pts binary char
#2. Select features
#3. Recode feature to binary columns 
#'@Updated 10/05 added surg_prim_site_V2
#'@Updated 10/05 added enrolled year
#'@updated 11/11 and 12/01: added and updated "DAJCC_T" ,"DAJCC_M","DAJCC_N" 
#'@TODO ADD num_claims later
########################################################################################################################
selected_charfeatures <- c("Enrolled_year","Age","months_since_dx","Race" , "Site" , "Stage","Grade",
                           "Laterality" ,"er_stat","pr_stat",	"her2_stat",	
                           "surg_prim_site_V2",
                           "DAJCC_T" ,"DAJCC_M","DAJCC_N",
                           "reg_age_at_dx",	"reg_nodes_exam", 
                           "reg_nodes_pos",	"cs_tum_size", "cs_tum_ext", 
                           "cs_tum_nodes", "regional")
outcome_label <- "y_PRE_OR_POST_2ndEvent"
other_cols <- c("study_id","Month_Start")

col_toconvert <- c("Race","Site","Stage","Grade","Laterality","er_stat","pr_stat","her2_stat",
                   "surg_prim_site_V2") #note regional does not need to be convert cuz it is 0 or 1

#1.Combine all pts df
#all_char_df_list <- lapply(paste0(data_dir1,perMonth_char_files), read.xlsx)
all_char_df_list <- mclapply(paste0(data_dir1,perMonth_char_files), mc.cores= numCores, function(z){read.xlsx(z, sheet = 1)})
all_char_df <- do.call(rbind,all_char_df_list)


#2. Select features
updated_all_char_df <- all_char_df[,c(other_cols,selected_charfeatures,outcome_label)]

#3.covert to binary
all_binary_char_df <- dummy_cols(updated_all_char_df, remove_first_dummy = FALSE,select_columns = col_toconvert)

#remove original columns
all_binary_char_df <- all_binary_char_df[, -which(colnames(all_binary_char_df) %in% col_toconvert)]

#Save it as csv instead of xlsx cuz there is an error for xlsx when open the file due to large number of rows
write.csv(all_binary_char_df,paste0(outdir,"All_Binary_Chars_WithSurgPrimSite_V2.csv"),row.names = F)



