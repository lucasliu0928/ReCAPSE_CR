source("Recapse_Ultility.R")

#onHPC
project_dir            <- "/recapse/intermediate_data/"
outdir                  <- project_dir

#local
project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
outdir                  <-project_dir



################################################################################ 
#1. Load Finaly ID
################################################################################ 
FinalID_df <- read.xlsx(paste0(project_dir,"/9_Final_Analysis_ID.xlsx"),sheet = 1)
Final_IDs <- FinalID_df$study_id

################################################################################ 
#2. Load patient level char 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(project_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% Final_IDs),] #only keep char for test ID

################################################################################ 
#3. Report some stats
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
n_sbce0   <- length(sbce_pt_Ids)
n_sbce1 <- length(nosbce_pt_Ids)

perc_sbce0 <- round(n_sbce0/length(Final_IDs)*100,2)

#Number of patients with first primary recurrence
table(pts_level_char_df$Type_2nd_Event)
table(pts_level_char_df$First_Primary_BC_related_Death)

missingtable <- get_missing_rate_table(pts_level_char_df, colnames(pts_level_char_df))

write.csv(missingtable, paste0("/Users/lucasliu/Desktop/","missingtable.csv"))

table(pts_level_char_df$DAJCC_M)
table(pts_level_char_df$DAJCC_N)
table(pts_level_char_df$DAJCC_T)
