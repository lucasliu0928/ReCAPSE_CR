source("Recapse_Ultility.R")
################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
data_dir2        <- paste0(proj_dir, "12B_Boxplot/WithPossibleMonthsHasNoCodes/")
outdir           <- paste0(proj_dir, "12C_TrainTestIDs/")

################################################################################ 
#1. Load analysis ID
################################################################################ 
Analysis_df <- read.xlsx(paste0(data_dir1,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_ID    <- unique(Analysis_df$study_id) #18239

################################################################################ 
#2.Load obvious Pre IDs to remove
################################################################################ 
obs_Ids_df <- read.csv(paste0(data_dir2,"Obs_Pre_IDs.csv"),stringsAsFactors = F)
obs_Ids <- gsub("ID","",obs_Ids_df$Obs_Pre_study_id)

################################################################################ 
#3.Update Final ID by removing obs IDs
################################################################################ 
Final_ID <- analysis_ID[-which(analysis_ID %in% obs_Ids)]


################################################################################ 
#4. Pts char
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir1,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% Final_ID),] #only keep char for final ID
print("Original non-SBCE vs SBCE : ")
table(pts_level_char_df$SBCE) #10680   1046

################################################################################ 
#2. Get SBCE and non-SBCE IDs
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids)) #10: 1

######################################################################################################## 
#3. make sure no overlapping in original Ids in train and test
#Without sampling
#Test: 20% of the original IDs
#Train: 80% of the orginal IDs
#Validation sets will be auto generated when doing CV
########################################################################################################
total_n <- length(Final_ID)
set.seed(123)
test_ID   <- sample(Final_ID,0.2*total_n)
train_ID  <- Final_ID[-which(Final_ID %in% test_ID)]

test_ID_withLabel_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% test_ID),c("study_id" , "SBCE")]
train_ID_withLabel_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% train_ID),c("study_id" , "SBCE")]

write.xlsx(test_ID_withLabel_df, paste0(outdir,"test_ID_withLabel.xlsx"))
write.xlsx(train_ID_withLabel_df,paste0(outdir,"train_ID_withLabel.xlsx"))

