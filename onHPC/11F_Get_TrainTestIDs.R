source("Recapse_Ultility.R")
################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


#'@NOTE: 
#'SBCE: consider there types of events
#'SBCE_Excluded_DeathLabel: treat death label as 0
#'SBCE_Excluded_DeathPts: exclude pts with sole death event 

feature_set_name <- "CCSandDM3SPE"    #choose from CCSandDM3SPE , CCSandVAL2nd
sample_name <- "Samples_HasAtLeastOneCodeGrpFeature"           #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"
SBCE_col <- "SBCE_Excluded_DeathPts"  #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts

#data dir
data_dir1        <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
data_dir2        <- paste0(proj_dir,"11E_AllPTs_ModelReadyData/",feature_set_name,"/", sample_name, "/")

newout <- paste0("11F_TrainTestIDs/",feature_set_name, "/",sample_name, "/",SBCE_col,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

################################################################################
#1. Load Model ready data to get study IDs
#All Samples and Samples_HasAtLeastOneCodeGrpFeature has different number of study IDs
################################################################################
load(file = paste0(data_dir2, "All_PTS_ModelReadyData.rda")) 
if (grepl("Samples_HasAtLeastOneCodeGrpFeature",data_dir2) == T){
  analysis_id <- unique(model_data_excluded$study_id)
}else{
  analysis_id <- unique(model_data$study_id)
}
analysis_id <- as.numeric(gsub("ID","",analysis_id))
  
################################################################################ 
#4. Pts char, keep data for analysis_id
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir1,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1,sep.names = " ")
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df[,"study_id"] %in% analysis_id),]

#'@NOTE: Some patient who died and also get other 2nd event(1st recur), ttey are still kept,
#'Only exclude patient who has sole event = death
if (SBCE_col == "SBCE_Excluded_DeathPts"){
  idxes <- which(pts_level_char_df[,"Type_2nd_Event"] == "Primary1stBC_related_Death") 
  pts_level_char_df <- pts_level_char_df[-idxes,]
  SBCE_col <- "SBCE" #Still use the same label column as SBCE
}

print("Original non-SBCE vs SBCE : ")
table(pts_level_char_df[,SBCE_col]) #16917  1322 or 17054  1185  or 16917  1185

Final_ID <- unique(pts_level_char_df[,"study_id"])
################################################################################ 
#2. Get SBCE and non-SBCE IDs
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df[,SBCE_col] == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df[,SBCE_col] == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids)) #10: 1
print("Original non-SBCE vs SBCE Ratio: ")
print(original_noSBCE_toSBCEratio)

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

test_ID_withLabel_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% test_ID),c("study_id" , SBCE_col)]
train_ID_withLabel_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% train_ID),c("study_id" ,SBCE_col)]

write.xlsx(test_ID_withLabel_df, paste0(outdir,"test_ID_withLabel.xlsx"))
write.xlsx(train_ID_withLabel_df,paste0(outdir,"train_ID_withLabel.xlsx"))

table(train_ID_withLabel_df[,SBCE_col]) #13555  1037 
table(test_ID_withLabel_df[,SBCE_col]) #3362  285 
