source("Recapse_Ultility.R")

################################################################################
#Data dir
################################################################################
#onHPC
#proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

feature_set_name  <- "CCSandVAL2nd"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"

if ((SBCE_ID_Folder == "SBCE") | (SBCE_ID_Folder == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
  SBCE_col <- "SBCE"
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
  SBCE_col <- "SBCE_Excluded_DeathLabel"
}

#data dir
data_dir1  <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")

################################################################################ 
#1. Load PTS level char for all analysis IDs 
################################################################################
pt_char_df <- load_pt_char_func(data_dir1)

#Inclusion
cond1 <- pt_char_df[,"reg_age_at_dx"] < 18   #non-adult
cond2 <-  pt_char_df[,"Stage"]  %in% c(0,4) | is.na(pt_char_df[,"Stage"])==T   #stage 0 or 4, or na
cond3 <-  pt_char_df[,"Diagnosis_Year"]<2004 | pt_char_df[,"Diagnosis_Year"]>2015| is.na(pt_char_df[,"Diagnosis_Year"])==T # year before 2004 and > 2015
exclusion1_indxes <- which(cond1 | cond2 |cond3)
pt_char_df <- pt_char_df[-exclusion1_indxes,]

#Exclusion: non- local or regional stage for 1st priamry bc
exclusion2_indxes <- which(pt_char_df[,"Comb_SEERSummStg"]  %in% c(0,7,9) | is.na(pt_char_df[,"Comb_SEERSummStg"])==T) 
print(length(exclusion2_indxes))
pt_char_df <- pt_char_df[-exclusion2_indxes,]

#Exclusion: does not has enough months in window
#HasEnoughMonths_InWindow:
#1).  If SBCE, 
#     A. If death, at least have 3 month data before death
#     B, If recur or diag of 2nd primary, at least 3 month data before and after 2nd event
#2):  if not SBCE,
#     aat least 6 months of data avaiable
exclusion3_indxes <- which(pt_char_df[,"HasEnoughMonths_InWindow"] ==0 | is.na(pt_char_df[,"HasEnoughMonths_InWindow"])==T)
print(length(exclusion3_indxes))
pt_char_df <- pt_char_df[-exclusion3_indxes,]

table(pt_char_df$SBCE)
