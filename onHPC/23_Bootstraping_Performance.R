source("Recapse_Ultility.R")


"20_Python_Results/CCSandVAL2nd/SBCE/XGB/DS3/nonobv/Grid/Prediction/Full_Model"


################################################################################
#Data dir
################################################################################
#onHPC
# proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/jliu6/Library/CloudStorage/OneDrive-FredHutchinsonCancerCenter/Projects/ReCAPSE/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

feature_set_name  <- "CCSandVAL2nd"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
method_name <- "XGB"
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"

if ((SBCE_ID_Folder == "SBCE") | (SBCE_ID_Folder == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent" 
  SBCE_col <- "SBCE"
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath" 
  SBCE_col <- "SBCE_Excluded_DeathLabel"
}

#data dir
data_dir1 <- paste0(proj_dir, "16C_Predictions/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/Test/")
data_dir2 <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")
data_dir3 <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/",feature_set_name,"/","Samples_HasAtLeastOneCodeGrpFeature/") #To get sample labels who has at least one code

newout <- paste0("17_Performance/",feature_set_name,"/",sample_name,"/", SBCE_ID_Folder, "/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)
