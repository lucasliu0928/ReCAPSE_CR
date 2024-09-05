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
data_dir2  <- paste0(proj_dir,"12E_OBVandNONOBV_SamplesIDs/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/")

newout <- paste0("18_Discrip_Statistics/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

################################################################################ 
#1. Load PTS level char for all analysis IDs 
################################################################################
pt_char_df <- load_pt_char_func(data_dir1)

################################################################################ 
#1.Load all train and test IDs with flag for obv pos/neg or non obv
################################################################################ 
train_res <- load_obsSample_IDs(data_dir2,"Train",pt_char_df)
all_train_df <- train_res[[1]]
all_train_pt_ids <- train_res[[2]] #14592
all_train_sp_ids <- train_res[[3]] #999117

test_res <- load_obsSample_IDs(data_dir2,"Test",pt_char_df)
all_test_df <- test_res[[1]]
all_test_pt_ids <- test_res[[2]] #3647
all_test_sp_ids <- test_res[[3]] #248732

all_analysis_pt_ids <-c(all_train_pt_ids,all_test_pt_ids) #18239
all_analysis_sp_ids <-c(all_train_sp_ids,all_test_sp_ids) #1247849

################################################################################ 
#3.Fiter pt char df for anlaysis Ids
################################################################################ 
pt_char_df <- pt_char_df[which(pt_char_df[,"study_id"] %in% all_analysis_pt_ids),]


################################################################################ 
#4.(Sample Level) Report pre/post status and number of sbce/nonsbce patients
#A. All Training
#B. All Testing
#C. obvious neg training
#D. obvious pos training
#E. non-obvious training
#F. obvious neg test
#G. obvious pos test
#H. non-obvious test
################################################################################ 
train_num_df1 <- compute_prepost_and_sbcepts_func(all_train_df, SBCE_col)
train_num_df2 <- compute_prepost_and_sbcepts_func(all_train_df[which(all_train_df$OBV_TYPE == "OBV_NEG"),],SBCE_col)
train_num_df3 <- compute_prepost_and_sbcepts_func(all_train_df[which(all_train_df$OBV_TYPE == "OBV_POS"),],SBCE_col)
train_num_df4 <- compute_prepost_and_sbcepts_func(all_train_df[which(all_train_df$OBV_TYPE == "OBV_NON"),],SBCE_col)

test_num_df1 <- compute_prepost_and_sbcepts_func(all_test_df,SBCE_col)
test_num_df2 <- compute_prepost_and_sbcepts_func(all_test_df[which(all_test_df$OBV_TYPE == "OBV_NEG"),],SBCE_col)
test_num_df3 <- compute_prepost_and_sbcepts_func(all_test_df[which(all_test_df$OBV_TYPE == "OBV_POS"),],SBCE_col)
test_num_df4 <- compute_prepost_and_sbcepts_func(all_test_df[which(all_test_df$OBV_TYPE == "OBV_NON"),],SBCE_col)

all_num_df <- rbind(train_num_df1,train_num_df2,train_num_df3,train_num_df4,
                    test_num_df1,test_num_df2,test_num_df3,test_num_df4)
rownames(all_num_df) <- c("All_Train","OBV_NEG_Train","OBV_POS_Train","OBV_NON_Train",
                          "All_Test","OBV_NEG_Test","OBV_POS_Test","OBV_NON_Test")
write.csv(all_num_df,paste0(outdir,"NUM_preOrpostSamples_sbcePatients_TrainTest.csv"))

################################################################################
#3.Get SBCE and non-SBCE pts char df  for compute stats separately
################################################################################
pt_char_df_SBCE1   <- pt_char_df[which(pt_char_df[,SBCE_col] == 1),]
pt_char_df_SBCE0   <- pt_char_df[which(pt_char_df[,SBCE_col] == 0),]

################################################################################ 
#3. Report some stats
#'@Updated 100521: 
#surg_prim_site_V1 and surg_prim_site_V2:
##Version1 (Dr.Huang): 0, 19, (20-24), 30, (40-42), (50-59,63),  (60-62, 64-69, 73,74), 70-72, 80, 90, 99
#Version2 (Quan):  00,19,20 (21-24),30,40,41,42,50,51(53-56),52(57,58,59,63),60,61(64-67),62(68,69,73,74),70,71,72,80,90,99
#'@Note: These table does not show all features for training (e.g, age at each month, monthes since diagnosis)
################################################################################ 
if ((SBCE_col == "SBCE") | (SBCE_col == "SBCE_Excluded_DeathPts")){
  num_month_vars <- c("Num_Month_before_2ndEvent","Num_Month_AfterOrEqual_2ndEvent")
}else{
  num_month_vars <- c("Num_Month_before_2ndEvent_ExcludedDeath", "Num_Month_AfterOrEqual_2ndEvent_ExcludedDeath")
}
all_variables <- c(SBCE_col,"Medicaid_OR_Medicare",
                   "DAJCC_M","DAJCC_N","DAJCC_T",
                   "reg_age_at_dx","Diagnosis_Year",
                   "most_recent_enrollment_year",
                   num_month_vars,
                   "Num_Enrolled_Prediction_Months",
                   "Type_2nd_Event",
                   "Race" , "Site" , "Stage","Grade",
                   "Laterality" ,"er_stat","pr_stat",	"her2_stat",	
                    "reg_nodes_exam", "reg_nodes_pos", "surg_prim_site_V1","surg_prim_site_V2",
                    "cs_tum_size", "cs_tum_ext", 
                    "cs_tum_nodes", "regional")

n_perc_variables <- c(SBCE_col, "Medicaid_OR_Medicare", "Race","Site","Stage","Grade","Laterality",
                      "er_stat","pr_stat","her2_stat","reg_nodes_exam", "reg_nodes_pos",
                      "surg_prim_site_V1","surg_prim_site_V2","regional",
                      "cs_tum_ext", 
                      "cs_tum_nodes",
                      "most_recent_enrollment_year","Diagnosis_Year","Type_2nd_Event",
                      "DAJCC_M","DAJCC_N","DAJCC_T")

table_all <- compute_stats_func(pt_char_df,"ALL",all_variables,n_perc_variables)
table_sbce <- compute_stats_func(pt_char_df_SBCE1,"SBCE",all_variables,n_perc_variables)
table_nonsbce <- compute_stats_func(pt_char_df_SBCE0,"non-SBCE",all_variables,n_perc_variables)

table_comb <- cbind(table_all,table_sbce,table_nonsbce)
write.csv(table_comb, paste0(outdir,"discrip_table2.csv"))

################################################################################ 
#Histogram
################################################################################ 
#Plot Diagnosis_Year:
output_hist_forSBCEand_nonSBCE(pt_char_df,"Diagnosis_Year","Diagnosis Year",
                               "Recurrent Patient","non-Recurrent Patient",
                               seq(2004, 2015, 1),1000,SBCE_col)

#Plot most_recent_enrollment_year: #"Both Medicaid Medicare"
#Medicaid
Medicaid_PTs_Char_df <- pt_char_df[which(pt_char_df$Medicaid_OR_Medicare== "Medicaid"),]
output_hist_forSBCEand_nonSBCE(Medicaid_PTs_Char_df,"most_recent_enrollment_year","Most Recent Enrollment Year",
                               "Recurrent Patient (Medicaid)","non-Recurrent Patient (Medicaid)",
                               seq(2005, 2020, 1),1200,SBCE_col)

Medicare_PTs_Char_df <- pt_char_df[which(pt_char_df$Medicaid_OR_Medicare== "Medicare"),]
output_hist_forSBCEand_nonSBCE(Medicare_PTs_Char_df,"most_recent_enrollment_year","Most Recent Enrollment Year",
                               "Recurrent Patient (Medicare)","non-Recurrent Patient (Medicare)",
                               seq(2005, 2020, 1),1200,SBCE_col)

Both_PTs_Char_df <- pt_char_df[which(pt_char_df$Medicaid_OR_Medicare== "Both"),]
output_hist_forSBCEand_nonSBCE(Both_PTs_Char_df,"most_recent_enrollment_year","Most Recent Enrollment Year",
                               "Recurrent Patient (Medicare & Medicaid)","non-Recurrent Patient (Medicare & Medicaid)", 
                               seq(2005, 2020, 1),1200,SBCE_col)

