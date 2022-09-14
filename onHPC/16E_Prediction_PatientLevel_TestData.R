source("Recapse_Ultility.R")
#this script get patientt level prediction by 3 methods:
#1:change-point analysis (BinSeg)
#2:OneMonth_GT_Threshold
#3:Persis3Month_GT_Threshold

run_3methods <- function(pred_table,sbce_df,SBCE_col,cohort_name,outdir,ds_out,model){
  #pred_table <- ds_pred_df_hascodes
  
  thres_list <- seq(1,9,1)
  method_name <- "OneMonth_GT_Threshold"
  pt_pred_df1 <- get_allpt_level_pred(pred_table,sbce_df,thres_list,method_name,SBCE_col)
  write.csv(pt_pred_df1,paste0(outdir, ds_out,model,"_",method_name,"_",cohort_name,"_patientlevel_pred_tb.csv"))
  
  #4. Get predicted month and predicted label by persisit 3 months
  thres_list <- seq(1,9,1)
  method_name <- "Persis3Month_GT_Threshold"
  pt_pred_df2 <-  get_allpt_level_pred(pred_table,sbce_df,thres_list,method_name,SBCE_col)
  write.csv(pt_pred_df2,paste0(outdir, ds_out,model,"_",method_name,"_",cohort_name,"_patientlevel_pred_tb.csv"))
  
  #5. Get predicted month and predicted label by change point analysis
  method_name <- "BinSeg"
  pt_pred_df3 <-  get_allpt_level_pred(pred_table,sbce_df,thres_list,method_name,SBCE_col)
  write.csv(pt_pred_df3,paste0(outdir, ds_out,model,"_",method_name,"_",cohort_name,"_patientlevel_pred_tb.csv"))
}
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
data_dir1 <- paste0(proj_dir, "16C_Predictions/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/Test/")
data_dir2 <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")
data_dir3 <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/",feature_set_name,"/","Samples_HasAtLeastOneCodeGrpFeature/") #To get sample labels who has at least one code

outdir    <- data_dir1

################################################################################ 
#1. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df$study_id <- paste0("ID",pts_level_char_df$study_id)

################################################################################ 
#2. Load sample IDs that has at least one code feaure
################################################################################ 
sample_hasonecode_df <- read.csv(paste0(data_dir3,"SampleIDs_HasAtLeastOneCodeGrpFeature.csv"),stringsAsFactors = F)
sp_id_has_codes <- unique(sample_hasonecode_df$sample_id)


################################################################################ 
#3. Get predicted SBCE month
################################################################################ 
#For each DS and each model
model_list <- c("Hybrid","AI","HybridCurveFit","AICurveFit")
for (ds_index in 0:10){
  #Create out dir for each ds 
  ds_out <- paste0("DS",ds_index,"/Patient_Prediction_Table/")
  dir.create(file.path(outdir, ds_out), recursive = TRUE)
  print(ds_out)
  for (model in model_list){
    model_pred_file <- paste0("pred_tb_",model,".csv")
    pred_col <- paste0("pred_Method_",model)
    
    #1. Load sample prediction table for current ds and curre model
    ds_in <- paste0(data_dir1,"DS",ds_index,"/Sample_Prediction_Table/")
    ds_pred_df <- read.csv(paste0(ds_in,model_pred_file),stringsAsFactors = F)
    ds_pred_df[,"month_start"] <- ymd(ds_pred_df[,"month_start"])
    
    #2.Get actual SBCE and SBCE month
    sbce_df <- get_pt_actual_sbcelabel_month(ds_pred_df,pts_level_char_df,SBCE_col)
    
    #3. Get  predicted month and label by the first month that the prediction probability is greater or equal to threholds
    #4. Get predicted month and predicted label by persisit 3 months
    #5. Get predicted month and predicted label by change point analysis
    cohort_name <- "AllSAMPLE"
    run_3methods(ds_pred_df,sbce_df,SBCE_col,cohort_name,outdir,ds_out,model)
 
    #6 Get sample pred table with at least one code  vs no code
    has_codes_idxes <- which(ds_pred_df[,"sample_id"] %in% sp_id_has_codes)
    ds_pred_df_hascodes <- ds_pred_df[has_codes_idxes,]
    ds_pred_df_nocodes  <- ds_pred_df[-has_codes_idxes,]
    
    #Re-run 3,4,5 for at least one code and no code
    #At least one code
    cohort_name <- "SAMPLEHASCODE"
    run_3methods(ds_pred_df_hascodes,sbce_df,SBCE_col,cohort_name,outdir,ds_out,model)
    
    #no code
    cohort_name <- "SAMPLENOCODE"
    run_3methods(ds_pred_df_nocodes,sbce_df,SBCE_col,cohort_name,outdir,ds_out,model)
    
    
  }
  
}
