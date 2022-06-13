source("Recapse_Ultility.R")
#this script get patientt level prediction by 3 methods:
#1:change-point analysis


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

SBCE_col    <- "SBCE_Excluded_DeathLabel" #choose SBCE or SBCE_Excluded_DeathLabel
feature_set_name <- "CCSandVAL2nd"
if (SBCE_col == "SBCE"){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
}

#data dir
data_dir1 <- paste0(proj_dir, "16C_Predictions/",feature_set_name,"/",SBCE_col,"/Test/")
data_dir2 <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")

outdir    <- data_dir1

################################################################################ 
#3. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df$study_id <- paste0("ID",pts_level_char_df$study_id)

################################################################################ 
#2. Get predicted SBCE month
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
    thres_list <- seq(1,9,1)
    method_name <- "OneMonth_GT_Threshold"
    pt_pred_df1 <- get_allpt_level_pred(ds_pred_df,sbce_df,thres_list,method_name,SBCE_col)
    write.csv(pt_pred_df1,paste0(outdir, ds_out,model,"_",method_name,"_patientlevel_pred_tb.csv"))
    
    #4. Get predicted month and predicted label by persisit 3 months
    thres_list <- seq(1,9,1)
    method_name <- "Persis3Month_GT_Threshold"
    pt_pred_df2 <-  get_allpt_level_pred(ds_pred_df,sbce_df,thres_list,method_name,SBCE_col)
    write.csv(pt_pred_df2,paste0(outdir, ds_out,model,"_",method_name,"_patientlevel_pred_tb.csv"))
    
    #5. Get predicted month and predicted label by change point analysis
    method_name <- "BinSeg"
    pt_pred_df3 <-  get_allpt_level_pred(ds_pred_df,sbce_df,thres_list,method_name,SBCE_col)
    write.csv(pt_pred_df3,paste0(outdir, ds_out,model,"_",method_name,"_patientlevel_pred_tb.csv"))
    

  }
  
}
