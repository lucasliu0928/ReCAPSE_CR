source("Recapse_Ultility.R")
#Patient level
compute_binaryclass_perf_func2_PTLEVEL <- function(prediction_df,thresholdClass_col,SBCE_col){
  #'@NOTE: #There is no AUC in this function, since there is no prediction prob, 
  #'#we predict patient level by estimate the month of SBCE from sample predictions
  predicted_class <- prediction_df[,thresholdClass_col]
  actual_label    <- prediction_df[,SBCE_col]
  
  #Match label factor levels
  matched_res   <- match_label_levels_func(predicted_class,actual_label)
  final_pred    <- matched_res[[1]]
  final_actual  <- matched_res[[2]]
  
  cm<-confusionMatrix(final_pred, final_actual, positive = "1", dnn = c("Prediction", "TrueLabels"),mode = "everything")
  #Manually get TN, FP, TP, FN
  TN <- length(which(final_pred==0 & final_actual==0))
  FP <- length(which(final_pred==1 & final_actual==0))
  TP <- length(which(final_pred==1 & final_actual==1))
  FN <- length(which(final_pred==0 & final_actual==1))

  #class 1
  performance_table <- cm$byClass[c("Sensitivity","Specificity",
                                    "Pos Pred Value","Neg Pred Value",
                                    "Precision", "Recall","F1")]
  performance_table["Accuracy"] <- cm$overall[1]
  performance_table["TNR_Specificity"] <- TN/(TN + FP) #True negative rate = specificity
  performance_table["FPR"] <- FP/(TN + FP) #False postive rate
  performance_table["TPR_Sensitivity_Recall"] <- TP/(TP + FN) #True postive rate = sensitivity = recall
  performance_table["FNR"] <- FN/(TP + FN) #False negative rate 
  
  performance_table <- round(performance_table,2)
  
  return(performance_table)
}

get_perf_table_PTLEVEL_func<- function(analysis_df,thres,SBCE_col){
  #analysis_df = ds_pred_df
  
  #Get SBCE or nonSBCE numbers
  n_nonSBCE           <- length(which(analysis_df[,SBCE_col]==0))
  n_SBCE              <- length(which(analysis_df[,SBCE_col]==1))
  
  #Get performance for each thresho
  thres_class_cols <- paste0("Pred_SBCEClass_Thres_0",thres)
  
  final_perf_df<- as.data.frame(matrix(NA, nrow = length(thres_class_cols),ncol = 11))
  colnames(final_perf_df) <- c("Threshold","N_NonRecurrent","N_Recurrent",
                               "Accuracy","Recall/Sensitivity/TPR",
                               "Specificity/TNR",
                               "Precision/PPV","F1",
                               "NPV","FPR","FNR")
  for (i in 1:length(thres_class_cols)){
    final_perf_df[i,"Threshold"]          <- thres_class_cols[i]
    final_perf_df[i,"N_NonRecurrent"]     <- n_nonSBCE
    final_perf_df[i,"N_Recurrent"]        <- n_SBCE
    
    curr_perf <- compute_binaryclass_perf_func2_PTLEVEL(analysis_df,thres_class_cols[i],SBCE_col)
    
    final_perf_df[i,"Accuracy"]               <- curr_perf["Accuracy"]
    
    final_perf_df[i,"Recall/Sensitivity/TPR"] <- curr_perf["Recall"]
    final_perf_df[i,"Specificity/TNR"]        <- curr_perf["Specificity"]
    final_perf_df[i,"Precision/PPV"]          <- curr_perf["Precision"]
    final_perf_df[i,"F1"]                     <- curr_perf["F1"]
    final_perf_df[i,"NPV"]                    <- curr_perf["Neg Pred Value"]
    final_perf_df[i,"FPR"]                    <- curr_perf["FPR"]
    final_perf_df[i,"FNR"]                    <- curr_perf["FNR"]
    
  }
  
  return(final_perf_df)
}

#Compute the diffrence between predicted month and actual month
compute_month_diff <- function(analysis_df,thres){
  pt_diff_df<- as.data.frame(matrix(NA, nrow = nrow(analysis_df),ncol = length(thres) + 1))
  colnames(pt_diff_df) <- c("study_id",paste0("DiffinMonth_Thres_0",thres))
  for (i in 1:nrow(analysis_df)){
    curr_id <- analysis_df[i,"study_id"]
    curr_acutal_month <- ymd(analysis_df[i,"Acutal_SBCEMonth"])
    pt_diff_df[i,"study_id"] <- curr_id
    for (j in 1:length(thres)){
      curr_th <- thres[j]
      curr_th_col <- paste0("Pred_SBCEMon_Thres_0", curr_th)
      curr_pred_month <- analysis_df[i,curr_th_col]
      if (curr_pred_month == "NONE"){
        curr_diff <- Inf
      }else{
        curr_diff <- as.numeric(difftime(ymd(curr_pred_month),curr_acutal_month, units = "days"))/30
      }
      curr_th_diff_col <- paste0("DiffinMonth_Thres_0", curr_th)
      pt_diff_df[i,curr_th_diff_col] <- curr_diff
      
    }
    
  }
  
  return(pt_diff_df)
}

#Compute performance of month difference
get_stats_month_diff <- function(analysis_df,thres){

  #total num of SBCE patients
  n_SBCE <- nrow(analysis_df)
  
  #get abs diff first
  analysis_df[,2:ncol(analysis_df)] <- abs(analysis_df[,2:ncol(analysis_df)])
  
  diff_stat_df<- as.data.frame(matrix(NA, nrow = length(thres),ncol = 9))
  colnames(diff_stat_df) <- c("Threshold",
                              "Num_PTs_HasPredictedSBCEMonth/n_TotalSBCE",
                              "Mean_Diff_InMonth",
                              "Std_Diff_InMonth", "Median_Diff_InMonth",
                              "Q1_Diff_InMonth","Q3_Diff_InMonth",
                              "Min_Diff_InMonth", "Max_Diff_InMonth")
  for (i in 1:length(thres)){
    curr_th <- thres[i]
    curr_th_col <- paste0("DiffinMonth_Thres_0",curr_th)
    
    curr_col_vals <- analysis_df[,curr_th_col] 
    
    #Get indxes of diff is inf
    curr_indxes_INF <- which(curr_col_vals == Inf)
    #exclude INF
    if(length(curr_indxes_INF) >0 ){
      curr_col_vals_excINF <- curr_col_vals[-curr_indxes_INF]
    }else{
      curr_col_vals_excINF <- curr_col_vals
    }
    
    diff_stat_df[i,"Threshold"]                       <- curr_th
    diff_stat_df[i,"Num_PTs_HasPredictedSBCEMonth/n_TotalSBCE"]   <- paste0(length(curr_col_vals_excINF),"/",n_SBCE)
    diff_stat_df[i,"Mean_Diff_InMonth"]               <- mean(curr_col_vals_excINF)
    diff_stat_df[i,"Std_Diff_InMonth"]                <- sd(curr_col_vals_excINF)
    diff_stat_df[i,"Median_Diff_InMonth"]             <- median(curr_col_vals_excINF)
    diff_stat_df[i,"Min_Diff_InMonth"]                <- min(curr_col_vals_excINF)
    diff_stat_df[i,"Max_Diff_InMonth"]                <- max(curr_col_vals_excINF)
    diff_stat_df[i,"Q1_Diff_InMonth"]                 <- quantile(curr_col_vals_excINF,0.25)
    diff_stat_df[i,"Q3_Diff_InMonth"]                 <- quantile(curr_col_vals_excINF,0.75)
    
    
    
  }
  return(diff_stat_df)
  
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

feature_set_name  <- "CCSandDM3SPE"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"


if ((SBCE_ID_Folder == "SBCE") | (SBCE_ID_Folder == "SBCE_Excluded_DeathPts")){
  SBCE_col <- "SBCE"
}else{
  SBCE_col <- "SBCE_Excluded_DeathLabel"
}

#data dir
data_dir1 <- paste0(proj_dir, "16C_Predictions/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/Test/")
data_dir2 <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")

outdir <- paste0(proj_dir,"17_Performance/",feature_set_name,"/",sample_name,"/", SBCE_ID_Folder, "/")


################################################################################ 
#1. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir2,"/8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df$study_id <- paste0("ID",pts_level_char_df$study_id)

################################################################################ 
#2. Compute performance
################################################################################ 
model_list <- c("Hybrid","AI","HybridCurveFit","AICurveFit")
#model_list <- c("AI")
method_list <- c("BinSeg","OneMonth_GT_Threshold","Persis3Month_GT_Threshold")
ths <- seq(1,9,1)

for (ds_index in 0:10){
  print(ds_index)
  #Create out dir for each ds 
  ds_out <- paste0("DS",ds_index,"/Patient_Level/")
  dir.create(file.path(outdir, ds_out), recursive = TRUE)
  
  for (model in model_list){
    for (method in method_list){
        model_pred_file <- paste0(model,"_",method,"_patientlevel_pred_tb.csv")
        
        #1. Load all patient prediction table
        ds_in <- paste0(data_dir1,"DS",ds_index,"/Patient_Prediction_Table/")
        ds_pred_df <- read.csv(paste0(ds_in,model_pred_file),stringsAsFactors = F)
        
        #Get threshold column surfix
        if (method == "BinSeg"){
          thres = c("BinSeg")
        }else {
          thres <- seq(1,9,1)
        }
        #3. Get classification performance for all test
        perf_tb_alltest <- get_perf_table_PTLEVEL_func(ds_pred_df,thres,SBCE_col)
        write.csv(perf_tb_alltest,paste0(outdir, ds_out, model,method,"_perf_tb_alltest",".csv"),row.names = T)

        #3.Get prediction df for SBCE patients only
        ds_pred_df_SBCE<- ds_pred_df[which(ds_pred_df[,SBCE_col]==1), ]
        #3.1 compute the difference between predicted SBCE month and actual SBCE month
        monthdiff_df_SBCE <- compute_month_diff(ds_pred_df_SBCE,thres)
        #3.2 Compute the performance of month difference
        perf_tb_monthdiff_SBCE  <- get_stats_month_diff(monthdiff_df_SBCE,thres)
        write.csv(perf_tb_monthdiff_SBCE,paste0(outdir, ds_out, model,method,"_MonthDiff_Perf_SBCE",".csv"),row.names = T)
      
      }
  }
  
}