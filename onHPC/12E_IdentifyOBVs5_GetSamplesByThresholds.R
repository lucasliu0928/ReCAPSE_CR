source("Recapse_Ultility.R")
compute_sp_label_ratio <- function(in_data){
  label_tb <- table(in_data[,"Label"]) 
  ct_total <- nrow(in_data)
  ct_pre   <- label_tb["Pre"]
  ct_post  <- label_tb["Post"]
  neg_post_ratio <- round(ct_pre/ct_post,10)
  ct_tb <- data.frame("Total" = ct_total,"Pre"=ct_pre,"Post"=ct_post,"Ratio"=neg_post_ratio)
  rownames(ct_tb) <- NULL
  return(ct_tb)
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
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "12B_TopPCAFeatureData_Train/WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "12B_TopPCAFeatureData_Train/WithPossibleMonthsHasNoCodes/")

newout1 <- "12E_OBVandNONOBV_SamplesIDs/WithPossibleMonthsHasNoCodes/Train/"
outdir   <- paste0(proj_dir, newout1)
dir.create(file.path(proj_dir, newout1), recursive = TRUE)

newout2 <- "12E_OBVandNONOBV_SamplesIDs/WithPossibleMonthsHasNoCodes/Test/"
outdir   <- paste0(proj_dir, newout1)
dir.create(file.path(proj_dir, newout1), recursive = TRUE)

######################################################################################################## 
#1. Load all pts model data with four top features
######################################################################################################## 
#1A. Load data
load(file = paste0(data_dir, "Top4PCAFeature_ModelReadyData_Train.rda"))

#2B. Orginal NEG POS ratio
compute_sp_label_ratio(model_data_4f)

# ######################################################################################################## 
#Identify sample
# ######################################################################################################## 
# #3.Get exclusion samples IDs to be treated as negatives
# #Data to be treated as negatives
# final_sample_idxes <- which(model_data_4f[,"cumul_ratio_CCS_PROC_202"] == -1 |
#                        model_data_4f[,"cumul_ratio_CCS_PROC_227"] < best_th1 | 
#                       model_data_4f[,"months_since_dx"] < best_th2 )
# final_sample_data  <- model_data_4f[final_sample_idxes,]
# sp_res        <- compute_sp_label_ratio(final_sample_data)
# sp_res
# sp_res["Pre"]/sp_res["Total"]
# 
# sample_ID_df_NEG <- final_sample_data[,c("study_id","sample_id","Label")]
# write.csv(sample_ID_df_NEG,paste0(outdir,"ObviousNeg_Samples.csv"))

# #3.Get exclusion samples IDs to be treated as postives
# #Best threshold hold
# best_th1 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_PROC227"],split = " < "))
# best_th2 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_months_since_dx"],split = " < "))
# best_th3 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_PROC202"],split = " < "))
# 
# best_th1_l <- best_th1[1]
# best_th1_u <- best_th1[3]
# best_th2_l <- best_th2[1]
# best_th2_u <- best_th2[3]
# best_th3_l <- best_th3[1]
# best_th3_u <- best_th3[3]
# model_data_4f_updated <- model_data_4f[-which(model_data_4f$sample_id %in% sample_ID_df_NEG$sample_id),] #eclued obv neg IDs so that no overlap, cuz we use | in conditions
# final_sample_idxes <-  which((model_data_4f_updated[,"cumul_ratio_CCS_PROC_202"] >  best_th3_l  & model_data_4f_updated[,"cumul_ratio_CCS_PROC_202"] <  best_th3_u )|
#                             (model_data_4f_updated[,"cumul_ratio_CCS_PROC_227"] > best_th1_l & model_data_4f_updated[,"cumul_ratio_CCS_PROC_227"] < best_th1_u) | 
#                             (model_data_4f_updated[,"months_since_dx"] > best_th2_l & model_data_4f_updated[,"months_since_dx"] < best_th2_u))
# final_sample_data  <- model_data_4f_updated[final_sample_idxes,]
# 
# sp_res        <- compute_sp_label_ratio(final_sample_data)
# sp_res
# sp_res["Post"]/sp_res["Total"]
# 
# sample_ID_df_POS <- final_sample_data[,c("study_id","sample_id","Label")]
# write.csv(sample_ID_df_POS,paste0(outdir,"ObviousPos_Samples.csv"))


# ######################################################################################################## 
# #5. Get sample IDs after exclusion
# ######################################################################################################## 
# obv_neg_IDs <- sample_ID_df_NEG$sample_id
# obv_pos_IDs <- sample_ID_df_POS$sample_id
# 
# after_data <- model_data_4f[-which(model_data_4f$sample_id %in% c(obv_neg_IDs,obv_pos_IDs)),]
# after_res <- compute_sp_label_ratio(after_data)
# after_res
# 
# non_obvious_data <- after_data[,c("study_id","sample_id","Label")]
# write.csv(non_obvious_data,paste0(outdir,"NON_Obvious_Samples.csv"))
