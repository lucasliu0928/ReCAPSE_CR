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
data_dir1        <- paste0(proj_dir, "15_XGB_Input/")

outdir   <- paste0(proj_dir, "12D_ExclusionSamples/WithPossibleMonthsHasNoCodes/Test/")


################################################################################
#1. Load test data
################################################################################
load(file = paste0(data_dir1, "test_data.rda"))
colnames(test_data)[which(colnames(test_data) == "y_PRE_OR_POST_2ndEvent")] <- "Label"

test_data[which(test_data$Label == 0),"Label"] <- "Pre"
test_data[which(test_data$Label == 1),"Label"] <- "Post"

#Original ratio
res        <- compute_sp_label_ratio(test_data)
res
res["Pre"]/res["Total"]

######################################################################################################## 
#2. OBV NEGTIVES use criteria from 12D Train
######################################################################################################## 
#Data to be treated as negatives
neg_sample_idxes <- which(test_data[,"cumul_ratio_CCS_PROC_202"] == -1 |
                          test_data[,"cumul_ratio_CCS_PROC_227"] < 0.2 | 
                          test_data[,"months_since_dx"] < 36 )
test_data_neg     <- test_data[neg_sample_idxes,]
sp_res        <- compute_sp_label_ratio(test_data_neg)
sp_res
sample_ID_df_NEG <- test_data_neg[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df_NEG,paste0(outdir,"ObviousNeg_Samples_Test.csv"))


######################################################################################################## 
#3. OBV Pos use criteria from 12D Train
######################################################################################################## 
#Data to be treated as postives
updated_test_data <- test_data[-which(test_data$sample_id %in% sample_ID_df_NEG$sample_id),] #exclude obv neg
pos_sample_idxes <-  which((updated_test_data[,"cumul_ratio_CCS_PROC_202"] >  0.2  & updated_test_data[,"cumul_ratio_CCS_PROC_202"] <  0.4 )|
                           (updated_test_data[,"cumul_ratio_CCS_PROC_227"] > 2 & updated_test_data[,"cumul_ratio_CCS_PROC_227"] < 4) | 
                           (updated_test_data[,"months_since_dx"] > 60 & updated_test_data[,"months_since_dx"] < 80))

test_data_pos     <- updated_test_data[pos_sample_idxes,]
sp_res        <- compute_sp_label_ratio(test_data_pos)
sp_res

sample_ID_df_POS <- test_data_pos[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df_POS,paste0(outdir,"ObviousPOS_Samples_Test.csv"))


######################################################################################################## 
#5. non-obv test
######################################################################################################## 
obv_neg_IDs <- sample_ID_df_NEG$sample_id
obv_pos_IDs <- sample_ID_df_POS$sample_id

after_data <- test_data[-which(test_data$sample_id %in% c(obv_neg_IDs,obv_pos_IDs)),]
after_res <- compute_sp_label_ratio(after_data)
after_res

non_obvious_data <- after_data[,c("study_id","sample_id","Label")]
write.csv(non_obvious_data,paste0(outdir,"NON_Obvious_Samples_Test.csv"))
