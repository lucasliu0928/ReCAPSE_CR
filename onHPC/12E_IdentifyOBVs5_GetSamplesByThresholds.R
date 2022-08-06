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


get_thres_onevar_func <-function(thres_df,thres_name){
  thres <- thres_df[thres_df[,"vars"] == thres_name,"thresholds"]
  return(thres)
}

identify_neg_func <-function(in_data , thres_df){
  #1.print Original ratio
  sp_res        <- compute_sp_label_ratio(in_data)
  print("Original Ratio")
  print(sp_res)
  
  #2.Get threhold
  th1 <- as.numeric(unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_PROC202"),"="))[2])
  th2 <- as.numeric(unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_PROC227"),"<"))[2])
  th3 <- as.numeric(unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_months_since_dx"),"<"))[2])
  sample_idxes <- which(in_data[,"cumul_ratio_CCS_PROC_202"] == th1 |
                          in_data[,"cumul_ratio_CCS_PROC_227"] < th2 |
                          in_data[,"months_since_dx"] < th3 )
  sample_data <- in_data[sample_idxes,]
  
  #3.Print sample ratio
  sp_res        <- compute_sp_label_ratio(sample_data)
  print("SP Ratio")
  print(sp_res)
  
  #4.Only keep id and label
  sample_data <- sample_data[,c("study_id","sample_id","Label")]
  return(sample_data)
}


identify_pos_func <-function(in_data , thres_df,neg_sample_ids){
  #1.print Original ratio
  sp_res        <- compute_sp_label_ratio(in_data)
  print(sp_res)
  
  #2.Get threhold
  th1 <- unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_PROC202"),split = " < "))
  th2 <- unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_PROC227"),split = " < "))
  th3 <- unlist(strsplit(get_thres_onevar_func(thres_df,"Threshold_months_since_dx"),split = " < "))
  th1_l <- as.numeric(th1[1])
  th1_u <- as.numeric(th1[3])
  th2_l <- as.numeric(th2[1])
  th2_u <- as.numeric(th2[3])
  th3_l <- as.numeric(th3[1])
  th3_u <- as.numeric(th3[3])
  
  #Exclude obv neg IDs so that no overlap
  in_data <- in_data[-which(in_data[,"sample_id"] %in% neg_sample_ids),] 
  sample_idxes <-  which((in_data[,"cumul_ratio_CCS_PROC_202"] >  th1_l  & in_data[,"cumul_ratio_CCS_PROC_202"] <  th1_u )|
                           (in_data[,"cumul_ratio_CCS_PROC_227"] > th2_l & in_data[,"cumul_ratio_CCS_PROC_227"] < th2_u) |
                           (in_data[,"months_since_dx"] > th3_l & in_data[,"months_since_dx"] < th3_u))
  sample_data <- in_data[sample_idxes,]
  
  #3.Print sample ratio
  sp_res        <- compute_sp_label_ratio(sample_data)
  print(sp_res)
  
  #4.Only keep id and label
  sample_data <- sample_data[,c("study_id","sample_id","Label")]
  return(sample_data)
}


identify_nonobv_func <-function(in_data,obv_neg_IDs,obv_pos_IDs){
  sample_data <- in_data[-which(in_data[,"sample_id"] %in% c(obv_neg_IDs,obv_pos_IDs)),]
  #4.Only keep id and label
  sample_data <- sample_data[,c("study_id","sample_id","Label")]
  return(sample_data)
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

SBCE_col    <- "SBCE_Excluded_DeathPts" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
feature_set_name <- "CCSandVAL2nd"
if ((SBCE_col == "SBCE") | (SBCE_col == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
}

#data dir
data_dir1  <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/",feature_set_name,"/")
data_dir2  <- paste0(proj_dir, "11F_TrainTestIDs/",SBCE_col,"/")
data_dir3  <- paste0(proj_dir, "12A_PCA_VarContri_Train/",feature_set_name,"/",SBCE_col,"/")
data_dir4  <- paste0(proj_dir, "12D_OBVsSample_Thresholds/",feature_set_name,"/",SBCE_col,"/")

newout <- paste0("12E_OBVandNONOBV_SamplesIDs/",feature_set_name,"/",SBCE_col,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)




######################################################################################################## 
#1. Load all pts model data and get train and test data
######################################################################################################## 
#A. Load data
load(file = paste0(data_dir1, "All_PTS_ModelReadyData.rda"))
model_data[which(model_data[,label_col] == 0),label_col] <- "Pre"
model_data[which(model_data[,label_col] == 1),label_col] <- "Post"
model_data[,label_col] <- factor(model_data[,label_col],levels = c("Pre", "Post")) #Factorize the label col
colnames(model_data)[which(colnames(model_data) == label_col)] <- "Label" #change column name


#B.Load contribution file
feature_contribution_PCA <- read.csv(paste0(data_dir3,"PCA_Variable_Contribution.csv"),stringsAsFactors = F)
#only keep the 3 top contributed features and ids
top2fs_dim1 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.1"],decreasing = T),"X"][1:2]
top2fs_dim2 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.2"],decreasing = T),"X"][1:2]
model_data <- model_data[,c("study_id","sample_id",
                               top2fs_dim1,
                               top2fs_dim2,
                               "Label")]

#C. Load train patient IDs
train_ID_df <- read.xlsx(paste0(data_dir2,"train_ID_withLabel.xlsx"),sheet = 1) 
train_ID_df$study_id <- paste0("ID",train_ID_df$study_id)
train_IDs <- unique(train_ID_df[,"study_id"])

test_ID_df <- read.xlsx(paste0(data_dir2,"test_ID_withLabel.xlsx"),sheet = 1) 
test_ID_df$study_id <- paste0("ID",test_ID_df$study_id)
test_IDs <- unique(test_ID_df[,"study_id"])

#C.get train data and test data
train_data <- model_data[model_data[,"study_id"] %in% train_IDs,]
table(train_data$Label) #966866  32251

test_data <- model_data[model_data[,"study_id"] %in% test_IDs,]
table(test_data$Label) #239885   8847

######################################################################################################## 
#2.Load Thresholds
######################################################################################################## 
thres_neg_df <- read.csv(paste0(data_dir4,"Threshold_NEG.csv"),stringsAsFactors = F)
thres_pos_df <- read.csv(paste0(data_dir4,"Threshold_POS.csv"),stringsAsFactors = F)

########################################################################################################
#Identify sample for train data
########################################################################################################
print("Training NEG:")
#1.Get obv neg ids
neg_train_data <- identify_neg_func(train_data,thres_neg_df)
neg_train_sample_ids <- neg_train_data[,"sample_id"]
write.csv(neg_train_data,paste0(proj_dir, newout,"ObviousNeg_Samples_Train.csv"))

print("Training POS:")
#2.Get obv pos ids
pos_train_data <- identify_pos_func(train_data, thres_pos_df,neg_train_sample_ids)
pos_train_sample_ids <- pos_train_data[,"sample_id"]
write.csv(pos_train_data,paste0(proj_dir, newout,"ObviousPos_Samples_Train.csv"))

print("Training non-obv:")
#3.Get non-obv ids
non_obvious_train_data <- identify_nonobv_func(train_data,neg_train_sample_ids,pos_train_sample_ids)
write.csv(non_obvious_train_data,paste0(proj_dir, newout,"NON_Obvious_Samples_Train.csv"))

########################################################################################################
#Identify sample for test data
########################################################################################################
#1.Get obv neg ids
print("Testing NEG:")
neg_test_data <- identify_neg_func(test_data,thres_neg_df)
neg_test_sample_ids <- neg_test_data[,"sample_id"]
write.csv(neg_test_data,paste0(proj_dir, newout,"ObviousNeg_Samples_Test.csv"))

#2.Get obv pos ids
print("Testing POS:")
pos_test_data <- identify_pos_func(test_data, thres_pos_df,neg_test_sample_ids)
pos_test_sample_ids <- pos_test_data[,"sample_id"]
write.csv(pos_test_data,paste0(proj_dir, newout,"ObviousPos_Samples_Test.csv"))

#3.Get non-obv ids
print("Testing non-obv:")
non_obvious_test_data <- identify_nonobv_func(test_data,neg_test_sample_ids,pos_test_sample_ids)
write.csv(non_obvious_test_data,paste0(proj_dir, newout,"NON_Obvious_Samples_Test.csv"))

