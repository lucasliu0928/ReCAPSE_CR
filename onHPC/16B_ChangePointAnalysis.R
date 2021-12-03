library(lubridate)
library(openxlsx)
library(changepoint)
#This scrpt use change point analysis to get the turning point of each patients (before smoothed)

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/Use_ImportantFs_Performance/")
data_dir2        <- paste0(proj_dir, "12C_TrainTestIDs/")

outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/Use_ImportantFs_Performance/")

#User Input
ds_index <- 1

######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed","/16_Prediction_Table_DS", ds_index, ".csv"))

#Add study_id and month start
original_IDs <- strsplit(as.character(test_prediction_df$sample_id),split = "@")
test_prediction_df$study_id    <- gsub("ID","",sapply(original_IDs, "[[", 1))
test_prediction_df$month_start <- sapply(original_IDs, "[[", 2)
test_ID <- unique(test_prediction_df$study_id)

################################################################################ 
#2. Load label df to get SBCE or not 
################################################################################ 
SBCE_label_df <- read.xlsx(paste0(data_dir2,"/test_ID_withLabel.xlsx"),sheet = 1)

################################################################################ 
#3.Change point anlaysis
#'@NOTE: funciton adopted from plot_changepoint_info() from src/Tomas/run_xgboost.s3.r
################################################################################ 
changepoint_df <- as.data.frame(matrix(NA, nrow = length(test_ID), ncol = 2))
colnames(changepoint_df) <- c("study_id","changepoint_month")

for (i in 1:length(test_ID)){
  if(i%% 500 == 0){print(i)}
  curr_id <- test_ID[i]
  #get prediction df
  curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
  curr_df$month_start <- ymd(curr_df$month_start)
  
  #get label
  curr_label_df <- SBCE_label_df[which(SBCE_label_df[,"study_id"] == curr_id),]
  curr_label <- curr_label_df[,"SBCE"]
  
  changepoints <- cpt.meanvar(curr_df$pred, Q=1, method="BinSeg", test.stat="Normal")
  changepoint_index <- changepoints@cpts[1]
  
  #plot(changepoints, cpt.width=3)
  changepoint_df[i,"study_id"] <- curr_id
  changepoint_df[i,"changepoint_month"] <-  as.character(curr_df$month_start[changepoint_index])
}

write.csv(changepoint_df,paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed","/16_ChangePoint",".csv"),row.names = F)
