source("Recapse_Ultility.R")

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
data_dir  <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/WithPossibleMonthsHasNoCodes/")
data_dir2 <- paste0(proj_dir, "12B_Boxplot_AndAfterRemovalPCA/WithPossibleMonthsHasNoCodes/")

outdir2   <- paste0(proj_dir, "12B_Boxplot_AndAfterRemovalPCA/WithPossibleMonthsHasNoCodes/AfterRemovalPCA/")


######################################################################################################## 
#1. Load all pts model data
######################################################################################################## 
#1A. Load data
load(file = paste0(data_dir, "All_PTS_ModelReadyData.rda"))
label_col   <- "y_PRE_OR_POST_2ndEvent"

#1B. Change the label value from 0,1 to Pre,Post
pre_idxes  <- which(model_data[,label_col] == 0)
post_idxes <- which(model_data[,label_col] == 1)

model_data[pre_idxes,label_col] <- "Pre"
model_data[post_idxes,label_col] <- "Post"

#1C.Factorize the label col
model_data[,label_col] <- factor(model_data[,label_col],levels = c("Pre", "Post")) 

#1D.Change label name
colnames(model_data)[which(colnames(model_data) == label_col)] <- "Label" #Change label col name for plot

######################################################################################################## 
#Load weighted sum scores of for each sample by top 10 contribtued features in Dim 1 
######################################################################################################## 
weighted_sum_df <-read.csv(paste0(data_dir2,"Dim1_Weighted_Sum_feature.csv"),stringsAsFactors = F)
#add to model data
weighted_sum_df <- weighted_sum_df[match(model_data[,"sample_id"],weighted_sum_df[,"sample_id"]),]
model_data[,"Dim1Top10Fs_WeightedSumScore"] <- weighted_sum_df[,"Dim1Top10Fs_WeightedSumScore"]

######################################################################################################## 
#3.Find samples < threshold OR > threshold by Most contributed feature on PCA Dim1 by examing boxplot
######################################################################################################## 
sample1_idxes <- which(model_data[,"cumul_ratio_CCS_PROC_202"] < 0)
sample1_data  <- model_data[sample1_idxes,]
label_tb <- table(sample1_data[,"Label"]) #Pre: 346296, Post : 6364
print(paste("Labels (Sample1 to be treated as negtives):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))

######################################################################################################## 
#4.Find samples < threshold OR > threshold by Most contributed feature on PCA Dim1 by examing boxplot
######################################################################################################## 
# model_data_2f <- model_data[,c("sample_id","cumul_ratio_CCS_PROC_202",
#                                "cumul_ratio_CCS_PROC_227",
#                                "months_since_dx",
#                                "Label")]
#save(model_data_2f, file=paste0(outdir2, "3f_ModelReadyData.rda"))
load(file = paste0(outdir2, "3f_ModelReadyData.rda"))

thres1_list <- seq(5,7.5,0.1)
thres2_list <- 48
x_list <- as.data.frame(matrix(NA, nrow = length(thres1_list), ncol = length(thres2_list)))
y_list <- as.data.frame(matrix(NA, nrow = length(thres1_list), ncol = length(thres2_list))) #Ratio after removal

for (i in 1:length(thres1_list)){
  if(i %% 10 == 0){print(i)}
  thres1 <- thres1_list[i]
  for (j in 1:length(thres2_list)){
      thres2 <- thres2_list[j]
      #Data to be treated as negatives
      sample1_idxes <- which(model_data_2f[,"cumul_ratio_CCS_PROC_202"] == -1 |
                             model_data_2f[,"cumul_ratio_CCS_PROC_227"] > thres1 | 
                             model_data_2f[,"months_since_dx"] < thres2 )
      sample1_data  <- model_data_2f[sample1_idxes,]
      label_tb <- table(sample1_data[,"Label"])
      #print(paste("Labels (Sample1 to be treated as negatives):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))
      x_list[i,j] <- label_tb["Pre"]/(nrow(sample1_data)) #"PRE RATIO
      
      #after
      data_after <- model_data_2f[-sample1_idxes,]
      label_tb <- table(data_after[,"Label"]) 
      #print(paste("Labels (Sample After Removal):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))
      y_list[i,j] <- label_tb["Pre"]/label_tb["Post"]
      
  }
}

x_list_flat <- unlist(x_list)
y_list_flat <- unlist(y_list)
plot(x_list_flat,y_list_flat)

######################################################################################################## 
#Clear memory
######################################################################################################## 
rm(list = (c("sample2_data","weighted_sum_df")))
gc()
######################################################################################################## 
#4.Find samples < threshold OR > threshold by weighted sum feature examing boxplot
########################################################################################################
thres <- 0.01
sample2_idxes <- which(model_data[,"Dim1Top10Fs_WeightedSumScore"] < thres)
sample2_data  <- model_data[sample2_idxes,]
label_tb <- table(sample2_data[,"Label"]) #Pre: 346296, Post : 6364
print(paste("Labels (Sample2 to be treated as negtives):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))


#'@TODO
######################################################################################################## 
#4. Get sample IDs to be treated as negatives
######################################################################################################## 
sample_ID_df <- sample1_data[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df,paste0(outdir2,"SampleIDs1_ToBeTreatedAsNegtives.csv"))

sample_ID_df2 <- sample2_data[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df2,paste0(outdir2,"SampleIDs2_ToBeTreatedAsNegtives.csv"))

######################################################################################################## 
#5.remove the sample IDs treated as negatives from the model data
########################################################################################################
afterRemoval_data <- model_data[-sample2_idxes,]
label_tb <- table(afterRemoval_data[,"Label"]) 
print(paste("Labels (data exclude samples treated as negtives):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))
