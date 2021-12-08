source("Recapse_Ultility.R")
norm_minmax <- function(x){
  (x- min(x,na.rm = T)) /(max(x,na.rm = T)-min(x,na.rm = T))
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
data_dir  <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/WithPossibleMonthsHasNoCodes/")
data_dir2 <- paste0(proj_dir, "12B_TopPCAFeatureModelData/WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, "12C_DistributionPlot/WithPossibleMonthsHasNoCodes/")


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
#2.Add weighted sum scores of for each sample by top 10 contribtued features(normed) in Dim 1 
######################################################################################################## 
load(file = paste0(data_dir2, "Dim1_Weighted_Sum_feature.rda"))
weighted_sum_df <- weighted_sum_df[match(model_data[,"sample_id"],weighted_sum_df[,"sample_id"]),]
model_data[,"Dim1Top10Fs_WeightedSumScore"] <- weighted_sum_df[,"Dim1Top10Fs_WeightedSumScore"]

#######################################################################################################
#3.Boxplot
#######################################################################################################
plot_df <- model_data
features <- c("cumul_ratio_CCS_PROC_202",
              "cumul_ratio_CCS_PROC_227",
              "months_since_dx",
              "Enrolled_year",
              "Dim1Top10Fs_WeightedSumScore")
for (i in 1:length(features)){
  if (i %% 10 == 0){print(i)}
  feature_col <- features[i]
  p<-ggplot(plot_df, aes_string(x="Label", y=feature_col, color="Label")) +
    #geom_violin() +
    geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=23, size=2) +
    theme(axis.text=element_text(size=12),
           axis.title=element_text(size=14,face="bold"))+
    scale_color_manual(values=c("darkgreen", "darkred"))

  png(paste0(outdir,"Box_plot/",feature_col,".png"),res = 150,width = 500,height = 500)
  print(p)
  dev.off()
}


#######################################################################################################
#5. Violin Plot
#######################################################################################################
for (i in 1:length(features)){
  if (i %% 10 == 0){print(i)}
  feature_col <- features[i]
  p<-ggplot(plot_df, aes_string(x="Label", y=feature_col, color="Label")) +
    geom_violin() +
    #geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=23, size=2) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14,face="bold"))+
    scale_color_manual(values=c("darkgreen", "darkred"))
  
  png(paste0(outdir,"Violin_plot/",feature_col,".png"),res = 150,width = 500,height = 500)
  print(p)
  dev.off()
}


#######################################################################################################
#6. histogram Plot
#######################################################################################################
for (i in 1:length(features)){
  if (i %% 10 == 0){print(i)}
  feature_col <- features[i]
  p<-ggplot(plot_df, aes_string(x=feature_col, color="Label")) +
    geom_histogram(fill="white") +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=10,face="bold"))+
    scale_color_manual(values=c("darkgreen", "darkred"))
  
  png(paste0(outdir,"Histogram/",feature_col,".png"),res = 150,width = 500,height = 500)
  print(p)
  dev.off()
}
