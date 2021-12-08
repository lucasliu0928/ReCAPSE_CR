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
data_dir2 <- paste0(proj_dir, "12A_PCA_TSNE_Analysis/WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, "12B_Boxplot_AndAfterRemovalPCA/WithPossibleMonthsHasNoCodes/")


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
#2.Load feature contribution data
######################################################################################################## 
#2A Load data
feature_contribution_PCA <- read.csv(paste0(data_dir2,"PCA_Variable_Contribution.csv"),stringsAsFactors = F)

######################################################################################################## 
#Get feature contribution to first 5 Dim of PCA
######################################################################################################## 
top10Fs_dim1 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.1"],decreasing = T),"X"][1:10]


######################################################################################################## 
#Get normlized top 10 features on dim 1
######################################################################################################## 
normalise_model_data <- as.data.frame(lapply(model_data[,top10Fs_dim1], norm_minmax))
colnames(normalise_model_data) <- paste0("Normed_",colnames(normalise_model_data))
model_data[,colnames(normalise_model_data)] <- normalise_model_data

######################################################################################################## 
#Compute weighted sum scores of for each sample by top 10 contribtued features(normed) in Dim 1 
######################################################################################################## 
top10Fs_dim1_contribution_df <- feature_contribution_PCA[which(feature_contribution_PCA$X %in% top10Fs_dim1),c("X","Dim.1")]
top10Fs_dim1_contribution_df <- top10Fs_dim1_contribution_df[match(top10Fs_dim1,top10Fs_dim1_contribution_df$X),]

model_data[,"Dim1Top10Fs_WeightedSumScore"] <- NA
for (i in 1:nrow(model_data)){
  if (i %% 10000 == 0){print(i)}
  curr_pt_df <- model_data[i,paste0("Normed_",top10Fs_dim1)]
  curr_weighted_score <- sum(curr_pt_df*top10Fs_dim1_contribution_df[,"Dim.1"])
  model_data[i,"Dim1Top10Fs_WeightedSumScore"] <- curr_weighted_score
}

weighted_sum_df <- model_data[,c("study_id","sample_id","Dim1Top10Fs_WeightedSumScore",paste0("Normed_",top10Fs_dim1))]
write.csv(weighted_sum_df,paste0(outdir,"Dim1_Weighted_Sum_feature.csv"))
#save(weighted_sum_df, file=paste0(outdir, "Dim1_Weighted_Sum_feature.rda"))


#######################################################################################################
#2.Boxplot
#######################################################################################################
plot_df <- model_data
#features <- c("Dim1Top10Fs_WeightedSumScore",top10Fs_dim1)
features <- c("months_since_dx","Enrolled_year")
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
