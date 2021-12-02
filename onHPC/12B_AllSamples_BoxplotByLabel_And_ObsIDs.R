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
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir  <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/WithPossibleMonthsHasNoCodes/")
data_dir2 <-  paste0(proj_dir, "12A_PCA_TSNE_Analysis/WithPossibleMonthsHasNoCodes/")
  
outdir   <- paste0(proj_dir, "12B_Boxplot/WithPossibleMonthsHasNoCodes/")

######################################################################################################## 
#1. Load all pts model data
######################################################################################################## 
load(file = paste0(data_dir, "All_PTS_ModelReadyData.rda"))
label_col   <- "y_PRE_OR_POST_2ndEvent"
######################################################################################################## 
#Change the label value from 0,1 to Pre,Post
######################################################################################################## 
pre_idxes  <- which(model_data[,label_col] == 0)
post_idxes <- which(model_data[,label_col] == 1)

model_data[pre_idxes,label_col] <- "Pre"
model_data[post_idxes,label_col] <- "Post"

######################################################################################################## 
#Factorize the label col
######################################################################################################## 
model_data[,label_col] <- factor(model_data[,label_col],levels = c("Pre", "Post"))


######################################################################################################## 
#Change label col name for plot
######################################################################################################## 
colnames(model_data)[which(colnames(model_data) == label_col)] <- "Label"

######################################################################################################## 
#Get feature contribution to first 5 Dim of PCA
######################################################################################################## 
feature_contribution_PCA <- read.csv(paste0(data_dir2,"PCA_Variable_Contribution.csv"),stringsAsFactors = F)
top10Fs_dim1 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.1"],decreasing = T),"X"][1:10]
top10Fs_dim2 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.2"],decreasing = T),"X"][1:10]
top10Fs_dim3 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.3"],decreasing = T),"X"][1:10]
top10Fs_dim4 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.4"],decreasing = T),"X"][1:10]
top10Fs_dim5 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.5"],decreasing = T),"X"][1:10]

all_tops_fs <- unique(c(top10Fs_dim1,top10Fs_dim2,top10Fs_dim3,top10Fs_dim4,top10Fs_dim5))


######################################################################################################## 
#2.Boxplot
######################################################################################################## 
plot_df <- model_data
#Features to plot
# char_features_toinclude <- c("Enrolled_year","Age","months_since_dx","reg_age_at_dx")
# code_features_toinclude <- colnames(plot_df)[which(grepl("CCS|DM3",colnames(plot_df)))] #Code count feature and 3 transforamtion feature
# features <- c(char_features_toinclude,code_features_toinclude)
features <- all_tops_fs
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
  
  png(paste0(outdir,"Violin_Plot/",feature_col,".png"),res = 150,width = 500,height = 500)
  print(p)
  dev.off()
}

# # Overlaid histograms
# ggplot(plot_df, aes_string(x=feature_col, color="Label")) +
#   geom_histogram(fill="white", alpha=0.5, position="identity")
# 
# 
# check_df <- plot_df[which(plot_df$months_since_dx<=45),]
# table(check_df$Label)

######################################################################################################## 
#3.Obvous Pre samples by Most contributed feature on PCA Dim1 and 2 by examing boxplot
######################################################################################################## 
obs_sample1_idxes <- which(plot_df$Label == "Pre" & plot_df$cumul_ratio_CCS_PROC_202 <0)
obs_sample2_idxes <- which(plot_df$Label == "Pre" & plot_df$months_since_dx < 48)
all_indxes <- unique(obs_sample1_idxes,obs_sample2_idxes)
length(all_indxes) #346296

#Find obvious Pre sample corrreding patient ID
#If patient has obvibous pre sample, then it is removed from the analysis
obs_sample_IDs <- unique(plot_df[all_indxes,"study_id"]) #6513
obs_sample_IDs_df <-data.frame(Obs_Pre_study_id = obs_sample_IDs)

write.csv(obs_sample_IDs_df,paste0(outdir,"Obs_Pre_IDs.csv"))
