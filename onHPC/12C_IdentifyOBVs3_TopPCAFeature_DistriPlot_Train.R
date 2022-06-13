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
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


SBCE_col    <- "SBCE_Excluded_DeathLabel" #choose SBCE or SBCE_Excluded_DeathLabel
feature_set_name <- "CCSandVAL2nd"
features <- c("cumul_ratio_CCS_PROC_202",
              "cumul_ratio_CCS_PROC_227",
              "months_since_dx",
              "Enrolled_year")
#data dir
data_dir  <- paste0(proj_dir,"12B_TopPCAFeatureData_Train/",feature_set_name,"/",SBCE_col,"/")

newout <- paste0("12C_TopPCAFeatureDistributionPlot_Train/",feature_set_name,"/",SBCE_col,"/")
newout1 <- paste0(newout, "Box_plot/")
newout2 <- paste0(newout, "Violin_plot/")
newout3 <- paste0(newout, "Histogram/")

dir.create(file.path(proj_dir, newout1), recursive = TRUE)
dir.create(file.path(proj_dir, newout2), recursive = TRUE)
dir.create(file.path(proj_dir, newout3), recursive = TRUE)

######################################################################################################## 
#1. Load top feature train data
######################################################################################################## 
#1A. Load data
load(file = paste0(data_dir, "Top4PCAFeature_ModelReadyData_Train.rda"))


# ######################################################################################################## 
# #2.Add weighted sum scores of for each sample by top 10 contribtued features(normed) in Dim 1 
# ######################################################################################################## 
# load(file = paste0(data_dir, "PCADim1Top10WSF_ModelReadyData_Train.rda"))
# weighted_sum_df <- weighted_sum_df[match(model_data_4f[,"sample_id"],weighted_sum_df[,"sample_id"]),]
# model_data_4f[,"Dim1Top10Fs_WeightedSumScore"] <- weighted_sum_df[,"Dim1Top10Fs_WeightedSumScore"]

#######################################################################################################
#3.Boxplot
#######################################################################################################
plot_df <- model_data_4f

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

  png(paste0(proj_dir, newout1,feature_col,".png"),res = 150,width = 500,height = 500)
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
  
  png(paste0(proj_dir, newout2,feature_col,".png"),res = 150,width = 500,height = 300)
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
    geom_histogram(fill="white",bins = 30) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          legend.position = "top" , 
          legend.title = element_blank())+
    scale_color_manual(values=c("darkgreen", "darkred")) 

  
  png(paste0(proj_dir, newout3, feature_col,".png"),res = 150,width = 700,height = 500)
  print(p)
  dev.off()
}


#######################################################################################################
#6. seperate group histogram Plot
#######################################################################################################
for (i in 1:length(features)){
  if (i %% 10 == 0){print(i)}
  feature_col <- features[i]
  p<-ggplot(plot_df, aes_string(x=feature_col,color="Label")) +
    geom_histogram(fill="white",bins = 30) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=12,face="bold"),
          legend.position = "none" , 
          legend.title = element_blank()) +
    facet_grid(Label ~ .,scales = "free_y") + 
    #facet_grid(Label ~ .) + 
    scale_color_manual(values=c("darkgreen", "darkred")) 
  png(paste0(proj_dir, newout3,feature_col,"PREPOST.png"),res = 150,width = 700,height = 500)
  print(p)
  dev.off()
}
