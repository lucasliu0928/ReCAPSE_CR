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

feature_set_name <- "CCSandDM3SPE" #choose from CCSandDM3SPE , CCSandVAL2nd
sample_name      <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"
SBCE_ID_folder   <- "SBCE_Excluded_DeathPts"#Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts

if (SBCE_ID_folder == "SBCE" | (SBCE_ID_folder == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
  
}
  
#data dir
data_dir  <- paste0(proj_dir,"11E_AllPTs_ModelReadyData/",feature_set_name,"/",sample_name,"/")
data_dir2  <- paste0(proj_dir, "11F_TrainTestIDs/",feature_set_name, "/",sample_name, "/",SBCE_ID_folder,"/")
data_dir3 <- paste0(proj_dir, "12A_PCA_VarContri_Train/",feature_set_name,"/",sample_name,"/",SBCE_ID_folder,"/")

newout <- paste0("12B_TopPCAFeatureData_Train/",feature_set_name,"/",sample_name,"/",SBCE_ID_folder,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

######################################################################################################## 
#1.Load PCA feature contribution data
######################################################################################################## 
feature_contribution_PCA <- read.csv(paste0(data_dir3,"PCA_Variable_Contribution.csv"),stringsAsFactors = F)

######################################################################################################## 
#1. Get model data for train data only
######################################################################################################## 
#1A. Load all pts data
load(file = paste0(data_dir, "All_PTS_ModelReadyData.rda"))
if (grepl("Samples_HasAtLeastOneCodeGrpFeature",data_dir) == T){
  model_data <- model_data_excluded
}

#1B. Change the label value from 0,1 to Pre,Post
pre_idxes  <- which(model_data[,label_col] == 0)
post_idxes <- which(model_data[,label_col] == 1)

model_data[pre_idxes,label_col] <- "Pre"
model_data[post_idxes,label_col] <- "Post"

#1C.Factorize the label col
model_data[,label_col] <- factor(model_data[,label_col],levels = c("Pre", "Post")) 

#1D.Change label name
colnames(model_data)[which(colnames(model_data) == label_col)] <- "Label" #Change label col name for plot

#1E. Load train patient IDs
train_ID_df <- read.xlsx(paste0(data_dir2,"train_ID_withLabel.xlsx"),sheet = 1)
train_ID_df$study_id <- paste0("ID",train_ID_df$study_id)
train_IDs <- unique(train_ID_df[,"study_id"])

#1F.Updated model data for train data only
model_data <- model_data[model_data[,"study_id"] %in% train_IDs,]
table(model_data$Label)


######################################################################################################## 
#3.Output model ready data for top 2 features on dim1 and dim 2
######################################################################################################## 
#Get top 2 features from contribution dataframe
top2fs_dim1 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.1"],decreasing = T),"X"][1:2]
top2fs_dim2 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.2"],decreasing = T),"X"][1:2]
model_data_4f <- model_data[,c("study_id","sample_id",
                               top2fs_dim1,
                               top2fs_dim2,
                               "Label")]
save(model_data_4f, file=paste0(outdir, "Top4PCAFeature_ModelReadyData_Train.rda"))


######################################################################################################## 
#4.Get normalized top 10 features on dim 1 for computing weighted sum
######################################################################################################## 
top10Fs_dim1 <- feature_contribution_PCA[order(feature_contribution_PCA[,"Dim.1"],decreasing = T),"X"][1:10]
normalise_model_data <- as.data.frame(lapply(model_data[,top10Fs_dim1], norm_minmax))
colnames(normalise_model_data) <- paste0("Normed_",colnames(normalise_model_data))
model_data[,colnames(normalise_model_data)] <- normalise_model_data

######################################################################################################## 
#5.Compute weighted sum scores of for each sample by top 10 contributed features(normed) in Dim 1 
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
save(weighted_sum_df, file=paste0(outdir, "PCADim1Top10WSF_ModelReadyData_Train.rda"))
