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
#4. Get sample IDs to be treated as negatives
######################################################################################################## 
sample_ID_df <- read.csv(paste0(outdir2,"SampleIDs1_ToBeTreatedAsNegtives.csv"),stringsAsFactors = F)
sample_ID_df2 <- read.csv(paste0(outdir2,"SampleIDs2_ToBeTreatedAsNegtives.csv"),stringsAsFactors = F)

######################################################################################################## 
#5.Run PCA again on Data after remove the sample IDs treated as negatives from the model data
########################################################################################################
sample_idxes <- which(model_data[,"sample_Id"] %in% sample_ID_df2[,"sample_Id"])
afterRemoval_data <- model_data[-sample_idxes,]
label_tb <- table(afterRemoval_data[,"Label"]) 
print(paste("Labels (data exclude samples treated as negtives):","Pre:",label_tb["Pre"],"Post:",label_tb["Post"]))

######################################################################################################## 
#6.release memory and load model_data again
######################################################################################################## 
rm(list = c("model_data","sample1_data","sample_ID_df"))
gc()

#Get PCA data 
char_features_toinclude <- c("Enrolled_year","Age","months_since_dx","reg_age_at_dx")
code_features_toinclude <- colnames(afterRemoval_data)[which(grepl("CCS|DM3",colnames(afterRemoval_data)))] #Code count feature and 3 transforamtion feature
PCA_df <- afterRemoval_data[,c(char_features_toinclude,code_features_toinclude)]
rownames(PCA_df) <- afterRemoval_data$sample_id

#Exclude features that is the same for all samples 
to_remove_f_idxes <- NA
ct <- 1
for (j in 1:ncol(PCA_df)){
  if (min(PCA_df[,j]) == max(PCA_df[,j])){
    to_remove_f_idxes[ct] <- j
    ct <- ct+1
  }
}

feature_removed <- data.frame(colnames(PCA_df)[to_remove_f_idxes])
write.csv(feature_removed,paste0(outdir2,"ConstFeature_removed_ForPCAandtSNE.csv"))

#remove features
PCA_df <- PCA_df[,-to_remove_f_idxes]


#Run PCA
res.pca <- PCA(PCA_df, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
write.csv(eig.val,paste0(outdir2,"PCA_Eigenvalues.csv"))

#Perc of explained Variation
p <- fviz_eig(res.pca, ncp = 10, addlabels = TRUE, ylim = c(0, max(eig.val[,2] + eig.val[,2]/5)))
png(paste0(outdir2,"PCA_Explained_Var.png"),res = 150,width = 1800,height = 1200)
print(p)
dev.off()


#Get varaible contribution
var <- get_pca_var(res.pca)
var_contribution <- var$contrib
write.csv(var_contribution,paste0(outdir2,"PCA_Variable_Contribution.csv"))


#plot
p <- fviz_pca_ind(res.pca,
                  geom.ind = "point", # show points only (nbut not "text")
                  col.ind = as.factor(afterRemoval_data$Label), # color by groups
                  palette = c("#00AFBB", "#E7B800"),
                  addEllipses = TRUE, # Concentration ellipses
                  legend.title = "Groups")

png(paste0(outdir2,"PCA_2DPlot.png"),res = 150,width = 1800,height = 1200)
print(p)
dev.off()