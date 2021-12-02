source("Recapse_Ultility.R")
library(Rtsne)
library("FactoMineR")
library("factoextra")

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

outdir   <- paste0(proj_dir, "12A_PCA_TSNE_Analysis/WithPossibleMonthsHasNoCodes/")

######################################################################################################## 
#1. Load all pts model data
######################################################################################################## 
load(file = paste0(data_dir, "All_PTS_ModelReadyData.rda"))

####################################################################################################
#Run PCA to get most contributions of features (non catogorical features)
####################################################################################################
#Get PCA data 
char_features_toinclude <- c("Enrolled_year","Age","months_since_dx","reg_age_at_dx")
code_features_toinclude <- colnames(model_data)[which(grepl("CCS|DM3",colnames(model_data)))] #Code count feature and 3 transforamtion feature
PCA_df <- model_data[,c(char_features_toinclude,code_features_toinclude)]
rownames(PCA_df) <- model_data$sample_id

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
write.csv(feature_removed,paste0(outdir,"ConstFeature_removed_ForPCAandtSNE.csv"))

#remove features
PCA_df <- PCA_df[,-to_remove_f_idxes]


#Run PCA
res.pca <- PCA(PCA_df, graph = FALSE)
eig.val <- get_eigenvalue(res.pca)
write.csv(eig.val,paste0(outdir,"PCA_Eigenvalues.csv"))

#Perc of explained Variation
p <- fviz_eig(res.pca, ncp = 10, addlabels = TRUE, ylim = c(0, max(eig.val[,2] + eig.val[,2]/5)))
png(paste0(outdir,"PCA_Explained_Var.png"),res = 150,width = 1800,height = 1200)
print(p)
dev.off()


#Get varaible contribution
var <- get_pca_var(res.pca)
var_contribution <- var$contrib
write.csv(var_contribution,paste0(outdir,"PCA_Variable_Contribution.csv"))


#plot
p <- fviz_pca_ind(res.pca,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = as.factor(model_data$y_PRE_OR_POST_2ndEvent), # color by groups
             palette = c("#00AFBB", "#E7B800"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups")

png(paste0(outdir,"PCA_2DPlot.png"),res = 150,width = 1800,height = 1200)
print(p)
dev.off()


####################################################################################################
# Run tsne
####################################################################################################
set.seed(42)
tsne_out <- Rtsne(PCA_df,pca=TRUE,perplexity=50) # Run TSNE

tsne_out_df <- data.frame(Y = tsne_out$Y,
                          Class_label = model_data$y_PRE_OR_POST_2ndEvent,
                          ID = rownames(PCA_df))

p <- ggplot(tsne_out_df, aes(x=Y.1, y=Y.2,color = Class_label)) +
  geom_point(size = 5) +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black")) +
  theme(legend.position="none",legend.title = element_blank(),legend.text=element_text(size=20)) +
  theme(axis.text = element_text(size = 20),axis.title=element_text(size=20,face="bold")) +
  guides(color = guide_legend(nrow = 3, byrow = TRUE)) +
  scale_x_continuous(name ="Tsne Dim1",limits = c(min(tsne_out_df$Y.1),max(tsne_out_df$Y.1))) +
  scale_y_continuous(name ="Tsne Dim2",limits = c(min(tsne_out_df$Y.2),max(tsne_out_df$Y.2)))

png(paste0(outdir,"tSNE_2DPlot.png"),res = 150,width = 1800,height = 1200)
print(p)
dev.off()
