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
data_dir3 <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")

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

#2B.Load CCS cateogry names and add discrption to imporatance matrix
Diag_grp <- read.xlsx(paste0(data_dir3,"Unique_Diag_And_Groups_inALLClaims.xlsx"), sheet = 1)
Proc_grp <- read.xlsx(paste0(data_dir3,"Unique_Proc_And_Groups_inALLClaims.xlsx"), sheet = 1)

#2C. Add CCS description
#var_contribution <- read.csv(paste0(outdir,"PCA_Variable_Contribution.csv"),stringsAsFactors = F)
var_contribution[,"CCS_descrption"] <- NA
for(i in 1:nrow(var_contribution)){ 
  if (i %% 50 == 0){print(i)}
  curr_feature <- var_contribution[i,"X"]
  
  if (grepl("CCS",curr_feature) == T){
    curr_feature <- gsub("CCS_|time_since_|time_until_|cumul_ratio_","",curr_feature)
    
    res <- unlist(strsplit(curr_feature,split = "_"))
    curr_ccs_code <- res[2]
    curr_ccs_type <- res[1]
    
    if (is.na(curr_ccs_code) == F & curr_ccs_code != "NA"){
      #Check if diag or proc
      if (curr_ccs_type == "DIAG"){
        curr_discrip <- find_ccs_discrption_func(Diag_grp,curr_ccs_code)
      }else if(curr_ccs_type == "PROC"){
        curr_discrip <- find_ccs_discrption_func(Proc_grp,curr_ccs_code)
      }
      #check if multiple, if so keep the longest one
      n_disc <- length(curr_discrip)
      n_char <- nchar(curr_discrip)
      if (n_disc > 1){ 
        curr_discrip <- curr_discrip[which(n_char == max(n_char))][1] #if still multiple keep the first one
      }else{
        curr_discrip <- curr_discrip
      }
    }else{
      curr_discrip <- NA
    }
    
  }else{
    curr_discrip <- NA
  }
  
  var_contribution[i,"CCS_descrption"] <- curr_discrip #if multiple has the same nchar, use the first one
}

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
