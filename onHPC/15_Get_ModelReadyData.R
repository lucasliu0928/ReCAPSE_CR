source("Recapse_Ultility.R")
library(fastDummies)

#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# # #local
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


##############################################################################
#1. Load data with bianry char feature
##############################################################################
All_data_withBinary_Char <- read.csv(paste0(outdir,"13_All_data_withBinary_Char.csv"), stringsAsFactors = F)

################################################################################ 
#2. load code features
################################################################################ 
diag_feature_df <- read.csv(paste0(data_dir,"12_diag_feature_df.csv"), stringsAsFactors = F)
proc_feature_df <- read.csv(paste0(data_dir,"12_proc_feature_df.csv"), stringsAsFactors = F)
drug_feature_df <- read.csv(paste0(data_dir,"12_drug_feature_df.csv"), stringsAsFactors = F)
all_code_feature_df <- cbind(diag_feature_df,proc_feature_df[,-1], drug_feature_df[-1])
all_code_feature_names <- colnames(all_code_feature_df)[-1]

##############################################################################
#3. Load selected_features
##############################################################################
featres_df <- read.csv(paste0(outdir,"13_Selected_Codefeature.csv"),stringsAsFactors = F)
selected_features <- featres_df$x #67
selected_code_feature_df <- all_code_feature_df[,selected_features]
################################################################################ 
#Combine 
################################################################################
#Combine
Comb_df <- cbind(All_data_withBinary_Char,selected_code_feature_df)

write.csv(Comb_df,paste0(outdir,"15_ModelReadyData.csv"),row.names = F)

