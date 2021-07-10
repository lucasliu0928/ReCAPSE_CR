source("Recapse_Ultility.R")
library(fastDummies)

#onHPC
grp_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# # #local
# grp_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


##############################################################################
#1. Load data with bianry char feature
##############################################################################
All_data_withBinary_Char <- read.csv(paste0(outdir,"13_All_data_withBinary_Char.csv"), stringsAsFactors = F)
table(All_data_withBinary_Char$y_PRE_OR_POST_2ndEvent) #661098,  1 :31563 
final_ID <- unique(All_data_withBinary_Char$study_id)
pre_pt_Ids  <-  All_data_withBinary_Char$study_id[which(All_data_withBinary_Char$y_PRE_OR_POST_2ndEvent == 0)]
post_pt_Ids <-  All_data_withBinary_Char$study_id[which(All_data_withBinary_Char$y_PRE_OR_POST_2ndEvent == 1)]

################################################################################ 
#1. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% final_ID),]
table(pts_level_char_df$SBCE)
table(pts_level_char_df$First_Primary_BC_related_Death)
table(pts_level_char_df$First_Primary_BC_related_Death)/length(final_ID)*100
table(pts_level_char_df$Type_2nd_Event)
sbce_pt_Ids <- pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)]
nosbce_pt_Ids <- pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)]


################################################################################ 
#2. load code features
################################################################################ 
diag_feature_df <- read.csv(paste0(data_dir,"12_diag_feature_df.csv"), stringsAsFactors = F)
proc_feature_df <- read.csv(paste0(data_dir,"12_proc_feature_df.csv"), stringsAsFactors = F)
drug_feature_df <- read.csv(paste0(data_dir,"12_drug_feature_df.csv"), stringsAsFactors = F)
all_code_feature_df <- cbind(diag_feature_df,proc_feature_df[,-1], drug_feature_df[-1])


################################################################################ 
#Combine 
################################################################################
#Combine
Comb_df <- cbind(All_data_withBinary_Char,all_code_feature_df[,-1])
#write.csv(Comb_df,paste0(outdir,"13_Model_data_before_SelectionOfFeatures_onFreq.csv"),row.names = F)


################################################################################
#Count group code freq by pre or post
################################################################################
#1.Get group freq
pt_Id0 <- nosbce_pt_Ids
pt_ID1 <-  sbce_pt_Ids
  
Comb_df0 <-  Comb_df[which(Comb_df$study_id %in% pt_Id0),]
Comb_df1 <-  Comb_df[which(Comb_df$study_id %in% pt_ID1),]

code_feature_names <- colnames(all_code_feature_df)[-1]

#0
grp_freq_tb_0 <- as.data.frame(matrix(NA, nrow = length(code_feature_names), ncol = 3))
colnames(grp_freq_tb_0) <- c("Code_Group","N_SamplesHASCode","Perc_SamplesHASCode")
for (i in 1:length(code_feature_names)){
  curr_grp <- code_feature_names[i]

  n_HasCode_incurrGrp <- length(which(Comb_df0[,curr_grp]>=1))
  perc_HasCode_incurrGrp <- n_HasCode_incurrGrp/nrow(Comb_df0)
  
  grp_freq_tb_0[i, "Code_Group"] <- curr_grp
  grp_freq_tb_0[i, "N_SamplesHASCode"] <- n_HasCode_incurrGrp
  grp_freq_tb_0[i, "Perc_SamplesHASCode"] <- perc_HasCode_incurrGrp
}

#1
grp_freq_tb_1 <- as.data.frame(matrix(NA, nrow = length(code_feature_names), ncol = 3))
colnames(grp_freq_tb_1) <- c("Code_Group","N_SamplesHASCode","Perc_SamplesHASCode")
for (i in 1:length(code_feature_names)){
  curr_grp <- code_feature_names[i]
  
  n_HasCode_incurrGrp <- length(which(Comb_df1[,curr_grp]>=1))
  perc_HasCode_incurrGrp <- n_HasCode_incurrGrp/nrow(Comb_df1)
  
  grp_freq_tb_1[i, "Code_Group"] <- curr_grp
  grp_freq_tb_1[i, "N_SamplesHASCode"] <- n_HasCode_incurrGrp
  grp_freq_tb_1[i, "Perc_SamplesHASCode"] <- perc_HasCode_incurrGrp
}

#1.Get CCS group discription
CCS_df <- load_and_clean_CSS_data(grp_dir)
CCS_df$CCS.CATEGORY <- str_remove(CCS_df$CCS.CATEGORY, "^0+") #remove leading 0
CCS_Diag_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Diag","ICD10_Diag")),]
CCS_Proc_df <- CCS_df[which(CCS_df[,"CODE_TYPE"] %in% c("ICD9_Proc","ICD10_Proc")),]

freq_tb_0 <- get_ccs_discription(grp_freq_tb_0,CCS_Diag_df,CCS_Proc_df)
freq_tb_1 <- get_ccs_discription(grp_freq_tb_1,CCS_Diag_df,CCS_Proc_df)


write.csv(freq_tb_0,paste0(outdir,"13_grouped_freq_tb_nonsbce.csv"),row.names = F)
write.csv(freq_tb_1,paste0(outdir,"13_grouped_freq_tb_sbce.csv"),row.names = F)

#nonrecurrent patient thresholds are 0.15, 0.15, and 0.05, 
#and the recurrent patient thresholds are 0.10, 0.10, and 0.01
feature_set1 <- freq_tb_0$Code_Group[which(freq_tb_0$Perc_SamplesHASCode > 0.05)]
feature_set2 <- freq_tb_1$Code_Group[which(freq_tb_1$Perc_SamplesHASCode > 0.01)]
final_feature <- unique(c(feature_set1,feature_set2))

write.csv(final_feature,paste0(outdir,"13_Selected_Codefeature.csv"),row.names = F)
