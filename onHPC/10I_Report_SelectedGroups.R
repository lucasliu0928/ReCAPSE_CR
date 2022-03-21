library(openxlsx)
data_df <- read.xlsx("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10H_Selected_Grps/WithPossibleMonthsHasNoCodes/Selected_VAL2ndDrug_Unique_Grps.xlsx",sheet = 1)
data_df$Selected_Grps <- gsub("VAL_2ND_","",data_df$Selected_Grps)

grp_info_df <- read.xlsx("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Code_Groups/Val_Quan_Final SecondRoot List and NDC.xlsx",sheet = 1)
grp_info_df <- grp_info_df[!duplicated(grp_info_df[,c("SECONDARY_CLASSIFICATION")]),] #only keep unique 2nd groups

freq_info_df <- read.xlsx("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10G_Counts_UniqueGrp_PtsLevel/WithPossibleMonthsHasNoCodes/Count_VAL_2ND_Unique_Grps.xlsx")
freq_info_df$Num_Pts_TOTAL <- freq_info_df$Num_PtsHasTheGrp_SBCE + freq_info_df$Num_PtsHasTheGrp_nonSBCE
TOTAL_N_PTS <- 18239
freq_info_df$freq_TOTAL <- freq_info_df$Num_Pts_TOTAL/TOTAL_N_PTS
freq_info_df$Code_Grp <- gsub("VAL_2ND_","",freq_info_df$Code_Grp)

grp_info_df$INCLUDED <- NA
grp_info_df$Num_Pts_TOTAL <- NA
grp_info_df$freq_TOTAL <- NA

for (i in 1:nrow(grp_info_df)){
  cur_group <- grp_info_df$SECONDARY_CLASSIFICATION[i]
  if (cur_group %in% data_df$Selected_Grps){
    grp_info_df$INCLUDED[i] <- 1
  }else{
    grp_info_df$INCLUDED[i] <- 0
  }
  #add freq
  idx <- which(freq_info_df$Code_Grp == cur_group)
  if (length(idx) > 0){
    grp_info_df$Num_Pts_TOTAL[i] <- freq_info_df[idx, "Num_Pts_TOTAL"]
    grp_info_df$freq_TOTAL[i]   <- freq_info_df[idx, "freq_TOTAL"]
  }
  
}

final_df <- grp_info_df[,c(3,6, 7,8)]


write.csv(final_df, "/Users/lucasliu/Desktop/SECONDARY_DRUG_GRP.csv",row.names = F)

