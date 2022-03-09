data_df <- read.xlsx("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/10H_Selected_Grps/WithPossibleMonthsHasNoCodes/Selected_VAL2ndDrug_Unique_Grps.xlsx",sheet = 1)
data_df$Selected_Grps <- gsub("VAL_2ND_","",data_df$Selected_Grps)

grp_info_df <- read.xlsx("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Code_Groups/Val_Quan_Final SecondRoot List and NDC.xlsx",sheet = 1)
grp_info_df <- grp_info_df[!duplicated(grp_info_df[,c("SECONDARY_CLASSIFICATION")]),] #only keep unique 2nd groups

grp_info_df$INCLUDED <- NA
for (i in 1:nrow(grp_info_df)){
  cur_group <- grp_info_df$SECONDARY_CLASSIFICATION[i]
  if (cur_group %in% data_df$Selected_Grps){
    grp_info_df$INCLUDED[i] <- 1
  }else{
    grp_info_df$INCLUDED[i] <- 0
  }
}

final_df <- grp_info_df[,c(3,6)]
write.csv(final_df, "/Users/lucasliu/Desktop/SECONDARY_DRUG_GRP.csv",row.names = F)

