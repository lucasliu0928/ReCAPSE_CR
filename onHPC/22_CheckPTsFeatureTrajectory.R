source("Recapse_Ultility.R")


#Load data
pt_ID <- "ID15938"
pt_df <- read.xlsx(paste0("/Users/lucasliu/Desktop/recapse_checkPts/", pt_ID, "_Comb_Features.xlsx"),sheet = 1)
#Add study_id and month start
original_IDs <- strsplit(as.character(pt_df$sample_id),split = "@")
pt_df$study_id    <- gsub("ID","",sapply(original_IDs, "[[", 1))
pt_df$month_start <- sapply(original_IDs, "[[", 2)

###################################################################################################
#'@TOCHECK
#For SBCE pts
#Check pred df
pred_df <- read.csv("/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/16_Performance_WithSurgPrimSite_V1_1111updated/All_DS_Performance/train_DS1/BeforeSmoothed/16_Prediction_Table_DS1_posweight0.5.csv",stringsAsFactors = F)
pred_df <- pred_df[which(grepl("15938",pred_df$sample_id) == T),]

#For SBCE pts, not have high predicted prob starting month index 12
feature_has0s_df <- data.frame(rownames(data.frame(which(colSums(pt_df[12:nrow(pt_df),61:278])==0))))

feature_changed_df <- feature_has0s_df 
colnames(feature_changed_df) <- c("Sudden_Changed_Features")
feature_changed_df$Sudden_Changed_Features <- as.character(feature_changed_df$Sudden_Changed_Features)
########################################################################################

#Find feature has sudden change, all 0s before predicted prob sharp increase
feature_changed <- NA
for (j in 61:278){
  curr_col <- pt_df[,j]
  
  increase_index <- 63
  sum_before <- sum(curr_col[1: increase_index -1 ])
  sum_after <- sum(curr_col[increase_index :length(curr_col)])
  
  if (sum_before == 0 & sum_after > 0 ){
    feature_changed[j] <- colnames(pt_df)[j]
  }
}

feature_changed_df <- as.data.frame(feature_changed[-which(is.na(feature_changed) == T)])
colnames(feature_changed_df) <- c("Sudden_Changed_Features")
feature_changed_df$Sudden_Changed_Features <- as.character(feature_changed_df$Sudden_Changed_Features)

#map code group to names
#Load group file
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/Grouped_CleanUniqueCodes/"
Diag_grp <- read.xlsx(paste0(data_dir , "Unique_Diag_And_Groups_inALLClaims.xlsx"), sheet = 1)
Proc_grp <- read.xlsx(paste0(data_dir , "Unique_Proc_And_Groups_inALLClaims.xlsx"), sheet = 1)



feature_changed_df$CODE_TYPE<- NA
feature_changed_df$CCS_CATEGORY<- NA
feature_changed_df$CCS_descrption <- NA
for (i in 1:nrow(feature_changed_df)){
  res <- unlist(strsplit(feature_changed_df[i,"Sudden_Changed_Features"],split = "_",fixed = T))
  curr_type <- res[2]
  curr_code <- res[3]
  feature_changed_df[i,"CODE_TYPE"] <- curr_type
  feature_changed_df[i,"CCS_CATEGORY"] <- curr_code
  
  if (curr_type == "DIAG"){
    curr_discrip <- find_ccs_discrption_func(Diag_grp,curr_code)
  }else if(curr_type == "PROC"){
    curr_discrip <- find_ccs_discrption_func(Proc_grp,curr_code)
  }else{
    curr_discrip <- NA
  }
  
  feature_changed_df[i,"CCS_descrption"] <- curr_discrip[1]
  
}

write.csv(feature_changed_df,paste0("/Users/lucasliu/Desktop/recapse_checkPts/",pt_ID, "feature_changed.csv"))


#Compare two pts
pt1 <- read.csv("/Users/lucasliu/Desktop/recapse_checkPts/ID13231feature_changed.csv")
pt2 <- read.csv("/Users/lucasliu/Desktop/recapse_checkPts/ID12873feature_changed.csv")

intersect(pt1$CCS_CATEGORY,pt2$CCS_CATEGORY)
