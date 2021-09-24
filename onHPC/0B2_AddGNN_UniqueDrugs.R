source("Recapse_Ultility.R")

#######################################################################
##############              Data dir                     ############## 
#######################################################################
#on HPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/NDC Drug List/"
data_dir2 <-"/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/recapse/data/"
outdir   <- "/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"

#local
data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/NDC Drug List/"
data_dir2<- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"
grping_data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/"

outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"

#######################################################################
#1. Read Drug name list
#######################################################################
options(scipen=999) #disable scientific numbers showing
drug_name_list1 <- read.csv(paste0(data_dir,"Medicaid DrugList.csv"), stringsAsFactors = F)
colnames(drug_name_list1) <- c("CODE","GNN")

drug_name_list2 <- read.csv(paste0(data_dir,"Medicare DrugList.csv"), stringsAsFactors = F)
colnames(drug_name_list2) <- c("CODE","BN","GNN")

#Comb two source
drug_name_list_comb <- rbind(drug_name_list1, drug_name_list2[,c("CODE","GNN")])
#remove duplicates
drug_name_list_comb_updated <- drug_name_list_comb[!duplicated(drug_name_list_comb[,"CODE"]),]
#remove leading zeros 
drug_name_list_comb_updated[,"CODE"] <- str_remove(drug_name_list_comb_updated[,"CODE"], "^0+")

#######################################################################
#2.Load unique drug in claims
#######################################################################
#Load  data
unique_drug_df <- read.xlsx(paste0(data_dir2,"0_Cleaned_Unique_Drug_Codes.xlsx"),sheet = 1)

##remove leading zeros
unique_drug_df$CODE_ForMatchGNNData<- str_remove(unique_drug_df$CODE, "^0+")

#######################################################################
#3.Add GNN to cleaned unique drug codes
#######################################################################
unique_drug_df$GNN <- NA
for (i in 1:nrow(unique_drug_df)){
  if (i %% 1000 == 0){print(i)}
  curr_code  <- unique_drug_df[i,"CODE_ForMatchGNNData"]
  curr_index <- which(drug_name_list_comb_updated[,"CODE"] == curr_code)
  if (length(curr_index) > 0 ){
    unique_drug_df[i,"GNN"] <- drug_name_list_comb_updated[curr_index,"GNN"]
  }
}

#Two DRUG_NDC has no GNN, and  469 DRUG_THERA_CLS_AHFS  has no GNN
check_df <- unique_drug_df[which(is.na(unique_drug_df$GNN) == T),]

#######################################################################
#4. Create short GNN
#'@Question:  what is exclude list
#######################################################################
get_first_word_of_name <- function(str_vec,exclude_list){
  str_vec <- as.character(str_vec)
  
  # get the first term or word of a multi-word generic name
  str_pair <- str_split(trimws(str_vec, which='both'), ',', simplify=TRUE)
  
  if (str_pair[,1] %in% exclude_list)
  {
    str_pair[,1] <- trimws(str_pair[,2], which='both')
  }
  
  str_pair <- str_split(str_pair[,1], ' ', simplify=TRUE)
  
  if (ncol(str_pair) > 1 & (str_pair[,1] %in% exclude_list))
  {
    str_pair[,1] <- trimws(str_pair[,2], which='both')
  }
  
  str_pair <- str_split(str_pair[,1], '/', simplify=TRUE)
  
  return (str_pair[,1])
}

exclude_list <- NULL
unique_drug_df$short_GNN <- NA
for (i in 1:nrow(unique_drug_df)){
  if (i %% 1000 == 0){print(i)}
  curr_gnn       <- unique_drug_df[i,"GNN"]
  curr_short_gnn <- get_first_word_of_name(curr_gnn,exclude_list)
  unique_drug_df[i,"short_GNN"] <- curr_short_gnn
}



#######################################################################
#5.Load drug group, and add group info 
#'@Note this section will be added 0C later
#######################################################################
group_drugcodes_into_DM3_funcV2 <- function(claim_code_df,DM3_df){
  claim_code_df$specific_group <- NA
  claim_code_df$general_group <- NA
  for (i in 1:nrow(claim_code_df)){
    if (i %% 1000 == 0){print(i)}
    curr_gnn <- claim_code_df[i,"short_GNN"]
    curr_idxes <- which(DM3_df[,"short_code"] == curr_gnn)
    if (length(curr_idxes) > 0){
      claim_code_df[i,"specific_group"] <-  unique(DM3_df[curr_idxes,"specific_group"])[1] #if there is still multiple, choose the 1st one
      claim_code_df[i,"general_group"]  <-  unique(DM3_df[curr_idxes,"general_group"])[1]
    }
  }
  return(claim_code_df)
}


DM3_df <- load_and_clean_DM3_data(grping_data_dir)
grouped_unique_drug_df <- group_drugcodes_into_DM3_funcV2(unique_drug_df,DM3_df)
write.xlsx(grouped_unique_drug_df,paste0(outdir,"0_Updated_Cleaned_Unique_Drug_Codes.xlsx"))

#Report grps
report_code_grps_func <- function(in_data, grp_name){
  # in_data <- grouped_unique_diag_df
  # grp_name <- "CCS_CATEGORY"
  
  #Get total number of unique codes
  n_codes <- nrow(in_data)
  
  #Get number of codes have groups
  has_grp_indexes <- which(is.na(in_data[,grp_name])==F)
  n_codes_hasgrps <- length(has_grp_indexes)
  
  #Number of codes have no groups
  n_codes_NOgrps <-  n_codes - n_codes_hasgrps
  
  #Number of groups
  n_grps <-  length(unique(in_data[has_grp_indexes,grp_name]))
  
  
  
  grp_stats <- cbind.data.frame(n_codes,n_codes_hasgrps,n_codes_NOgrps,n_grps)
  
  return(grp_stats)
}
DM3_stats1 <- report_code_grps_func(grouped_unique_drug_df,"specific_group")
DM3_stats2 <- report_code_grps_func(grouped_unique_drug_df,"general_group")
Drug_stats <- rbind(DM3_stats1,DM3_stats2)
rownames(Drug_stats) <- c("DM3_specific","DM3_general")
write.xlsx(Drug_stats,paste0(outdir,"0_Updated_Drug_Group_stats.xlsx"))

#Check
length(intersect(DM3_df$short_code,grouped_unique_drug_df$short_GNN)) #359
length(unique(grouped_unique_drug_df$short_GNN))  #unique short_GNN: 4180
length(unique(DM3_df$short_code))                 #unique short_GNN: 420
