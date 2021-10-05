source("Recapse_Ultility.R")

#######################################################################
##############              Data dir                     ############## 
#######################################################################
data_dir <- "/recapse/intermediate_data/0_Codes/BeforeClean_UniqueCodes/"
drug_name_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/recapse/intermediate_data/0_Codes/AfterClean_UniqueCodes/"

# #local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/BeforeClean_UniqueCodes/"
drug_name_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir   <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/0_Codes/AfterClean_UniqueCodes/"
#######################################################################
############################## Medicaid  ############################## 
#######################################################################
Diag_code1 <- read.xlsx(paste0(data_dir, "0_unique_Diag_Codes_Medicaid.xlsx"),sheet = 1)
Proc_code1 <- read.xlsx(paste0(data_dir, "0_unique_Proc_Codes_Medicaid.xlsx"),sheet = 1)
Drug_code1 <- read.xlsx(paste0(data_dir, "0_unique_Drug_Codes_Medicaid.xlsx"),sheet = 1)


#######################################################################
############################## Medicare  ############################## 
#######################################################################
Diag_code2 <- read.xlsx(paste0(data_dir, "0_unique_Diag_Codes_Medicare.xlsx"),sheet = 1)
Proc_code2 <- read.xlsx(paste0(data_dir, "0_unique_Proc_Codes_Medicare.xlsx"),sheet = 1)
Drug_code2 <- read.xlsx(paste0(data_dir, "0_unique_Drug_Codes_Medicare.xlsx"),sheet = 1)



##############################################################################
###If codes both in medcaid and medicare, change the CLAIMS source after combine
##############################################################################
common_diag_codes <- intersect(Diag_code1[,"CODE"],Diag_code2[,"CODE"])
common_proc_codes <- intersect(Proc_code1[,"CODE"],Proc_code2[,"CODE"])
common_drug_codes <- intersect(Drug_code1[,"CODE"],Drug_code1[,"CODE"])

#######################################################################
############################## Combine   ############################## 
#######################################################################
#Combine
Comb_diag <- rbind(Diag_code1,Diag_code2)
Comb_proc <- rbind(Proc_code1,Proc_code2)
Comb_drug <- rbind(Drug_code1,Drug_code2)

#If codes both in medcaid and medicare, change the CLAIMS source after combine
Comb_diag[which(Comb_diag[,"CODE"] %in% common_diag_codes),"CLAIM"] <- "BOTH"
Comb_proc[which(Comb_proc[,"CODE"] %in% common_proc_codes),"CLAIM"] <- "BOTH"
Comb_drug[which(Comb_drug[,"CODE"] %in% common_drug_codes),"CLAIM"] <- "BOTH"

#remove duplicates
Comb_diag <- Comb_diag[!duplicated(Comb_diag[,"CODE"]),] #remove duplicates
Comb_proc <- Comb_proc[!duplicated(Comb_proc[,"CODE"]),] #remove duplicates
Comb_drug <- Comb_drug[!duplicated(Comb_drug[,"CODE"]),] #remove duplicates

#remove the predix of code 
Comb_diag[,"CODE"] <- gsub("CODE_","",Comb_diag[,"CODE"])
Comb_proc[,"CODE"] <- gsub("CODE_","",Comb_proc[,"CODE"])
Comb_drug[,"CODE"] <- gsub("CODE_","",Comb_drug[,"CODE"])

#######################################################################
## Clean codes
#'@NOTE: Warinigs are OK. (From checking if a code is num or char)
#######################################################################
Comb_diag_cleaned <- Comb_diag
Comb_proc_cleaned <- Comb_proc
Comb_drug_cleaned <- Comb_drug

#Reformat
Comb_diag_cleaned[,"CODE"] <- clean_code_func2(Comb_diag_cleaned[,"CODE"],Comb_diag_cleaned[,"TYPE"])
Comb_proc_cleaned[,"CODE"] <- clean_code_func2(Comb_proc_cleaned[,"CODE"],Comb_proc_cleaned[,"TYPE"])
Comb_drug_cleaned[,"CODE"] <- clean_code_func2(Comb_drug_cleaned[,"CODE"],Comb_drug_cleaned[,"TYPE"])


#Remove duplicate and NAs after reformat
Comb_diag_cleaned <- Comb_diag_cleaned[!duplicated(Comb_diag_cleaned[,"CODE"]),]
Comb_proc_cleaned <- Comb_proc_cleaned[!duplicated(Comb_proc_cleaned[,"CODE"]),]
Comb_drug_cleaned <- Comb_drug_cleaned[!duplicated(Comb_drug_cleaned[,"CODE"]),]

Comb_diag_cleaned <- Comb_diag_cleaned[-which(is.na(Comb_diag_cleaned[,"CODE"])==T | Comb_diag_cleaned[,"CODE"]==""),]
Comb_proc_cleaned <- Comb_proc_cleaned[-which(is.na(Comb_proc_cleaned[,"CODE"])==T | Comb_proc_cleaned[,"CODE"] ==""),]
Comb_drug_cleaned <- Comb_drug_cleaned[-which(is.na(Comb_drug_cleaned[,"CODE"])==T | Comb_drug_cleaned[,"CODE"] ==""),]


#write.xlsx(Comb_diag_cleaned,paste0(outdir,"0_Cleaned_Unique_Diag_Codes.xlsx"))
#write.xlsx(Comb_proc_cleaned,paste0(outdir,"0_Cleaned_Unique_Proc_Codes.xlsx"))

#######################################################################
## Add Drug names
#######################################################################
#1. Get drung names
drug_name_df <- read.csv(paste0(drug_name_dir,"DrugList.csv"),stringsAsFactors = F,header = F)
drug_name_df$V1 <- as.character(drug_name_df$V1)

#2.clean code in drug name df
drug_name_df[,"V1"] <- clean_code_func2(drug_name_df[,"V1"],drug_name_df[,"V2"])

#3. Clean drug names
drug_name_df[,"V2"] <- gsub("[[:punct:]]","",drug_name_df[,"V2"])
drug_name_df[,"V2"] <- trimws(drug_name_df[,"V2"], which = c("both"), whitespace = "[ \t\r\n]")



#3.Filter out durg name df for code in claims
drug_name_df <- drug_name_df[which(drug_name_df[,"V1"] %in% Comb_drug_cleaned[,"CODE"]),] 

#4. Add drug name to Comb_drug_cleaned
Comb_drug_cleaned$DRUG_NAME <- NA

for (i in 1:nrow(Comb_drug_cleaned)){
  if (i %% 1000 == 0 ){print(i)}
  curr_drug <- Comb_drug_cleaned[i,"CODE"]
  curr_idxes <- which(drug_name_df[,"V1"] == curr_drug)
  if (length(curr_idxes) > 0){
    Comb_drug_cleaned[i,"DRUG_NAME"] <- drug_name_df[curr_idxes,"V2"]
  }
}



#######################################################################
#Add GNN (Added Sep24 21)
#######################################################################
#1. Read Drug name list
options(scipen=999) #disable scientific numbers showing
drug_name_list1 <- read.csv(paste0(drug_name_dir,"NDC Drug List/Medicaid DrugList.csv"), stringsAsFactors = F)
colnames(drug_name_list1) <- c("CODE","GNN")

drug_name_list2 <- read.csv(paste0(drug_name_dir,"NDC Drug List/Medicare DrugList.csv"), stringsAsFactors = F)
colnames(drug_name_list2) <- c("CODE","BN","GNN")


drug_name_list_comb <- rbind(drug_name_list1, drug_name_list2[,c("CODE","GNN")]) #Comb two source
drug_name_list_comb_updated <- drug_name_list_comb[!duplicated(drug_name_list_comb[,"CODE"]),] #remove duplicates
drug_name_list_comb_updated[,"CODE"] <- str_remove(drug_name_list_comb_updated[,"CODE"], "^0+") #remove leading zeros 


#2.Create a column for Code used for matching GNN data (remove leading zeros)
Comb_drug_cleaned$CODE_ForMatchGNNData<- str_remove(Comb_drug_cleaned$CODE, "^0+") #remove leading zeros

#3.Add GNN to cleaned unique drug codes
Comb_drug_cleaned$GNN <- NA
for (i in 1:nrow(Comb_drug_cleaned)){
  if (i %% 1000 == 0){print(i)}
  curr_code  <- Comb_drug_cleaned[i,"CODE_ForMatchGNNData"]
  curr_index <- which(drug_name_list_comb_updated[,"CODE"] == curr_code)
  if (length(curr_index) > 0 ){
    Comb_drug_cleaned[i,"GNN"] <- drug_name_list_comb_updated[curr_index,"GNN"]
  }
}

#check_df <- Comb_drug_cleaned[which(is.na(Comb_drug_cleaned$GNN) == T),] #Two DRUG_NDC has no GNN, and  469 DRUG_THERA_CLS_AHFS  has no GNN


#4. Create short GNN     # This functions is from Teresa   
#'@Question:  what is exclude list
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
Comb_drug_cleaned$short_GNN <- NA
for (i in 1:nrow(Comb_drug_cleaned)){
  if (i %% 1000 == 0){print(i)}
  curr_gnn       <- Comb_drug_cleaned[i,"GNN"]
  curr_short_gnn <- get_first_word_of_name(curr_gnn,exclude_list)
  Comb_drug_cleaned[i,"short_GNN"] <- curr_short_gnn
}

write.xlsx(Comb_drug_cleaned,paste0(outdir,"0_Cleaned_Unique_Drug_Codes.xlsx"))
