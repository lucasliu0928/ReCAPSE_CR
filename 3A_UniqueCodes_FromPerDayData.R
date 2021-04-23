#This script cleaned unique diag/proc/drug codes from per day data in claims
#Since codes in perData data are clean, thus the resulting unique codes are also clean


proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))


data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


################################################################################ 
#### Load combined per day data
################################################################################ 
Comb_perday_df <- read.csv(paste0(data_dir, "Comb_PerDay_data.csv"),stringsAsFactors = F)
Comb_perday_df <- Comb_perday_df[,-1]

################################################################################ 
#####Get ALl unique diag,procedure, drug codes
################################################################################ 
unique_Diag_Codes_list <- list()
unique_Proc_Codes_list <- list()
unique_Drug_Codes_list <- list()
for (i in 1:nrow(Comb_perday_df)){
  if (i %% 100000==0){ print(i)}
  
  unique_Diag_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Diag_Codes"],split = "$$$$",fixed = T))
  unique_Proc_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Proc_Codes"],split = "$$$$",fixed = T))
  unique_Drug_Codes_list[[i]] <- unlist(strsplit(Comb_perday_df[i,"Drug_Codes"],split = "$$$$",fixed = T))
}

unique_Diag_codes <- unique(unlist(unique_Diag_Codes_list)) #30047 diag codes
unique_Proc_codes <- unique(unlist(unique_Proc_Codes_list)) #17737 procedure codes
unique_Drug_codes <- unique(unlist(unique_Drug_Codes_list)) #48943

#Remove NA
na_idxes <- which(is.na(unique_Diag_codes)==T)
unique_Diag_codes<- unique_Diag_codes[-na_idxes]

na_idxes <- which(is.na(unique_Proc_codes)==T)
unique_Proc_codes<- unique_Proc_codes[-na_idxes]

na_idxes <- which(is.na(unique_Drug_codes)==T)
unique_Drug_codes<- unique_Drug_codes[-na_idxes]

unique_Diag_codes <- as.data.frame(unique_Diag_codes)
unique_Proc_codes <- as.data.frame(unique_Proc_codes)
unique_Drug_codes <- as.data.frame(unique_Drug_codes)
nrow(unique_Diag_codes) #30046
nrow(unique_Proc_codes) #17736
nrow(unique_Drug_codes) # 48942

write.csv(unique_Diag_codes,paste0(outdir,"All_unique_Diag_codes_Cleaned.csv"),row.names = F)
write.csv(unique_Proc_codes,paste0(outdir,"All_unique_Proc_codes_Cleaned.csv"),row.names = F)

################################################################################ 
##For drug codes, get the drug name
################################################################################ 
#1. Load drug name list
drug_name_list <- read.csv(paste0(proj_dir,"/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/DrugList.csv"),stringsAsFactors = F,header = F)

##.Get drug name
unique_Drug_codes$drug_name <- NA
for (i in 1:nrow(unique_Drug_codes)){
  if (i %% 1000 == 0){
    print(i)
  }
  curr_drug_code <- unique_Drug_codes[i,"unique_Drug_codes"]
  name_idx <- which(drug_name_list[,"V1"] == curr_drug_code)
  if (length(name_idx) > 0 ){
    unique_Drug_codes[i,"drug_name"] <- drug_name_list[name_idx,"V2"]
  }
}
nrow(unique_Drug_codes) # 48942
write.csv(unique_Drug_codes,paste0(outdir,"All_unique_Drug_codes_Cleaned.csv"),row.names = F)
