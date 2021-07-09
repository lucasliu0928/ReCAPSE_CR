source("Recapse_Ultility.R")

#onHPC
grp_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/Code_Groups/"
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

#local
grp_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/Code_Groups/"
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


################################################################################ 
#1. Load all per month with char data
################################################################################ 
All_data <- read.xlsx(paste0(data_dir,"10_All_PerMonthData_WithMonthChar_df.xlsx"))


################################################################################ 
##2. Unique Codes in entire data
################################################################################ 
all_unique_diag_codes <- split_code_strings(All_data,"Diag_Codes")
all_unique_diag_codes_df <- as.data.frame(all_unique_diag_codes)
colnames(all_unique_diag_codes_df) <- "Unique_Diag_Codes"

all_unique_proc_codes <- split_code_strings(All_data,"Proc_Codes")
all_unique_proc_codes_df <- as.data.frame(all_unique_proc_codes)
colnames(all_unique_proc_codes_df) <- "Unique_Proc_Codes"

all_unique_drug_codes <- split_code_strings(All_data,"Drug_Codes")
all_unique_drug_codes_df <- as.data.frame(all_unique_drug_codes)
colnames(all_unique_drug_codes_df) <- "Unique_Drug_Codes"


########################################################################   
######                3. load HCUP CCS file:                      ########
########################################################################   
HCUP_Diag1_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-9.diag_ref.edit.csv"),stringsAsFactors = F)
HCUP_Diag2_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-10.diag_ref.edit.csv"),stringsAsFactors = F)
HCUP_Proc1_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-9.proc_ref.edit.csv"),stringsAsFactors = F)
HCUP_Proc2_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-10.proc_ref.edit.csv"),stringsAsFactors = F)

#Change col name to comb
colnames(HCUP_Diag1_df)[which(colnames(HCUP_Diag1_df) == "ICD.9.CM.CODE")] <- "Code"
colnames(HCUP_Diag2_df)[which(colnames(HCUP_Diag2_df) == "ICD.10.CM.CODE")] <- "Code"
colnames(HCUP_Proc1_df)[which(colnames(HCUP_Proc1_df) == "ICD.9.CM.CODE")] <- "Code"
colnames(HCUP_Proc2_df)[which(colnames(HCUP_Proc2_df) == "ICD.10.PCS.CODE")] <- "Code"

HCUP_Diag1_df$CODE_TYPE <- "ICD9_Diag"
HCUP_Diag2_df$CODE_TYPE <- "ICD10_Diag"
HCUP_Proc1_df$CODE_TYPE <- "ICD9_Proc"
HCUP_Proc2_df$CODE_TYPE <- "ICD10_Proc"

HCUP_comb <- rbind(HCUP_Diag1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Diag2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Proc1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Proc2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")])

clean_code_func <-function(list_of_codes){
  #list_of_codes <- HCUP_comb[,"Code"]
  
  #1.omitting any codes with non-alphanumeric characters,
  updated_list_of_codes<- gsub("[^[:alnum:]]", " ", list_of_codes)
  
  #2. space
  updated_list_of_codes <- trimws(updated_list_of_codes, which = c("both"), whitespace = "[ \t\r\n]")
  
  #3.decimal
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
  #.if it is non-numeric, then exclude codes 
  # if it is numeric , then prepending '0'
  l3_idxes <- which(n_char< 3)
  l3_codes <- updated_list_of_codes[l3_idxes]
  if(length(l3_codes) > 0){
    updated_code <- NA
    for (c in 1:length(l3_codes)){
      cur_code <- l3_codes[c]
      if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric
        updated_code[c] <- NA #remove
      }else{ #it is numeric , then prepending '0'
        updated_code[c] <- paste0("0",cur_code)
      }
    }
    
    updated_list_of_codes[l3_idxes] <- updated_code
    
    #This might result in NAs(from converting to numeric when it is char) from orignal 
  }
  
  return(updated_list_of_codes)
}


HCUP_comb[,"CCS.CATEGORY"]  <- clean_code_func(HCUP_comb[,"CCS.CATEGORY"]) #Clean category in HCUP

################################################################################ 
##3. Unique Codes in entire data
################################################################################
