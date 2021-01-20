library(icdcoder)
library(comorbidity)
library(icd)
require(stringr)
require(dplyr)
library(data.table)
convICD9to10 <- function(icd9) {
  icd9=str_pad(gsub("\\.","",icd9), width=3, pad="0") 
  icd9=data.table(icd9=icd9)
  setkey(icd9, "icd9")
  icd9 %>% 
    dplyr::left_join(icdcoder::icd9to10GEMs, by="icd9") %>% 
    dplyr::select(-flag)
}


data_dir <- "/Users/lucasliu/Desktop/"
DIAGNOSIS_df<-read.csv(paste0(data_dir,"unique_diag_codes.csv"),stringsAsFactors = F)
all_diags <- DIAGNOSIS_df$X0


converted_Code_list <- NA
for (i in 1:length(all_diags)) {
  curr_d <- all_diags[i]
  #Conversion from icd9 to 10, if not icd10
    if(i %% 1000==0) {
      # Print on the screen some message
      cat(paste0("iteration: ", i, "\n"))
    }
  
    isICD10_flag<<-is_valid(as.icd10(curr_d))
    if(isICD10_flag==F){
      converted_Code<-convICD9to10(curr_d)
      converted_Code_list[i]<-as.character(converted_Code$icd10)
    }
}

comb_df <- cbind(all_diags,converted_Code_list)
colnames(comb_df) <- c("ICD9","ICD10")
write.csv(comb_df, "ICD9toICD10.csv")



