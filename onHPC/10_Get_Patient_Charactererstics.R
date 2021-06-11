library(openxlsx)
library(lubridate)
get_DAJCC_var_funtion <- function(kcr_data, pathology_results_col,clinical_results_col){
  #Rules : consider the values from 'TNMPathT' first (which is pathology results), 
  #       if TNMPathT is in value of '88' or 'pX' (unknown) then you check the value from 'TNMClinT' (clinical diagnosis results
  
  #pathology_results_col <- "TNMPathT"
  #clinical_results_col <- "TNMClinT"
  
  computed_value <- NA
  for (i in 1:nrow(kcr_data)){
    curr_res <- kcr_data[i,pathology_results_col]
    if (curr_res %in% c("88","pX","") | is.na(curr_res) == T){ #if pathology is 88 or pX or NA
      curr_res <- kcr_data[i,clinical_results_col]
      if (curr_res %in% c("88","pX","") | is.na(curr_res) == T){ #if clinical is 88 or pX or NA
        curr_res <- NA
      }
      
    }
    computed_value[i] <- curr_res
  }
  
  return(computed_value)
}

data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"


#########################################################################################################
####1. Load outcome/event type data
#########################################################################################################
updated_All_event_df<- read.csv(paste0(outdir,"updated_All_event_df.csv"),stringsAsFactors = F)
length(unique(updated_All_event_df[,"ID"])) # 40329

#########################################################################################################
### 2.  Load patinet char data
#########################################################################################################
kcr_data <- read.csv(paste0(data_dir, "uh3_kcrdata.csv"),stringsAsFactors = F)
#Compute DAJCC_T, DAJCC_M, DAJCC_N
kcr_data$DAJCC_T <- get_DAJCC_var_funtion(kcr_data,"TNMPathT","TNMClinT")
kcr_data$DAJCC_M <- get_DAJCC_var_funtion(kcr_data,"TNMPathM","TNMClinM")
kcr_data$DAJCC_N <- get_DAJCC_var_funtion(kcr_data,"TNMPathN","TNMClinN")
