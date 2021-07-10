library(openxlsx)
library(lubridate)
source("Recapse_Ultility.R")
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


#local
raw_data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
per_month_data_dir <- "/Users/lucasliu/Desktop/intermediate_data/6_perMonthData_inValidMonth_perPatientData_V2_nonuniquecodes/"
perday_dir <- "/Users/lucasliu/Desktop/intermediate_data/3_perDay_PerPatientData/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"



kcr_data <- read.csv(paste0(raw_data_dir, "uh3_kcrdata.csv"),stringsAsFactors = F)
#Compute DAJCC_T, DAJCC_M, DAJCC_N
kcr_data$DAJCC_T <- get_DAJCC_var_funtion(kcr_data,"TNMPathT","TNMClinT")
kcr_data$DAJCC_M <- get_DAJCC_var_funtion(kcr_data,"TNMPathM","TNMClinM")
kcr_data$DAJCC_N <- get_DAJCC_var_funtion(kcr_data,"TNMPathN","TNMClinN")

kcr_data[which(kcr_data=="",arr.ind = T)] <- NA
missing_table <- get_missing_rate_table(kcr_data,colnames(kcr_data))


#After exlcusion of SEERSummStg2000 NA
exlucde_indxes <- which(is.na(kcr_data$SEERSummStg2000)==T | kcr_data$SEERSummStg2000 == "")
kcr_data2 <- kcr_data[-exlucde_indxes,]
nrow(kcr_data2) # 12021
missing_table2 <- get_missing_rate_table(kcr_data2,colnames(kcr_data2))

table(kcr_data$BestStageGrp)
#After exlcusion of BestStageGrp  stage unkown or 0  or na
exlucde_indxes <- which(kcr_data$BestStageGrp %in% c(0,1,2,seq(70,79,1),88,99) | is.na(kcr_data$BestStageGrp)==T) #2788
kcr_data3 <- kcr_data[-exlucde_indxes,]
nrow(kcr_data3) # 43358
missing_table3 <- get_missing_rate_table(kcr_data3,colnames(kcr_data3))
