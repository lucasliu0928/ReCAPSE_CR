source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/5_Enrollment_And_Prediction_Months/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/5_Enrollment_And_Prediction_Months"

#########################################################################################################
#I. Get Medicaid 
#mon1-mon240 (from 1/2000-12/2019)
#########################################################################################################
medicaid_enrollment_df <- read.csv(paste0(data_dir,"kcr_medicaid_enroll_fb0015.csv"),stringsAsFactors = F)
#1. Get Month Columns
month_col_names <- paste0("mon", seq(1, 240,1)) 
month_col_indexes <- which(colnames(medicaid_enrollment_df) %in% month_col_names)
#2. Convert Month column names from mon1-mon240 to (1/2000-12/2019)
medicaid_enrollment_df <- convert_intCol_toDate(medicaid_enrollment_df,month_col_indexes, my("1/2000"),my("12/2019"), "1 mon")
#3. remove all NA rows (All Month columns)
medicaid_enrollment_df <- medicaid_enrollment_df[complete.cases(medicaid_enrollment_df[ ,month_col_indexes]),]

#4. Get IDs
Medicaid_IDs <- unique(medicaid_enrollment_df[,"study_id"])
length(Medicaid_IDs) #13902

#########################################################################################################
#II. Get Medicare 
#Mon1-Mon324 (1/1991-12/2017)
#GHO1-GHO324 (HMO enrollment indicator)
#########################################################################################################
Medicare_enrollment_df <- read.csv(paste0(data_dir,"kcr_medicare_enroll_fb0015.csv"),stringsAsFactors = F)
#1. Remove GHO cols
Medicare_enrollment_df <- Medicare_enrollment_df[, -which(grepl("GHO",colnames(Medicare_enrollment_df)) == T)]

#2.reorder ID column to be the 1st
Medicare_enrollment_df <- relocate(Medicare_enrollment_df,"study_id")

#3. Get Month Columns
month_col_names <- paste0("MON", seq(1, 324,1)) 
month_col_indexes <- which(colnames(Medicare_enrollment_df) %in% month_col_names)

#4. Convert Month column names from Mon1-Mon324  to (1/1991-12/2017)
Medicare_enrollment_df <- convert_intCol_toDate(Medicare_enrollment_df,month_col_indexes, my("1/1991"),my("12/2017"), "1 mon")

#5. remove all NA rows (All Month columns)
Medicare_enrollment_df <- Medicare_enrollment_df[complete.cases(Medicare_enrollment_df[ , month_col_indexes]),]

#6.Recode enrollent 1,2,3 to 1 as enrolled
for (j in 1:length(month_col_indexes)){
  curr_col_idx <- month_col_indexes[j]
  Medicare_enrollment_df[which(Medicare_enrollment_df[,curr_col_idx] > 1),curr_col_idx] <- 1
}


#6. Get IDs
Medicare_IDs <- unique(Medicare_enrollment_df[,"study_id"])
length(Medicare_IDs) #33446

#########################################################################################################
#1. Get Enrollment month  (the month date where enrollment = 1)
#########################################################################################################
analysis_Ids <- sort(unique(c(Medicaid_IDs,Medicare_IDs))) #37469

enrollment_months_list <- list(NA)
for (i in 1:length(analysis_Ids)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_Ids[i]
  
  medicaid_idx <- which(medicaid_enrollment_df[,"study_id"] == curr_id)
  medicare_idx <- which(Medicare_enrollment_df[,"study_id"] == curr_id)
  
  #Medicaid enrollment
  if (length(medicaid_idx) > 0 ){ 
    curr_medicaid_enroll_df <- medicaid_enrollment_df[medicaid_idx,3:242]
    enroll_idx <- which(curr_medicaid_enroll_df ==1)
    if (length(enroll_idx) >0){ #if any enroll
      curr_enroll_month1 <- colnames(curr_medicaid_enroll_df)[enroll_idx]
    }else{
      curr_enroll_month1 <- NA
    }
  }else{
    curr_enroll_month1 <- NA
  }
  
  
  #Medicare enrollment
  if (length(medicare_idx) > 0 ){ 
    curr_medicare_enroll_df <- Medicare_enrollment_df[medicare_idx,2:325]
    enroll_idx2 <- which(curr_medicare_enroll_df ==1)
    if (length(enroll_idx2) >0){#if any enroll
      curr_enroll_month2 <- colnames(curr_medicare_enroll_df)[enroll_idx2]
    }else{
      curr_enroll_month2 <- NA
    }
    
  }else{
    curr_enroll_month2 <- NA
  }
  
  curr_all_enroll_months <- unique(c(curr_enroll_month1,curr_enroll_month2))
  
  #remove NA months
  na_idxes <- which(is.na(curr_all_enroll_months) == T)
  if (length(na_idxes) > 0){
   curr_all_enroll_months <- curr_all_enroll_months[-na_idxes]
  }
  
  if (length(curr_all_enroll_months)>0){
     enrollment_months_list[[i]] <- data.frame("study_id" = curr_id, "Enrolled_Month"= curr_all_enroll_months)
  }else{
    enrollment_months_list[[i]] <- data.frame("study_id" = curr_id, "Enrolled_Month" = NA)
    
  }
}

all_enrollment_months_df <- do.call(rbind, enrollment_months_list)

#########################################################################################################
#2. remove no enrollment ids
#########################################################################################################
no_enrollment_idx <- which(is.na(all_enrollment_months_df[,"Enrolled_Month"]) == T)
all_enrollment_months_df <-  all_enrollment_months_df[-no_enrollment_idx,]

write.xlsx(all_enrollment_months_df,paste0(outdir,"5_enrollment_Months.xlsx"))
