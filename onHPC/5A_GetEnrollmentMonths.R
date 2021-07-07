library(lubridate)
library(dplyr)
library(openxlsx)
####Functions
#1.Convert column integer names to acutal date
convert_intCol_toDate <- function(enroll_df,month_col_indexes, min_date,max_date, date_unit){
  dates_seq <- seq(min_date,max_date,by = date_unit)
  colnames(enroll_df)[month_col_indexes] <- as.character(dates_seq)
  return(enroll_df)
}

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


# #local
# data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

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
#III. Get Enrollment month  (the month date where enrollment = 1)
#########################################################################################################
analysis_Ids <- sort(unique(c(Medicaid_IDs,Medicare_IDs))) #37469

enrollment_start_end_df <- as.data.frame(matrix(NA, nrow = length(analysis_Ids), ncol = 3))
colnames(enrollment_start_end_df) <- c("study_id" , "Enroll_Start","Enroll_End")

options(warn = 2)
for (i in 1:length(analysis_Ids)){
  if (i %% 1000 == 0){print(i)}
  curr_id <- analysis_Ids[i]
  
  medicaid_idx <- which(medicaid_enrollment_df[,"study_id"] == curr_id)
  medicare_idx <- which(Medicare_enrollment_df[,"study_id"] == curr_id)
  
  if (length(medicaid_idx) > 0 & length(medicare_idx) > 0 ){ #if in both
    curr_medicaid_enroll_df <- medicaid_enrollment_df[medicaid_idx,3:242]
    enroll_idx <- which(curr_medicaid_enroll_df ==1)
    if (length(enroll_idx) >0){ #if any enroll
      start1 <- min(ymd(colnames(curr_medicaid_enroll_df)[enroll_idx]))
      Stop1 <- max(ymd(colnames(curr_medicaid_enroll_df)[enroll_idx]))
    }else{
      start1 <- NA
      Stop1 <- NA
    }
    
    curr_medicare_enroll_df <- Medicare_enrollment_df[medicare_idx,2:325]
    enroll_idx2 <- which(curr_medicare_enroll_df ==1)
    if (length(enroll_idx2) >0){#if any enroll
      start2 <- min(ymd(colnames(curr_medicare_enroll_df)[enroll_idx2]))
      Stop2 <- max(ymd(colnames(curr_medicare_enroll_df)[enroll_idx2]))
    }else{
      start2 <- NA
      Stop2 <- NA
    }
    
    start <- min(start1,start2,na.rm = T)
    stop <- max(Stop1,Stop2,na.rm = T)
  }else if(length(medicaid_idx) > 0){ #if in medicaid only
    curr_medicaid_enroll_df <- medicaid_enrollment_df[medicaid_idx,3:242]
    enroll_idx <- which(curr_medicaid_enroll_df ==1)
    if (length(enroll_idx) >0){#if any enroll
      start <- min(ymd(colnames(curr_medicaid_enroll_df)[enroll_idx]))
      stop <- max(ymd(colnames(curr_medicaid_enroll_df)[enroll_idx]))
    }else{
      start <- NA
      stop <- NA
    }
  }else if(length(medicare_idx) > 0){# if in medicare  only
    curr_medicare_enroll_df <- Medicare_enrollment_df[medicare_idx,2:325]
    enroll_idx2 <- which(curr_medicare_enroll_df ==1)
    if (length(enroll_idx2) >0){#if any enroll
      start <- min(ymd(colnames(curr_medicare_enroll_df)[enroll_idx2]))
      stop <- max(ymd(colnames(curr_medicare_enroll_df)[enroll_idx2]))
    }else{
      start <- NA
      stop <- NA
    }
  }else{
    start <- NA
    stop <- NA
  }

  enrollment_start_end_df[i,"study_id"] <- curr_id
  enrollment_start_end_df[i,"Enroll_Start"] <- as.character(start)
  enrollment_start_end_df[i,"Enroll_End"] <- as.character(stop)
}

#remove no enrollment ids
no_enrollment_idx <- which(is.na(enrollment_start_end_df$Enroll_Start)==T & is.na(enrollment_start_end_df$Enroll_End)==T)
enrollment_start_end_df <-  enrollment_start_end_df[-no_enrollment_idx,]

write.xlsx(enrollment_start_end_df,paste0(outdir,"4_enrollment_start_end_df.xlsx"))
