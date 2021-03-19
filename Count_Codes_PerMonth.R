library(lubridate)

get_code_func <-function(curr_claims,record_date_col,typeofcode_cols,curr_start,curr_end){
  curr_interval_index <- which(curr_claims[,record_date_col] >= curr_start &
                               curr_claims[,record_date_col]  < curr_end  )
  
  if (length(curr_interval_index) > 0){ #if any claims in the time window
      curr_interval_clamins <- curr_claims[curr_interval_index,]
      curr_codes <- unique(as.character(unlist(curr_interval_clamins[,typeofcode_cols])))
      if(length(curr_codes) >0){ #if any codes
        blank_code_idxes <- which(curr_codes =="") #remove blanks
        if (length(blank_code_idxes) >0){
          curr_codes <- curr_codes[-blank_code_idxes]
        }
      }else{
        curr_codes <- NA
      }
    
  }else{
    curr_codes <- NA
  }
  return(curr_codes)
}


out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0318_21/"
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"

#Load claims
health_claims<- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
health_claims$DTE_FIRST_SVC <- mdy(health_claims$DTE_FIRST_SVC)

pharm_claims<- read.csv(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv"),stringsAsFactors = F)
#convert date col
pharm_claims$DTE_FIRST_SVC <- dmy(pharm_claims$DTE_FIRST_SVC)

all_valid_month_df <- read.csv(paste0(out_dir,"all_valid_month_medicaid.csv"),stringsAsFactors = F)
analysis_IDs <- unique(all_valid_month_df$ID) #10701 patients has enrollment

n_valid_month <- NA
diag_patient_month_table_list <- list(NA)
proc_patient_month_table_list <- list(NA)
drug_patient_month_table_list <- list(NA)

for (i in 1:length(analysis_IDs)){
  if (i %% 500 == 0){
    print(i)
  }
  curr_healthclims <- health_claims[which(health_claims[,"study_id"] == curr_id),]
  curr_pharmclims <- pharm_claims[which(pharm_claims[,"study_id"] == curr_id),]
  
  curr_id <- analysis_IDs[i]
  curr_enroll_months <- all_valid_month_df$Valid_Month[which(all_valid_month_df[,"ID"] == curr_id)]
  n_valid_month[i] <- length(curr_enroll_months)
  
  #Diag table
  diag_patient_month_table <- as.data.frame(matrix(NA, nrow = n_valid_month[i],ncol = 4))
  colnames(diag_patient_month_table) <- c("ID","n_codes","Diag_codes","Month_Start_Date")
  diag_patient_month_table$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))

  proc_patient_month_table <- as.data.frame(matrix(NA, nrow = n_valid_month[i],ncol = 4))
  colnames(proc_patient_month_table) <- c("ID","n_codes","Proc_codes","Month_Start_Date")
  proc_patient_month_table$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  drug_patient_month_table <- as.data.frame(matrix(NA, nrow = n_valid_month[i],ncol = 4))
  colnames(drug_patient_month_table) <- c("ID","n_codes","Drug_codes","Month_Start_Date")
  drug_patient_month_table$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  for (j in 1:length(curr_enroll_months)){
     curr_mon_start <- ymd(curr_enroll_months[j]) 
     curr_mon_end <- curr_mon_start + months(1)
     
     diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
     proc_cols <- c("CDE_PROC_PRIM")


     #get diag unique codes in this interval
     curr_diags <- get_code_func(curr_healthclims,'DTE_FIRST_SVC',diag_cols,
                                 curr_mon_start,curr_mon_end)
     diag_patient_month_table$Month_Start_Date[j] <- as.character(curr_mon_start)
     diag_patient_month_table$n_codes[j] <- length(curr_diags)
     diag_patient_month_table$Diag_codes[j] <- paste0(curr_diags,collapse = "$$$$$")
     
     #get proc unique codes in this interval
     curr_proc <- get_code_func(curr_healthclims,'DTE_FIRST_SVC',proc_cols,
                                curr_mon_start,curr_mon_end)
     proc_patient_month_table$Month_Start_Date[j] <- as.character(curr_mon_start)
     proc_patient_month_table$n_codes[j] <- length(curr_proc)
     proc_patient_month_table$Proc_codes[j] <- paste0(curr_proc,collapse = "$$$$$")
     
     #Get drug codes
     durg_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")
     curr_durgs <- get_code_func(curr_pharmclims,'DTE_FIRST_SVC',durg_cols,
                   curr_mon_start,curr_mon_end)
     
     drug_patient_month_table$Month_Start_Date[j] <- as.character(curr_mon_start)
     drug_patient_month_table$n_codes[j] <- length(curr_durgs)
     drug_patient_month_table$Drug_codes[j] <- paste0(curr_durgs,collapse = "$$$$$")

     
     
  }
  
  diag_patient_month_table_list[[i]] <- diag_patient_month_table
  proc_patient_month_table_list[[i]] <- proc_patient_month_table
  drug_patient_month_table_list[[i]] <- drug_patient_month_table
  
}


#Report averge valid month for each patients
mean(n_valid_month)
qplot(n_valid_month, geom="histogram",bins = 50)



#Report total number of patient-month samples
all_diag_patinets_month <- do.call(rbind,diag_patient_month_table_list)
all_proc_patinets_month <- do.call(rbind,proc_patient_month_table_list)
all_drug_patinets_month <- do.call(rbind,drug_patient_month_table_list)

nrow(all_diag_patinets_month) 
nrow(all_proc_patinets_month)
nrow(all_drug_patinets_month)

#Report all unique diag, proc, and drug codes
all_diag_codes <- unlist(strsplit(all_diag_patinets_month$Diag_codes, "$$$$$",fixed = TRUE))
all_diag_codes <- all_diag_codes[-which(is.na(all_diag_codes)==T | all_diag_codes == "NA")]
length(unique(all_diag_codes)) #N of unique diag codes

all_Proc_codes <- unlist(strsplit(all_proc_patinets_month$Proc_codes, "$$$$$",fixed = TRUE))
all_Proc_codes <- all_Proc_codes[-which(is.na(all_Proc_codes)==T | all_Proc_codes == "NA")]
length(unique(all_Proc_codes)) #N of unique proc codes

all_drug_codes <- unlist(strsplit(all_drug_patinets_month$Drug_codes, "$$$$$",fixed = TRUE))
all_drug_codes <- all_drug_codes[-which(is.na(all_drug_codes)==T | all_drug_codes == "NA")]
length(unique(all_drug_codes)) #N of unique proc codes

#Report average number of codes per month
mean(all_diag_patinets_month$n_codes)
mean(all_proc_patinets_month$n_codes)
mean(all_drug_patinets_month$n_codes)

#'@Question3: what to do with the patinet month which has no dia/proc/drug codes, still keep them?
#'@Question4: what to do witht the patients has no successive enrollment months?
write.csv(all_diag_patinets_month,paste0(out_dir,"all_diag_patinets_month_medicaid.csv"),row.names = F)
write.csv(all_proc_patinets_month,paste0(out_dir,"all_proc_patinets_month_medicaid.csv"),row.names = F)
write.csv(all_drug_patinets_month,paste0(out_dir,"all_drug_patinets_month_medicaid.csv"),row.names = F)


