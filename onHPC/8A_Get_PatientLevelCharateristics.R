source("Recapse_Ultility.R")
get_DAJCC_var_funtion <- function(kcr_data, pathology_results_col,clinical_results_col){
  #Rules : consider the values from 'TNMPathT' first (which is pathology results), 
  #       if TNMPathT is in value of '88' or 'pX' (unknown) then you check the value from 'TNMClinT' (clinical diagnosis results
  
  # pathology_results_col <- "TNMPathT"
  # clinical_results_col <- "TNMClinT"
  # 
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

#########################################################################################################
#Data dir
#########################################################################################################
#onHPC
raw_data_dir  <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/recapse/intermediate_data/"

#local
raw_data_dir  <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithPossibleMonthsHasNoCodes/")
data_dir2  <- paste0(proj_dir, "7_PrePostLabels_AndAvailibility6mon/A_PrePost_Labels/EnrolledMonths_WithEveryMonthsHasCodes/")
data_dir3 <- paste0(proj_dir, "4_RecurrDates_Outcome_Info/")
outdir   <- paste0(proj_dir, "8_Characteristics/Patient_Level/")

# #########################################################################################################
# #onHPC
# per_month_data_dir <- "/recapse/intermediate_data/6_perMonthData_inValidMonth_perPatientData_V2_nonuniquecodes/"
# perday_dir <- "/recapse/intermediate_data/3_perDay_PerPatientData/"
# outdir <- "/recapse/intermediate_data/"
# 
# #local
# per_month_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_perMonthData_inValidMonth_perPatientData_V2_nonuniquecodes/"
# perday_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3_perDay_PerPatientData/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


#########################################################################################################
####1. Load outcome/event type data
#########################################################################################################
updated_All_event_df <- read.xlsx(paste0(data_dir,"4_updated_All_event_df.xlsx"),sheet = 1)

#########################################################################################################
### 2.  Load patinet char data
#########################################################################################################
kcr_data <- read.csv(paste0(raw_data_dir, "uh3_kcrdata.csv"),stringsAsFactors = F)

#Feature Missing_N_Perc
#1        TNMPathM   49244 (100%)
#2        TNMClinM   49246 (100%)
#3 SEERSummStg2000 37225 (75.59%)

kcr_data[which(kcr_data$TNMPathM == ""),"TNMPathM"] <- NA
kcr_data[which(kcr_data$TNMClinM == ""),"TNMClinM"] <- NA
get_missing_rate_table(kcr_data,c("TNMPathM","TNMClinM","SEERSummStg2000"))

#Add updated columns to kcr_data
new_kcr_data <- read.sas7bdat(paste0(raw_data_dir, "ky0015_update_DerivedSS2000_andTNM.sas7bdat"),debug = FALSE)
new_kcr_data <- new_kcr_data[match(new_kcr_data[,c("study_id")],kcr_data[,c("study_id")]),]
new_kcr_data[which(new_kcr_data$TNMPathM == ""),"TNMPathM"] <- NA
new_kcr_data[which(new_kcr_data$TNMClinM == ""),"TNMClinM"] <- NA

#Update 
kcr_data$TNMPathM <- as.character(new_kcr_data$TNMPathM)
kcr_data$TNMClinM <- as.character(new_kcr_data$TNMClinM)
kcr_data$DerivedSS2000 <- new_kcr_data$DerivedSS2000


#'SEERSummStg2000’ only captures SEER summary stage from the years 2001-2003. 
# ‘DerivedSS2000’ was also added for the summary stage between the years 2004-2015. 
#No such information was captured for the year 2000.
kcr_data$Comb_SEERSummStg <- NA
#for not missing original SEER stage, use "SEERSummStg2000
NOTmissing_idxes <- which(is.na(kcr_data[,"SEERSummStg2000"])== F)
kcr_data[NOTmissing_idxes,"Comb_SEERSummStg"] <- kcr_data[NOTmissing_idxes,"SEERSummStg2000"]
#formissing original SEER stage, use "DerivedSS2000
missing_idxes    <- which(is.na(kcr_data[,"SEERSummStg2000"])== T)
kcr_data[missing_idxes,"Comb_SEERSummStg"] <- kcr_data[missing_idxes,"DerivedSS2000"]
kcr_data <- kcr_data[, -which(colnames(kcr_data) %in% c("SEERSummStg2000","DerivedSS2000"))]

#           Feature Missing_N_Perc
#1         TNMPathM  23443 (47.6%)
#2         TNMClinM  12945 (26.29%)
#3 Comb_SEERSummStg   3329 (6.76%)
get_missing_rate_table(kcr_data,c("TNMPathM","TNMClinM","Comb_SEERSummStg"))

#########################################################################################################
#Compute DAJCC_T, DAJCC_M, DAJCC_N
#########################################################################################################
kcr_data$DAJCC_T <- get_DAJCC_var_funtion(kcr_data,"TNMPathT","TNMClinT")
kcr_data$DAJCC_M <- get_DAJCC_var_funtion(kcr_data,"TNMPathM","TNMClinM")
kcr_data$DAJCC_N <- get_DAJCC_var_funtion(kcr_data,"TNMPathN","TNMClinN")

DAJCC_T_tb <- as.data.frame(table(kcr_data$DAJCC_T))
DAJCC_M_tb <- as.data.frame(table(kcr_data$DAJCC_M))
DAJCC_N_tb <- as.data.frame(table(kcr_data$DAJCC_N))
write.csv(DAJCC_T_tb,"/Users/lucasliu/Desktop/DAJCC_T_tb.csv")
write.csv(DAJCC_M_tb,"/Users/lucasliu/Desktop/DAJCC_M_tb.csv")
write.csv(DAJCC_N_tb,"/Users/lucasliu/Desktop/DAJCC_N_tb.csv")

get_missing_rate_table(kcr_data,c("DAJCC_T","DAJCC_M","DAJCC_N"))

################################################################################ 
###3.  Load Valid month
################################################################################ 
Valid_Month_df <- read.xlsx(paste0(data_dir,"5_valid_month_df.xlsx"),sheet = 1)


################################################################################ 
##4. Load ID source
################################################################################ 
ID_Sources_Df <- read.xlsx(paste0(data_dir,"1_All_ID_Source.xlsx"),sheet = 1)

################################################################################ 
##5.anlaysis ID 
################################################################################ 
analysis_ID <- unique(Reduce(intersect, list(updated_All_event_df$study_id,kcr_data$study_id, Valid_Month_df$study_id)))

################################################################################ 
#7. BC codes
################################################################################ 
bc_codes <- paste0("C50",seq(0,9,1))

################################################################################ 
#8. All cancer site df
################################################################################ 
All_cancer_site_date_df <- read.xlsx(paste0(data_dir,"4_All_cancer_site_date_df.xlsx"),sheet = 1)

#########################################################################################################
#### 6.  get charastersitc for final anlaysis IDs
#'@NOTE:  for now, we use Best stage to determine reginoal or not, 
#'#Update this later using updated seerstage
#########################################################################################################
char_df <- as.data.frame(matrix(NA, nrow =length(analysis_ID) ,ncol = 35))
colnames(char_df) <- c("study_id","Medicaid_OR_Medicare","SBCE","First_Primary_BC_related_Death","Type_2nd_Event",
                       "Diagnosis_Year_1stEvent","Diagnosis_Year_2ndEvent","Year_Death",
                       "Race","Site","Stage","Laterality",
                       "Grade","er_stat","pr_stat","surg_prim_site","her2_stat",
                       "radiation","DAJCC_T","DAJCC_M","DAJCC_N","reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                       "cs_tum_size","num_claims","most_recent_enrollment_year",
                       "cs_tum_ext","chemo","hormone","cs_tum_nodes",
                       "num_nonbc","regional","Comb_SEERSummStg","date_Birth")

for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){
    print(i)
  }
  curr_id <- analysis_ID[i]
  char_df[i,"study_id"] <- curr_id
  
  #in medicare or medicaid
  curr_ID_source <- ID_Sources_Df[which(ID_Sources_Df[,"Kcr_ID"] == curr_id),]
  if ( (curr_ID_source$in_Medicare == 1) & (curr_ID_source$in_Medicaid == 1)){
    char_df[i,"Medicaid_OR_Medicare"] <- "Both"
  }else if(curr_ID_source$in_Medicare == 1){
    char_df[i,"Medicaid_OR_Medicare"] <- "Medicare"
  }else if (curr_ID_source$in_Medicaid == 1){
    char_df[i,"Medicaid_OR_Medicare"] <- "Medicaid"
  }else{
    char_df[i,"Medicaid_OR_Medicare"] <- "None"
  }
  
  #valid claim per month
  curr_perMonth_file <- paste0(per_month_data_dir,"ID",curr_id,"_perMonthData_inValidMonth.xlsx")
  
  if ( file.exists(curr_perMonth_file) == T){
        curr_perMonth_df <- read.xlsx(curr_perMonth_file,sheet = 1)
        min_month <- min(ymd(curr_perMonth_df$Month_Start))
        max_month <- max(ymd(curr_perMonth_df$Month_Start))
        
        #per day file
        curr_perday_file <- paste0(perday_dir,"ID",curr_id,"_perDay_Data.xlsx") #    #1.get per day file
        curr_perday_df <- read.xlsx(curr_perday_file,sheet = 1)
        #claims in valid months
        curr_claims_indxes <- which(ymd(curr_perday_df$claims_date) >= min_month & ymd(curr_perday_df$claims_date) <= max_month)

        char_df[i,"num_claims"]  <- length(curr_claims_indxes)
        
        char_df[i,"most_recent_enrollment_year"]  <-  as.numeric(unlist(strsplit(as.character(max_month),split= "-"))[1])
  }

  
  
  #Event data
  curr_event <- updated_All_event_df[which(updated_All_event_df[,"study_id"] == curr_id),]
  char_df[i,"SBCE"] <-  curr_event$SBCE
  char_df[i,"First_Primary_BC_related_Death"] <-  curr_event$First_Primary_BC_related_Death
  char_df[i,"Type_2nd_Event"] <-  curr_event$Type_2nd_Event
  
  #first and 2nd event and death date 
  curr_1stevent_date <- curr_event[,"Date_1st_Event"]
  curr_2ndevent_date <- curr_event[,"Date_2nd_Event"]
  curr_date_death <- curr_event[,"Date_death"]
  
  char_df[i,"Diagnosis_Year_1stEvent"] <-  as.numeric(unlist(strsplit(curr_1stevent_date,split = "/"))[3])
  char_df[i,"Diagnosis_Year_2ndEvent"] <-  as.numeric(unlist(strsplit(curr_2ndevent_date,split = "/"))[3])
  char_df[i,"Year_Death"] <-  as.numeric(unlist(strsplit(curr_date_death,split = "/"))[3])
  
  
  #KCR Data
  #1.get frist primary site, due to merging effect, we need to do the following
  curr_1stevent_type <- unlist(strsplit(curr_event[,"Type_1st_Event"],split = "$$$",fixed = T))
  curr_1st_pbc_idx   <- which(curr_1stevent_type == "First_Primary")
  curr_1stevent_site <- unlist(strsplit(curr_event[,"Site_1st_Event"],split = "$$$",fixed = T))
  curr_1stprimary_site <- curr_1stevent_site[curr_1st_pbc_idx]
  #make sure it is in bc_code
  curr_1stprimary_site <- curr_1stprimary_site[which(curr_1stprimary_site %in% bc_codes)] 
  
  #curr KCR
  curr_kcr <- kcr_data[which(kcr_data[,"study_id"] == curr_id & 
                               kcr_data[,"PrimarySite"] %in% curr_1stprimary_site &
                               kcr_data[,"Date_dx"] ==  curr_1stevent_date &
                               kcr_data[,"CentralSequenceNumber"] %in% c(0,1)),] #Acutal first priamry
  
  char_df[i,"date_Birth"] <- curr_kcr[,"date_Birth"]
  char_df[i,"Race"] <- curr_kcr[,"Race1"]
  char_df[i,"Site"] <- curr_kcr[,"PrimarySite"]
  char_df[i,"Stage"] <- curr_kcr[,"BestStageGrp"]
  char_df[i,"Grade"] <- curr_kcr[,"Grade"]
  char_df[i,"Laterality"] <- curr_kcr[,"Laterality"]
  char_df[i,"er_stat"] <- curr_kcr[,"er_stat"]
  char_df[i,"pr_stat"] <- curr_kcr[,"pr_stat"]
  char_df[i,"her2_stat"] <- curr_kcr[,"her2_stat"]
  char_df[i,"surg_prim_site"] <- curr_kcr[,"RXSummSurgPrimSite"]
  
  char_df[i,"radiation"] <- curr_kcr[,"RXSummRadiation"]
  char_df[i,"chemo"] <- curr_kcr[,"RXSummChemo"]
  char_df[i,"hormone"] <- curr_kcr[,"RXSummHormone"]
  
  char_df[i,"reg_nodes_exam"] <- curr_kcr[,"RegNodesExamined"]
  char_df[i,"reg_nodes_pos"] <- curr_kcr[,"RegNodesPositive"]
  char_df[i,"cs_tum_size"] <- curr_kcr[,"CSTumorSize"]
  char_df[i,"cs_tum_ext"] <- curr_kcr[,"CSTumorSizeExtEval"]
  char_df[i,"reg_age_at_dx"] <- curr_kcr[,"DiagAge"]
  char_df[i,"cs_tum_nodes"] <- curr_kcr[,"CSLymphNodes"]
  
  
  char_df[i,"DAJCC_T"] <- curr_kcr[,"DAJCC_T"]
  char_df[i,"DAJCC_M"] <- curr_kcr[,"DAJCC_M"]
  char_df[i,"DAJCC_N"] <- curr_kcr[,"DAJCC_N"]
  
  #num_nonbc: number of primary cancer diagnoses that are not breast cancer that the patient has had
  #curr all cancer site df
  curr_cancer_site_df <- All_cancer_site_date_df[which(All_cancer_site_date_df[,"study_id"] == curr_id),]
  curr_cancer_site_df <- curr_cancer_site_df[order(mdy(curr_cancer_site_df$Date)),] #ordered by time increasing
  curr_subseq_idxes <- which(mdy(curr_cancer_site_df$Date) > mdy(curr_1stevent_date)) 
  if (length(curr_subseq_idxes) > 0 ){
    curr_subseq_idx <- curr_subseq_idxes
    curr_subseq_df <- curr_cancer_site_df[curr_subseq_idx,]
    
    curr_subseq_code_types <- unlist(strsplit(curr_subseq_df[,"Type"],split = "$$$",fixed = T))
    curr_subseq_code_site <- unlist(strsplit(curr_subseq_df[,"Site"],split = "$$$",fixed = T))
    
    primary_idxes <- which(grepl("Primary",curr_subseq_code_types)==T)
    primary_codes  <- curr_subseq_code_site[primary_idxes]
    
    non_primary_bc_indxes <- which(!primary_codes %in% bc_codes)
    
    char_df[i,"num_nonbc"] <- length(non_primary_bc_indxes)
    
  }else{
    char_df[i,"num_nonbc"] <- 0
  }
  
  char_df[i,"Comb_SEERSummStg"] <- curr_kcr[,"Comb_SEERSummStg"]
  
  
  
  #Local or regional
  curr_seer_stage <- curr_kcr[,"Comb_SEERSummStg"]
  if (is.na(curr_seer_stage) == F){
    if (curr_seer_stage %in% c(2,3,4,5)){
        char_df[i,"regional"] <- 1
    }else{
       char_df[i,"regional"] <- 0
    }
  }else{ #no stage info
    char_df[i,"regional"] <- NA
  }

}

write.xlsx(char_df,paste0(outdir,"8_PatientLevel_charecteristics.xlsx"))
