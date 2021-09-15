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

get_pts_level_char_func <- function(analysis_ID,ID_Sources_Df,num_month_df,event_df,kcr_df,All_cancer_site_date_df){
  # analysis_ID <- analysis_ID1_Allenrolled
  # ID_Sources_Df <- ID_Sources_data
  # num_month_df <- NUM_Month_df1_Allenrolled
  # event_df <- All_event_df
  # kcr_df   <- kcr_data
  # All_cancer_site_date_df <- All_cancer_site_date_data

  char_df <- as.data.frame(matrix(NA, nrow =length(analysis_ID) ,ncol = 41))
  colnames(char_df) <- c("study_id","Medicaid_OR_Medicare","SBCE",
                         "Diagnosis_Year","Race","Site",
                         "BestStageGrp","Stage",
                         "Comb_SEERSummStg","regional","Laterality",
                         "Grade","er_stat","pr_stat","surg_prim_site","her2_stat",
                         "radiation","reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                         "cs_tum_size","cs_tum_ext","chemo","hormone","cs_tum_nodes",
                         "num_nonbc","date_Birth",
                         "Site_1st_Event","Date_1st_Event",
                         "Site_2nd_Event","Type_2nd_Event","Date_2nd_Event",
                         "Event_2nd_Is1stPrimaryBCDeath","Year_1stPrimaryBCDeath",
                         "Days_1stEventTODeath","Days_1stTO2nd",
                         "Num_Enrolled_Prediction_Months","most_recent_enrollment_year",
                         "Num_Month_before_2ndEvent","Num_Month_AfterOrEqual_2ndEvent","HasEnoughMonths_InWindow")


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
    
    #Num of enrolled prediction month
    curr_num_month_df <- num_month_df[which(num_month_df[,"study_id"] == curr_id),]
    char_df[i,"Num_Enrolled_Prediction_Months"]  <- curr_num_month_df[,"Num_Enrolled_Prediction_Months"]
    char_df[i,"most_recent_enrollment_year"]     <- as.numeric(unlist(strsplit(curr_num_month_df[,"Last_Enrolled_Prediction_Month_End"],split= "-"))[1])
    char_df[i,"SBCE"]          <-  curr_num_month_df[,"SBCE"]
    char_df[i,"Num_Month_before_2ndEvent"]  <- curr_num_month_df[,"Num_Month_before_2ndEvent"]
    char_df[i,"Num_Month_AfterOrEqual_2ndEvent"]  <- curr_num_month_df[,"Num_Month_AfterOrEqual_2ndEvent"]
    char_df[i,"HasEnoughMonths_InWindow"]  <- curr_num_month_df[,"HasEnoughMonths_InWindow"]

    #Event data
    curr_event <- event_df[which(event_df[,"study_id"] == curr_id),]
    
    #1st event site and year,due to merging effect, we need to do the following
    #1.get frist primary site, 
    curr_1stevent_type <- unlist(strsplit(curr_event[,"Type_1st_Event"],split = "$$$",fixed = T))
    curr_1st_pbc_idx   <- which(curr_1stevent_type == "First_Primary")
    curr_1stevent_site <- unlist(strsplit(curr_event[,"Site_1st_Event"],split = "$$$",fixed = T))
    curr_1stprimary_site <- curr_1stevent_site[curr_1st_pbc_idx]
    #make sure it is in bc_code
    curr_1stprimary_site <- curr_1stprimary_site[which(curr_1stprimary_site %in% bc_codes)] 
    char_df[i,"Site_1st_Event"] <- curr_1stprimary_site
    
    curr_1stevent_date <- curr_event[,"Date_1st_Event"]
    char_df[i,"Date_1st_Event"] <-  curr_1stevent_date
    
    
    #2nd event site, type and year
    char_df[i,"Type_2nd_Event"] <-  curr_event[,"Type_2nd_Event"]
    char_df[i,"Site_2nd_Event"] <-  curr_event[,"Site_2nd_Event"]
    char_df[i,"Date_2nd_Event"] <-  curr_event[,"Date_2nd_Event"]
    
    
    #2nd event is 1st primary BC related death
    if (grepl("Death",char_df[i,"Type_2nd_Event"]) == T){
      char_df[i,"Event_2nd_Is1stPrimaryBCDeath"] <- 1
      char_df[i,"Year_1stPrimaryBCDeath"] <-  as.numeric(unlist(strsplit(curr_event[,"Primary_1stBC_Death_Date"],split = "/"))[3])
      char_df[i,"Days_1stEventTODeath"]   <-  curr_event[,"Days_1stEventTODeath"]
      char_df[i,"Days_1stTO2nd"]          <-  curr_event[,"Days_1stTO2nd"]
      

    }else{
      char_df[i,"Event_2nd_Is1stPrimaryBCDeath"] <- 0
      char_df[i,"Year_1stPrimaryBCDeath"]  <- NA
      char_df[i,"Days_1stEventTODeath"]    <-  NA
      char_df[i,"Days_1stTO2nd"]           <-  NA
    }
    
    #KCR Data
    curr_kcr <- kcr_df[which(kcr_df[,"study_id"] == curr_id & 
                               kcr_df[,"PrimarySite"] %in% curr_1stprimary_site &
                               kcr_df[,"Date_dx"] ==  curr_1stevent_date &
                               kcr_df[,"CentralSequenceNumber"] %in% c(0,1)),] #Acutal first priamry
    
    char_df[i,"Diagnosis_Year"] <- curr_kcr[,"Year_Diag"]
    char_df[i,"date_Birth"] <- curr_kcr[,"date_Birth"]
    char_df[i,"Race"] <- curr_kcr[,"Race1"]
    char_df[i,"Site"] <- curr_kcr[,"PrimarySite"]
    
    #For BestStageGrp: Stage 0 (0-2) Stage I [10-30) Stage II [30-50) Stage III [50-70) Stage IV [70-80)
    curr_BestStageGrp <- curr_kcr[,"BestStageGrp"]
    char_df[i,"BestStageGrp"] <- curr_BestStageGrp
    if (is.na(curr_BestStageGrp) == T){
      char_df[i,"Stage"] <- NA
    }else if(curr_BestStageGrp >= 0 & curr_BestStageGrp < 2){
      char_df[i,"Stage"] <- 0
    }else if (curr_BestStageGrp >=10 & curr_BestStageGrp < 30){
      char_df[i,"Stage"] <- 1
    }else if(curr_BestStageGrp >=30 & curr_BestStageGrp < 50){
      char_df[i,"Stage"] <- 2
    }else if(curr_BestStageGrp >=50 & curr_BestStageGrp < 70){
      char_df[i,"Stage"] <- 3
    }else if (curr_BestStageGrp >=70 & curr_BestStageGrp < 80){
      char_df[i,"Stage"] <- 4
    }else{
      char_df[i,"Stage"] <- NA
    }
    
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
    
    #'@NOTE: add back later when data issue solved
    # char_df[i,"DAJCC_T"] <- curr_kcr[,"DAJCC_T"]
    # char_df[i,"DAJCC_M"] <- curr_kcr[,"DAJCC_M"]
    # char_df[i,"DAJCC_N"] <- curr_kcr[,"DAJCC_N"]
    
    #num_nonbc: number of primary cancer diagnoses that are not breast cancer that the patient has had
    #curr all cancer site df
    curr_cancer_site_df <- All_cancer_site_date_df[which(All_cancer_site_date_df[,"study_id"] == curr_id),]
    curr_cancer_site_df <- curr_cancer_site_df[order(mdy(curr_cancer_site_df[,"Date"])),] #ordered by time increasing
    curr_subseq_idxes <- which(mdy(curr_cancer_site_df[,"Date"]) > mdy(curr_1stevent_date)) 
    if (length(curr_subseq_idxes) > 0 ){
      curr_subseq_df <- curr_cancer_site_df[curr_subseq_idxes,]
      
      curr_subseq_code_types <- unlist(strsplit(curr_subseq_df[,"Type"],split = "$$$",fixed = T))
      curr_subseq_code_site <- unlist(strsplit(curr_subseq_df[,"Site"],split = "$$$",fixed = T))
      
      primary_idxes <- which(grepl("Primary",curr_subseq_code_types)==T)
      primary_codes  <- curr_subseq_code_site[primary_idxes]
      
      non_bc_primary_indxes <- which(!primary_codes %in% bc_codes)
      
      char_df[i,"num_nonbc"] <- length(non_bc_primary_indxes)
      
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
  return(char_df)
}


#########################################################################################################
#Data dir
#########################################################################################################
#onHPC
raw_data_dir  <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
Proj_dir <- "/recapse/intermediate_data/"

#local
raw_data_dir  <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
Proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

data_dir1  <- paste0(Proj_dir, "4_RecurrDates_Outcome_Info/")
data_dir2  <- paste0(Proj_dir, "7_PrePostLabels_AndAvailibility6mon/")
data_dir3  <- paste0(Proj_dir, "1_ID_Sources_Info/")
outdir     <- paste0(Proj_dir, "8_Characteristics/Patient_Level/")


#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(data_dir1,"4_All_event_df.xlsx"),sheet = 1)
SBCE_df      <- read.xlsx(paste0(data_dir1,"4_SBCE_Label.xlsx"),sheet = 1)

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
# write.csv(DAJCC_T_tb,"/Users/lucasliu/Desktop/DAJCC_T_tb.csv")
# write.csv(DAJCC_M_tb,"/Users/lucasliu/Desktop/DAJCC_M_tb.csv")
# write.csv(DAJCC_N_tb,"/Users/lucasliu/Desktop/DAJCC_N_tb.csv")

get_missing_rate_table(kcr_data,c("DAJCC_T","DAJCC_M","DAJCC_N"))

################################################################################ 
###3.  Load Num enrolled prediction month
################################################################################ 
NUM_Month_df1_Allenrolled <- read.xlsx(paste0(data_dir2,"NUM_Months_AvalFlags_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
NUM_Month_df2_EnrolledHasCodes <- read.xlsx(paste0(data_dir2,"NUM_Months_AvalFlags_WithEveryMonthsHasCodes.xlsx"),sheet = 1)


################################################################################ 
##4. Load ID source
################################################################################ 
ID_Sources_data <- read.xlsx(paste0(data_dir3,"1_All_ID_Source.xlsx"),sheet = 1)

################################################################################ 
##5.anlaysis ID 
################################################################################ 
analysis_ID1_Allenrolled <- unique(Reduce(intersect, list(All_event_df[,"study_id"],kcr_data[,"study_id"], NUM_Month_df1_Allenrolled[,"study_id"])))          #27989
analysis_ID2_EnrolledHasCode <- unique(Reduce(intersect, list(All_event_df[,"study_id"],kcr_data[,"study_id"], NUM_Month_df2_EnrolledHasCodes[,"study_id"]))) #27830

################################################################################ 
#7. BC codes
################################################################################ 
bc_codes <- paste0("C50",seq(0,9,1))

################################################################################ 
#8. All cancer site df
################################################################################ 
All_cancer_site_date_data <- read.xlsx(paste0(data_dir1,"4_All_cancer_site_date_df.xlsx"),sheet = 1)

#########################################################################################################
#### 6.  get charastersitc for final anlaysis IDs
#'@TODO: Add "DAJCC_T","DAJCC_M","DAJCC_N" back later due to data issue
#########################################################################################################
#1. For using all enrollment data with possible months has no codes at all
pts_level_char_df1 <- get_pts_level_char_func(analysis_ID1_Allenrolled,ID_Sources_data,NUM_Month_df1_Allenrolled,All_event_df,kcr_data,All_cancer_site_date_data)
write.xlsx(pts_level_char_df1,paste0(outdir,"8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"))

#1. For using all enrollment data with possible months has no codes at all
pts_level_char_df2 <- get_pts_level_char_func(analysis_ID2_EnrolledHasCode,ID_Sources_data,NUM_Month_df2_EnrolledHasCodes,All_event_df,kcr_data,All_cancer_site_date_data)
write.xlsx(pts_level_char_df2,paste0(outdir,"8_PatientLevel_char_WithEveryMonthsHasCodes.xlsx"))
