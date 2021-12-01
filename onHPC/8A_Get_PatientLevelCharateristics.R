source("Recapse_Ultility.R")

#This function update seer summ_stg  
updated_SEERSummStg2000_func <- function(kcr_df, new_kcr_df){
  #kcr_df     <- kcr_data
  #new_kcr_df <- new_kcr_data2
  
  #Match ids
  new_kcr_df <- new_kcr_df[match(new_kcr_df[,c("study_id")],kcr_df[,c("study_id")]),]
  
  #Add DerivedSS2000 in old kcr data
  kcr_df$DerivedSS2000 <- new_kcr_df$DerivedSS2000
  
  #Get comb SEERSummStg
  #1. for not missing original SEER stage, use "SEERSummStg2000 ('SEERSummStg2000’ only captures SEER summary stage from the years 2001-2003. )
  #2. for missing original SEER stage, use "DerivedSS2000 (‘DerivedSS2000’ was added for the summary stage between the years 2004-2015. )
  #No such information was captured for the year 2000.
  kcr_df$Comb_SEERSummStg <- NA
  NOTmissing_idxes <- which(is.na(kcr_df[,"SEERSummStg2000"])== F)
  missing_idxes    <- which(is.na(kcr_df[,"SEERSummStg2000"])== T)
  
  kcr_df[NOTmissing_idxes,"Comb_SEERSummStg"] <- kcr_df[NOTmissing_idxes,"SEERSummStg2000"] #use "SEERSummStg2000
  kcr_df[missing_idxes,"Comb_SEERSummStg"]    <- kcr_df[missing_idxes,"DerivedSS2000"] #use "DerivedSS2000
  
  
  
  #remove reduantant old columns
  kcr_df <- kcr_df[, -which(colnames(kcr_df) %in% c("SEERSummStg2000","DerivedSS2000"))]
  
  return(kcr_df)
}

#This funcion update TNM
updated_TNM_func <- function(kcr_df, new_kcr_df){
  #kcr_df     <- kcr_data
  #new_kcr_df <- new_kcr_data3
  
  #replace blank as NA
  new_kcr_df[which(new_kcr_df$TNMPathT == ""),"TNMPathT"] <- NA
  new_kcr_df[which(new_kcr_df$TNMPathN == ""),"TNMPathN"] <- NA
  new_kcr_df[which(new_kcr_df$TNMPathM == ""),"TNMPathM"] <- NA
  new_kcr_df[which(new_kcr_df$TNMClinT == ""),"TNMClinT"] <- NA
  new_kcr_df[which(new_kcr_df$TNMClinN == ""),"TNMClinN"] <- NA
  new_kcr_df[which(new_kcr_df$TNMClinM == ""),"TNMClinM"] <- NA
  
  #Match ids
  new_kcr_df <- new_kcr_df[match(new_kcr_df[,c("study_id")],kcr_df[,c("study_id")]),]
  
  #Update TNM in old kcr data
  kcr_df$TNMPathT <- as.character(new_kcr_df$TNMPathT)
  kcr_df$TNMPathN <- as.character(new_kcr_df$TNMPathN)
  kcr_df$TNMPathM <- as.character(new_kcr_df$TNMPathM)
  kcr_df$TNMClinT <- as.character(new_kcr_df$TNMClinT)
  kcr_df$TNMClinN <- as.character(new_kcr_df$TNMClinN)
  kcr_df$TNMClinM <- as.character(new_kcr_df$TNMClinM)
  
  return(kcr_df)
}


#This function re-code race
recode_Race_func <- function(kcr_df){
  #kcr_df     <- kcr_data
  
  #Re-code race
  #1: original 1
  #2: original 2
  #3: others
  kcr_df$Race_Recoded <- NA
  race1_idxes <-  which(kcr_df$Race1 == 1)
  race2_idxes <-  which(kcr_df$Race1 == 2)
  kcr_df[race1_idxes,"Race_Recoded"] <- 1
  kcr_df[race2_idxes,"Race_Recoded"] <- 2
  kcr_df[-c(race1_idxes,race2_idxes),"Race_Recoded"] <- 3
  
  kcr_df <- kcr_df[, -which(colnames(kcr_df) %in% c("Race1"))]
  
  return(kcr_df)
  
}

#This function recode  RXSummSurgPrimSite (two versions)
recode_SPS_func <- function(kcr_df){
  #kcr_df     <- kcr_data
  
  #recode RXSummSurgPrimSite
  #'@Qestion: what about 43,44,45,46,47,48,49, 75 and 76?
  #Version1 (Dr.Huang): 0, 19, (20-24), 30, (40-42), (50-59,63),  (60-62, 64-69, 73,74), 70-72, 80, 90, 99
  #Version2 (Quan):  00,19,20 (21-24),30,40,41,42,50,51(53-56),52(57,58,59,63),60,61(64-67),62(68,69,73,74),70,71,72,80,90,99
  kcr_df$RXSummSurgPrimSite_RecodedV1 <- NA
  kcr_df$RXSummSurgPrimSite_RecodedV2 <- NA

  kcr_df[,"RXSummSurgPrimSite_RecodedV1"] <- recode_SurgPrimSite_func_v1(kcr_df[,"RXSummSurgPrimSite"])
  kcr_df[,"RXSummSurgPrimSite_RecodedV2"] <- recode_SurgPrimSite_func_v2(kcr_df[,"RXSummSurgPrimSite"])
  kcr_df <- kcr_df[, -which(colnames(kcr_df) %in% c("RXSummSurgPrimSite"))]

  return(kcr_df)
  
}


recode_SurgPrimSite_func_v1 <- function(SurgPrimSite_values){
  #SurgPrimSite_values <- kcr_df[,"RXSummSurgPrimSite"] 
  
  grp0_idxes  <- which(SurgPrimSite_values == 0)
  grp19_idxes <- which(SurgPrimSite_values == 19)
  grp20_idxes <- which(SurgPrimSite_values %in% c(20,21,22,23,24))
  grp30_idxes <- which(SurgPrimSite_values ==30 )
  grp40_idxes <- which(SurgPrimSite_values %in% c(40,41,42))
  grp50_idxes <- which(SurgPrimSite_values %in% c(50,51,52,53,54,55,56,57,58,59,63))
  grp60_idxes <- which(SurgPrimSite_values %in% c(60,61,62,64,65,66,67,68,69,73,74))
  grp70_idxes <- which(SurgPrimSite_values %in% c(70,71,72))
  grp80_idxes <- which(SurgPrimSite_values == 80)
  grp90_idxes <- which(SurgPrimSite_values == 90)
  grp99_idxes <- which(SurgPrimSite_values == 99)
  
  SurgPrimSite_values[grp0_idxes]  <- 0
  SurgPrimSite_values[grp19_idxes] <- 19
  SurgPrimSite_values[grp20_idxes] <- 20
  SurgPrimSite_values[grp30_idxes] <- 30
  SurgPrimSite_values[grp40_idxes] <- 40
  SurgPrimSite_values[grp50_idxes] <- 50
  SurgPrimSite_values[grp60_idxes] <- 60
  SurgPrimSite_values[grp70_idxes] <- 70
  SurgPrimSite_values[grp80_idxes] <- 80
  SurgPrimSite_values[grp90_idxes] <- 90
  SurgPrimSite_values[grp99_idxes] <- 99
  
  #Recode other values as NA
  all_idxes <- c(grp0_idxes,grp19_idxes,grp20_idxes,grp30_idxes,grp40_idxes,grp50_idxes,grp60_idxes,
                 grp70_idxes,grp80_idxes,grp90_idxes,grp99_idxes)
  SurgPrimSite_values[-all_idxes] <- NA
  
  return(SurgPrimSite_values)
}

recode_SurgPrimSite_func_v2 <- function(SurgPrimSite_values){
  #SurgPrimSite_values <- kcr_df[,"RXSummSurgPrimSite"] 
  
  #Version2 (Quan): 
  grp0_idxes  <- which(SurgPrimSite_values == 0)
  grp19_idxes <- which(SurgPrimSite_values == 19)
  grp20_idxes <- which(SurgPrimSite_values %in% c(20,21,22,23,24))
  grp30_idxes <- which(SurgPrimSite_values == 30 )
  grp40_idxes <- which(SurgPrimSite_values == 40 )
  grp41_idxes <- which(SurgPrimSite_values == 41 )
  grp42_idxes <- which(SurgPrimSite_values == 42 )
  grp50_idxes <- which(SurgPrimSite_values == 50 )
  grp51_idxes <- which(SurgPrimSite_values %in% c(51,53,54,55,56))
  grp52_idxes <- which(SurgPrimSite_values %in% c(52,57,58,59,63))
  grp60_idxes <- which(SurgPrimSite_values == 60)
  grp61_idxes <- which(SurgPrimSite_values %in% c(61,64,65,66,67))
  grp62_idxes <- which(SurgPrimSite_values %in% c(62,68,69,73,74))
  grp70_idxes <- which(SurgPrimSite_values == 70)
  grp71_idxes <- which(SurgPrimSite_values == 71)
  grp72_idxes <- which(SurgPrimSite_values == 72)
  grp80_idxes <- which(SurgPrimSite_values == 80)
  grp90_idxes <- which(SurgPrimSite_values == 90)
  grp99_idxes <- which(SurgPrimSite_values == 99)
  
  SurgPrimSite_values[grp0_idxes]  <- 0
  SurgPrimSite_values[grp19_idxes] <- 19
  SurgPrimSite_values[grp20_idxes] <- 20
  SurgPrimSite_values[grp30_idxes] <- 30
  SurgPrimSite_values[grp40_idxes] <- 40
  SurgPrimSite_values[grp41_idxes] <- 41
  SurgPrimSite_values[grp42_idxes] <- 42
  SurgPrimSite_values[grp50_idxes] <- 50
  SurgPrimSite_values[grp51_idxes] <- 51
  SurgPrimSite_values[grp52_idxes] <- 52
  SurgPrimSite_values[grp60_idxes] <- 60
  SurgPrimSite_values[grp61_idxes] <- 61
  SurgPrimSite_values[grp62_idxes] <- 62
  SurgPrimSite_values[grp70_idxes] <- 70
  SurgPrimSite_values[grp71_idxes] <- 71
  SurgPrimSite_values[grp72_idxes] <- 72
  SurgPrimSite_values[grp80_idxes] <- 80
  SurgPrimSite_values[grp90_idxes] <- 90
  SurgPrimSite_values[grp99_idxes] <- 99
  
  #Recode other values as NA
  all_idxes <- c(grp0_idxes,grp19_idxes,grp20_idxes,grp30_idxes,
                 grp40_idxes,grp41_idxes,grp42_idxes,
                 grp50_idxes,grp51_idxes,grp52_idxes,
                 grp60_idxes,grp61_idxes,grp62_idxes,
                 grp70_idxes,grp71_idxes,grp72_idxes,
                 grp80_idxes,grp90_idxes,grp99_idxes)
  SurgPrimSite_values[-all_idxes] <- NA
  
  return(SurgPrimSite_values)
}


#This function compute DAJCC
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

#According to "pedsf_attachment_a.pdf", convert DAJCC variable to numeric code
convert_DAJCC_var_function <- function(in_data,var_name){
  #in_data <- kcr_data
  #var_name <- "DAJCC_N"
  
  in_values <- in_data[,var_name]
  
  if (var_name == "DAJCC_M"){
    #Remove prefix c or p or I+
    in_values <- gsub("c|p|I\\+","",in_values,ignore.case = F)
    
    #Convert
    in_values[which(in_values == "X")] <- "99"
    in_values[which(in_values == "0")] <- "00"
    in_values[which(in_values == "1")] <- "10"
    in_values[which(in_values == "1A")] <- "11"
    in_values[which(in_values == "1B")] <- "12"
    in_values[which(in_values == "1C")] <- "13"
    in_values[which(in_values == "1 NOS")] <- "19"
    in_values[which(is.na(in_values) == T)] <- "88"
  }else if (var_name == "DAJCC_T"){
    #Remove prefix c or p 
    in_values <- gsub("c|p","",in_values,ignore.case = F)
    #2D and ISD not found in table, so recode them as NA
    in_values[which(in_values %in% c("ISD","2D"))] <- NA
    
    in_values[which(in_values == "X")] <- "99"
    in_values[which(in_values == "0")] <- "00"
    in_values[which(in_values == "A")] <- "01"
    in_values[which(in_values == "IS")] <- "05"
    in_values[which(in_values == "ISPU")] <- "06"
    in_values[which(in_values == "ISPD")] <- "07"
    
    in_values[which(in_values == "1")] <- "10"
    in_values[which(in_values %in% c("1MI","1MIC"))] <- "11"
    in_values[which(in_values == "1 NOS")] <- "19"
    in_values[which(in_values == "1A")] <- "12"
    in_values[which(in_values == "1A1")] <- "13"
    in_values[which(in_values == "1A2")] <- "14"
    in_values[which(in_values == "1B")] <- "15"
    in_values[which(in_values == "1B1")] <- "16"
    in_values[which(in_values == "1B2")] <- "17"
    in_values[which(in_values == "1C")] <- "18"
    
    in_values[which(in_values == "2")] <- "20"
    in_values[which(in_values == "2 NOS")] <- "29"
    in_values[which(in_values == "2A")] <- "21"
    in_values[which(in_values == "2B")] <- "22"
    in_values[which(in_values == "2C")] <- "23"
    
    in_values[which(in_values == "3")] <- "30"
    in_values[which(in_values == "3 NOS")] <- "39"
    in_values[which(in_values == "3A")] <- "31"
    in_values[which(in_values == "3B")] <- "32"
    in_values[which(in_values == "3C")] <- "33"
    
    in_values[which(in_values == "4")] <- "40"
    in_values[which(in_values == "4 NOS")] <- "49"
    in_values[which(in_values == "4A")] <- "41"
    in_values[which(in_values == "4B")] <- "42"
    in_values[which(in_values == "4C")] <- "43"
    in_values[which(in_values == "4D")] <- "44"
    
    in_values[which(in_values == "1A NOS")] <- "80"
    in_values[which(in_values == "1B NOS")] <- "81"
    
    in_values[which(is.na(in_values) == T)] <- "88"
  }else if (var_name == "DAJCC_N"){
    #Remove prefix c or p 
    in_values <- gsub("c|p","",in_values,ignore.case = F)
    
    in_values[which(in_values == "X")] <- "99"
    in_values[which(in_values == "0")] <- "00"
    in_values[which(in_values == "0I-")] <- "01"
    in_values[which(in_values == "0I+")] <- "02"
    in_values[which(in_values == "0M-")] <- "03"
    in_values[which(in_values == "0M+")] <- "04"
    
    in_values[which(in_values == "1")] <- "10"
    in_values[which(in_values == "1 NOS")] <- "19"
    in_values[which(in_values == "1A")] <- "11"
    in_values[which(in_values == "1B")] <- "12"
    in_values[which(in_values == "1C")] <- "13"
    in_values[which(in_values == "1MI")] <- "18"
    
    in_values[which(in_values == "2")] <- "20"
    in_values[which(in_values == "2 NOS")] <- "29"
    in_values[which(in_values == "2A")] <- "21"
    in_values[which(in_values == "2B")] <- "22"
    in_values[which(in_values == "2C")] <- "23" 
    
    in_values[which(in_values == "3")] <- "30"
    in_values[which(in_values == "3 NOS")] <- "39"
    in_values[which(in_values == "3A")] <- "31"
    in_values[which(in_values == "3B")] <- "32"
    in_values[which(in_values == "3C")] <- "33"
    
    in_values[which(is.na(in_values) == T)] <- "88"
  }
  
  in_data[,var_name] <- as.numeric(in_values)
  return(in_data)
  
}


get_pts_level_char_func <- function(analysis_ID,ID_Sources_Df,num_month_df,event_df,kcr_df,All_cancer_site_date_df){
  # analysis_ID <- analysis_ID1_Allenrolled
  # ID_Sources_Df <- ID_Sources_data
  # num_month_df <- NUM_Month_df1_Allenrolled
  # event_df <- All_event_df
  # kcr_df   <- kcr_data
  # All_cancer_site_date_df <- All_cancer_site_date_data

  char_df <- as.data.frame(matrix(NA, nrow =length(analysis_ID) ,ncol = 45))
  colnames(char_df) <- c("study_id","Medicaid_OR_Medicare","SBCE",
                         "Diagnosis_Year","Race","Site",
                         "BestStageGrp","Stage",
                         "Comb_SEERSummStg","regional","Laterality",
                         "Grade","er_stat","pr_stat","surg_prim_site_V1","surg_prim_site_V2","her2_stat",
                         "radiation","reg_age_at_dx","reg_nodes_exam","reg_nodes_pos",
                         "cs_tum_size","cs_tum_ext","chemo","hormone","cs_tum_nodes",
                         "num_nonbc","date_Birth",
                         "DAJCC_T","DAJCC_M","DAJCC_N",
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
    char_df[i,"Race"] <- curr_kcr[,"Race_Recoded"]
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
    char_df[i,"surg_prim_site_V1"] <- curr_kcr[,"RXSummSurgPrimSite_RecodedV1"]
    char_df[i,"surg_prim_site_V2"] <- curr_kcr[,"RXSummSurgPrimSite_RecodedV2"]
    
    char_df[i,"radiation"] <- curr_kcr[,"RXSummRadiation"]
    char_df[i,"chemo"] <- curr_kcr[,"RXSummChemo"]
    char_df[i,"hormone"] <- curr_kcr[,"RXSummHormone"]
    
    char_df[i,"reg_nodes_exam"] <- curr_kcr[,"RegNodesExamined"]
    char_df[i,"reg_nodes_pos"] <- curr_kcr[,"RegNodesPositive"]
    char_df[i,"cs_tum_size"] <- curr_kcr[,"CSTumorSize"]
    char_df[i,"cs_tum_ext"] <- curr_kcr[,"CSTumorSizeExtEval"]
    char_df[i,"reg_age_at_dx"] <- curr_kcr[,"DiagAge"]
    char_df[i,"cs_tum_nodes"] <- curr_kcr[,"CSLymphNodes"]
    
    #Add DAJCC
    char_df[i,"DAJCC_T"] <- curr_kcr[,"DAJCC_T"]
    char_df[i,"DAJCC_M"] <- curr_kcr[,"DAJCC_M"]
    char_df[i,"DAJCC_N"] <- curr_kcr[,"DAJCC_N"]
    
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

#Local
raw_data_dir  <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
Proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

data_dir1  <- paste0(Proj_dir, "4_RecurrDates_Outcome_Info/")
data_dir2  <- paste0(Proj_dir, "7_PrePostLabels_AndAvailibility6mon/")
data_dir3  <- paste0(Proj_dir, "1_ID_Sources_Info/")
outdir     <- paste0(Proj_dir, "8_Characteristics2/Patient_Level/")


#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(data_dir1,"4_All_event_df.xlsx"),sheet = 1)
SBCE_df      <- read.xlsx(paste0(data_dir1,"4_SBCE_Label.xlsx"),sheet = 1)

#########################################################################################################
### 2.  Load patinet char data
#'A. Data 1: uh3_kcrdata.csv
#'B. Data 2  (@Add DerivedSS2000): ky0015_update_DerivedSS2000_andTNM.sas7bdat
#'C. Data 3: (@Updated 12/01/21, TNM) UH3 Nov Update with TNM and Staging.csv
#'@NOTE: Ignore UH3_KCR data_1018Updates.csv, cuz it does not exactly match the old data,(e.g, primary site)
#########################################################################################################
#Kcr data 1
kcr_data <- read.csv(paste0(raw_data_dir, "uh3_kcrdata.csv"),stringsAsFactors = F) #
kcr_data[which(kcr_data$TNMPathT == ""),"TNMPathT"] <- NA
kcr_data[which(kcr_data$TNMClinT == ""),"TNMClinT"] <- NA
kcr_data[which(kcr_data$TNMPathM == ""),"TNMPathM"] <- NA
kcr_data[which(kcr_data$TNMClinM == ""),"TNMClinM"] <- NA
kcr_data[which(kcr_data$TNMPathN == ""),"TNMPathN"] <- NA
kcr_data[which(kcr_data$TNMClinN == ""),"TNMClinN"] <- NA
get_missing_rate_table(kcr_data,c("TNMPathT","TNMClinT",
                                  "TNMPathM","TNMClinM",
                                  "TNMPathN","TNMClinN","SEERSummStg2000"))

#Recode race
kcr_data <- recode_Race_func(kcr_data)

#recode SurgPrimSite
kcr_data <- recode_SPS_func(kcr_data)

#KCr data 2 (Update SEERSummStg)
new_kcr_data2 <- read.sas7bdat(paste0(raw_data_dir, "ky0015_update_DerivedSS2000_andTNM.sas7bdat"),debug = FALSE)
kcr_data <- updated_SEERSummStg2000_func(kcr_data,new_kcr_data2)


#Kcr data 3 (Updated TNM)
new_kcr_data3 <- read.csv(paste0(raw_data_dir, "UH3 Nov Update with TNM and Staging.csv"),stringsAsFactors  = FALSE)
kcr_data <- updated_TNM_func(kcr_data,new_kcr_data3)

get_missing_rate_table(kcr_data,c("TNMPathT","TNMClinT",
                                  "TNMPathM","TNMClinM",
                                  "TNMPathN","TNMClinN","Comb_SEERSummStg"))

#########################################################################################################
#Compute DAJCC_T, DAJCC_M, DAJCC_N
#########################################################################################################
kcr_data$DAJCC_T <- get_DAJCC_var_funtion(kcr_data,"TNMPathT","TNMClinT")
kcr_data$DAJCC_M <- get_DAJCC_var_funtion(kcr_data,"TNMPathM","TNMClinM")
kcr_data$DAJCC_N <- get_DAJCC_var_funtion(kcr_data,"TNMPathN","TNMClinN")

#convert DAJCC variable to numeric code
kcr_data <- convert_DAJCC_var_function(kcr_data, "DAJCC_T")
kcr_data <- convert_DAJCC_var_function(kcr_data, "DAJCC_N")
kcr_data <- convert_DAJCC_var_function(kcr_data, "DAJCC_M")

table(kcr_data$DAJCC_T)
table(kcr_data$DAJCC_N)
table(kcr_data$DAJCC_M)
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
#########################################################################################################
#1. For using all enrollment data with possible months has no codes at all
pts_level_char_df1 <- get_pts_level_char_func(analysis_ID1_Allenrolled,ID_Sources_data,NUM_Month_df1_Allenrolled,All_event_df,kcr_data,All_cancer_site_date_data)
write.xlsx(pts_level_char_df1,paste0(outdir,"8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"))

#1. For using enrollment data with  months has at least one codes
pts_level_char_df2 <- get_pts_level_char_func(analysis_ID2_EnrolledHasCode,ID_Sources_data,NUM_Month_df2_EnrolledHasCodes,All_event_df,kcr_data,All_cancer_site_date_data)
write.xlsx(pts_level_char_df2,paste0(outdir,"8_PatientLevel_char_WithEveryMonthsHasCodes.xlsx"))
