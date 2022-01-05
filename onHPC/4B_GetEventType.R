source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
intermediate_dir <- "/recapse/intermediate_data/4_RecurrDates_Outcome_Info/"
outdir <- "/recapse/intermediate_data/4_RecurrDates_Outcome_Info/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
intermediate_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/4_RecurrDates_Outcome_Info/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/4_RecurrDates_Outcome_Info/"


###########################################################################
###########   1. Load UH3 KCR data                           ##############
###########################################################################
uh3_kcr_df <- read.csv(paste0(data_dir,"uh3_kcrdata.csv"),stringsAsFactors = F)
#Replace blank as NA
uh3_kcr_df[uh3_kcr_df ==""] <- NA

#site01-06 should be for other cancer, but we found code C500-509 in these columns, manually checked SEER, they are other cancers acutally, 
#So manually code them as NA
bc_codes <- paste0("C50",seq(0,9,1))
uh3_kcr_df[which(uh3_kcr_df[,"site_o1"] %in% bc_codes),"site_o1"] <- NA
#Found none in the following
# uh3_kcr_df[which(uh3_kcr_df[,"site_o2"] %in% bc_codes),"site_o2"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o3"] %in% bc_codes),"site_o3"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o4"] %in% bc_codes),"site_o4"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o5"] %in% bc_codes),"site_o5"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o6"] %in% bc_codes),"site_o6"]

###########################################################################
#2. Load last contact dates
###########################################################################
uh3_kcr_extra_df <- read.csv(paste0(data_dir,"uh3_kcrdata_add_datelc.csv"),stringsAsFactors = F)
identical(uh3_kcr_extra_df$study_id, uh3_kcr_df$study_id)
uh3_kcr_df$Date_LC <- uh3_kcr_extra_df[,"Date_LC"]

############################################################################################
#3. Load cancer site df
############################################################################################
merged_cancer_site_df <- read.xlsx(paste0(outdir,"4_All_cancer_site_date_df.xlsx"),sheet = 1)


############################################################################################
#2.Analysis ID
############################################################################################
analysis_ID <- unique(merged_cancer_site_df$study_id) #41375

############################################################################################
#Get 1st,2nd, 3rd event type and date 
#1st event:  1st primary dates
#2nd event:  recurrence of the 1st or second primary or 1st primary bc death
#3rd event:  subsqeunt after 2nd event
############################################################################################
All_event_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 14))
colnames(All_event_df) <- c("study_id",
                            "Date_1st_Event","Date_2nd_Event","Date_3rd_Event",
                            "Site_1st_Event","Site_2nd_Event","Site_3rd_Event",
                            "Type_1st_Event","Type_2nd_Event","Type_3rd_Event",
                            "Date_LC","Primary_1stBC_Death_Date","Days_1stEventTODeath","Days_1stTO2nd")
for (p in 1:length(analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- analysis_ID[p]
  All_event_df[p,"study_id"] <- curr_id
  
  #1.Get kcr info
  curr_idx_inkcr     <- which(uh3_kcr_df[,"study_id"] == curr_id)
  curr_uh3_kcr_df    <- uh3_kcr_df[curr_idx_inkcr,]
  
  #2.Last contact date 
  curr_last_contact_date     <- unique(curr_uh3_kcr_df[,"Date_LC"])

  #3.Get death info
  curr_death_site <- unique(curr_uh3_kcr_df[,"CauseOfDeath"]) 
  curr_death_site <- paste0(curr_death_site,collapse = "$$$") #might have multiple death sites
  
  #4. get current cancer info
  curr_idx <- which(merged_cancer_site_df[,"study_id"] == curr_id)
  curr_cancer_df <- merged_cancer_site_df[curr_idx,]
  
  #5.Get first primary info
  first_primary_info  <- get_cancer_info_func(curr_cancer_df,"First_Primary")
  first_primary_site  <- unlist(strsplit(first_primary_info[[2]],split = "$$$",fixed = T))[1] #due to same dates different type, take the 1st one, it is always the 1st primary

  #6.Check first-primary related death
  if(grepl(first_primary_site ,curr_death_site)== T) {  #if first primary related 
    curr_1st_p_death_site <- curr_death_site
    curr_1st_p_death_date <- curr_last_contact_date
    curr_1st_p_death_type <- "Primary1stBC_related_Death"
  }else{
    curr_1st_p_death_site <- NA
    curr_1st_p_death_date <- NA
    curr_1st_p_death_type <- NA
  }

  #7.Get First-primary related death df
  curr_death_1st_p_df <- as.data.frame(matrix(NA, nrow = 1 , ncol = 4))
  colnames(curr_death_1st_p_df) <- c("Site","Date","Type","study_id")
  if (is.na(curr_1st_p_death_site) == F){
    curr_death_1st_p_df[,"study_id"] <- curr_id
    curr_death_1st_p_df[,"Site"] <- curr_1st_p_death_site
    curr_death_1st_p_df[,"Date"] <- curr_1st_p_death_date
    curr_death_1st_p_df[,"Type"] <- curr_1st_p_death_type
  }else{
    curr_death_1st_p_df <- NULL
  }
  
  #5.Combine cancer info and death info
  curr_cancer_and_death_df <- rbind(curr_cancer_df,curr_death_1st_p_df)
  curr_cancer_and_death_df_ordered <- curr_cancer_and_death_df[order(mdy(curr_cancer_and_death_df$Date), decreasing = F),] #reorder

  #6.Get all event
  #'@1stevent must be 1st primary bc dates
  first_idx <- which(grepl("First_Primary",curr_cancer_and_death_df_ordered[,"Type"]) == T) #use grepl, due to merged process, as long as it contains First primary, we consider it, just ignore the other ones
  All_event_df[p,"Site_1st_Event"] <- curr_cancer_and_death_df_ordered[first_idx,"Site"]
  
  All_event_df[p,"Date_1st_Event"] <- curr_cancer_and_death_df_ordered[first_idx,"Date"]
  All_event_df[p,"Type_1st_Event"] <- curr_cancer_and_death_df_ordered[first_idx,"Type"]
  
  #'@2ndevent  could be 1st primary bc death OR 
  #'recur OR secondry primary OR merged both (happened at the same time)
  curr_cancer_and_death_df_ordered <- curr_cancer_and_death_df_ordered[-first_idx,] #remove the first index, due to the case "First and Second" being merged, aslongas it is first, it was the 1st event
  second_idx <- which(grepl("Second_Primary|1Recur|Primary1stBC_related_Death",curr_cancer_and_death_df_ordered[,"Type"]) == T)[1]  # index [1]choose the first one as the 2nd event, then the next one is the 3rd event. (e.g,2nd date <  1Recur)
  
  
  
  if (length(second_idx) == 0){ #if no 2nd event, then no 3rd 
    All_event_df[p,"Site_2nd_Event"] <- NA
    All_event_df[p,"Date_2nd_Event"] <- NA
    All_event_df[p,"Type_2nd_Event"] <- NA
    All_event_df[p,"Site_3rd_Event"] <- NA
    All_event_df[p,"Date_3rd_Event"] <- NA
    All_event_df[p,"Type_3rd_Event"] <- NA
  }else {
    All_event_df[p,"Site_2nd_Event"] <- curr_cancer_and_death_df_ordered[second_idx,"Site"]
    All_event_df[p,"Date_2nd_Event"] <- curr_cancer_and_death_df_ordered[second_idx,"Date"]
    All_event_df[p,"Type_2nd_Event"] <- curr_cancer_and_death_df_ordered[second_idx,"Type"]
    
    #'@ThridEvent  #3rd event could be any subsequent diagnoses/death after 2nd event 
    #'#the diagnoses could be a recurrences or primary breast or non-breast cancers)
    second_event_date <-  All_event_df[p,"Date_2nd_Event"]
    subsequent_idx <- which(mdy(curr_cancer_and_death_df_ordered$Date) > mdy(second_event_date))[1] #since it is ordered, so the first one is the subsquent one
    
    if (length(subsequent_idx) > 0){ #if there are subsqeuent one
      All_event_df[p,"Site_3rd_Event"] <- curr_cancer_and_death_df_ordered[subsequent_idx,"Site"]
      All_event_df[p,"Date_3rd_Event"] <- curr_cancer_and_death_df_ordered[subsequent_idx,"Date"]
      All_event_df[p,"Type_3rd_Event"] <- curr_cancer_and_death_df_ordered[subsequent_idx,"Type"]
    }else{
      All_event_df[p,"Site_3rd_Event"] <- NA
      All_event_df[p,"Date_3rd_Event"] <- NA
      All_event_df[p,"Type_3rd_Event"] <- NA
    }
  }
  
  #8.Last contact date
  All_event_df[p,"Date_LC"] <- curr_last_contact_date
  
  #9.Get time from 1st event to 1st primary bc related death
  All_event_df[p,"Primary_1stBC_Death_Date"] <- curr_1st_p_death_date #record this cuz the death date might not be 2nd or 3rd event
  All_event_df[p,"Days_1stEventTODeath"]     <-   as.numeric(difftime(mdy(curr_1st_p_death_date),
                                                                  mdy(All_event_df[p,"Date_1st_Event"]),units = "days"))
  
  #10.Get time from 1st event to 2nd event
  All_event_df[p,"Days_1stTO2nd"] <- as.numeric(difftime(mdy(All_event_df[p,"Date_2nd_Event"]),
                                                         mdy(All_event_df[p,"Date_1st_Event"]),units = "days"))
}



#Include death as the type if it is the same as event date, but the event type
sameAs1st_indexes <- which(All_event_df$Primary_1stBC_Death_Date == All_event_df$Date_1st_Event & 
                           All_event_df$Type_1st_Event != "Primary1stBC_related_Death")
sameAs2nd_indexes <- which(All_event_df$Primary_1stBC_Death_Date == All_event_df$Date_2nd_Event & 
                           All_event_df$Type_2nd_Event != "Primary1stBC_related_Death")
sameAs3rd_indexes <- which(All_event_df$Primary_1stBC_Death_Date == All_event_df$Date_3rd_Event & 
                           All_event_df$Type_3rd_Event != "Primary1stBC_related_Death")

All_event_df[sameAs1st_indexes,"Type_1st_Event"] <- paste0(All_event_df[sameAs1st_indexes,"Type_1st_Event"],"$$$","Primary1stBC_related_Death")
All_event_df[sameAs2nd_indexes,"Type_2nd_Event"] <- paste0(All_event_df[sameAs2nd_indexes,"Type_2nd_Event"],"$$$","Primary1stBC_related_Death")
All_event_df[sameAs3rd_indexes,"Type_3rd_Event"] <- paste0(All_event_df[sameAs3rd_indexes,"Type_3rd_Event"],"$$$","Primary1stBC_related_Death")

####################################################################################################
#'@NOTE:  The 2nd event (recur or 2nd primary or death) must occur 6 month (6*30 = 180 days) after 1st event
# Exclude patients who has 2nd event within 6 month after 1st event
####################################################################################################
exclude_idexes <- which(All_event_df[,"Days_1stTO2nd"] < 180) #excluded 1051
All_event_df <- All_event_df[-exclude_idexes,]

write.xlsx(All_event_df,paste0(outdir,"4_All_event_df.xlsx"))

############################################################################################
#Just checking if any 1st event happens before 2nd event
############################################################################################
which(mdy(All_event_df$Date_1st_Event) >= mdy(All_event_df$Date_2nd_Event))
which(mdy(All_event_df$Date_1st_Event) >= mdy(All_event_df$Date_3rd_Event))
which(mdy(All_event_df$Date_2nd_Event) >= mdy(All_event_df$Date_3rd_Event))

