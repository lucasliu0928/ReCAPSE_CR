data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/"

###########################################################################
###########   1. Load UH3 KCR data                           ##############
###########################################################################
uh3_kcr_df <- read.csv(paste0(data_dir,"Testing data for UH3 - Dec 16 2020/uh3_kcrdata.csv"),stringsAsFactors = F)
#Replace blank as NA
uh3_kcr_df[uh3_kcr_df ==""] <- NA


#site01-06 should be for other cancer, but we found code C500-509 in these columns, manually checked SEER, they are other cancers acutally, 
#So manually code them as NA
bc_codes <- paste0("C50",seq(0,9,1))
uh3_kcr_df[which(uh3_kcr_df[,"site_o1"] %in% bc_codes),"site_o1"] <- NA

###########################################################################
#2. Load last contact dates
###########################################################################
uh3_kcr_extra_df <- read.csv(paste0(data_dir,"Testing data for UH3 - Dec 16 2020/uh3_kcrdata_add_datelc.csv"),stringsAsFactors = F)
uh3_kcr_df$Date_LC <- uh3_kcr_extra_df[,"Date_LC"]

###########################################################################
#3. Load all event df
###########################################################################
All_event_df <- read.csv(paste0(out_dir,"All_event_df.csv"),stringsAsFactors = F)

############################################################################################
##4.
#Add First Primary BC related death
#Add Any BC related death
#Add Cause of death site
#Add Date of Last contact
############################################################################################
analysis_IDs <- unique(All_event_df$ID)
All_event_df$BC_related_Death <- 0
All_event_df$BC_related_Death_Site <- NA
All_event_df$First_Primary_BC_related_Death<-0
All_event_df$Date_LC <- NA

for (i in 1:length(analysis_IDs)){
  if (i %% 1000 == 0){
    print(i)
  }
  curr_id <- analysis_IDs[i]
  
  curr_first_primary_site <- All_event_df[which(All_event_df[,"ID"] == curr_id),"Site_1st_Event"]
  curr_first_primary_site <- gsub("Primary_","",curr_first_primary_site)
  
  curr_idx <- which(uh3_kcr_df[,"study_id"] == curr_id)
  
  #Last contact date
  All_event_df[i,"Date_LC"] <- unique(uh3_kcr_df[curr_idx,"Date_LC"])
  
  #Death site
  curr_death_site <- unique(uh3_kcr_df[curr_idx,"CauseOfDeath"]) 
  
  if (length(curr_death_site) > 1){ #might have multiple death sites
    curr_death_site <- paste0(curr_death_site,collapse = "$$$")
  }

  if ( grepl(paste0(bc_codes,collapse = "|"),curr_death_site)==T ){ #if any BC related death site
    All_event_df[i,"BC_related_Death"] <- 1
    All_event_df[i,"BC_related_Death_Site"] <- curr_death_site
    
    if( grepl(curr_first_primary_site,curr_death_site)==T ){ #if it is first priamry related death
      All_event_df[i,"First_Primary_BC_related_Death"] <- 1
    }else{
      All_event_df[i,"First_Primary_BC_related_Death"] <- 0
    }
  }else{
    All_event_df[i,"BC_related_Death"] <- 0
    All_event_df[i,"BC_related_Death_Site"] <- NA
    All_event_df[i,"First_Primary_BC_related_Death"] <- 0
  }
  
}

####################################################################################################
#Add a flag for the 3rd event type (other cancer or breast cancer event (recur or primary))
####################################################################################################
All_event_df$Event3_Flag <- NA
no3rd_idxes <- which(is.na(All_event_df[,"Site_3rd_Event"])==T)
All_event_df$Event3_Flag[no3rd_idxes] <- "No 3rd"
priBC_3rd_idxes <- which(grepl("Primary",All_event_df[,"Site_3rd_Event"])==T)
All_event_df[priBC_3rd_idxes,"Event3_Flag"] <- "Primary BC"
recurBC_3rd_idxes <- which(All_event_df[,"Site_3rd_Event"]== "1Recur")
All_event_df[recurBC_3rd_idxes,"Event3_Flag"] <- "1Recur"
All_event_df[-c(no3rd_idxes,priBC_3rd_idxes,recurBC_3rd_idxes),"Event3_Flag"] <- "Other Cancer"

####################################################################################################
#Compute    For all BC related death
#           1. Date of death  (Last contact Date)
#           2. days from 1st event to death date 
####################################################################################################
All_event_df$Date_death <- NA
for (i in 1:nrow(All_event_df)){
  curr_death <- All_event_df[i,"BC_related_Death"]
  if (curr_death == 1){
    All_event_df[i,"Date_death"] <- All_event_df[i,"Date_LC"]
  }
}

All_event_df$Days_1stEventTODeath <- difftime(mdy(All_event_df[,"Date_death"]),
                                              mdy(All_event_df[,"Date_1st_Event"]),units = "days")

####################################################################################################
#Compute     days from 1stEvent to 2ndEvent
####################################################################################################
All_event_df$Days_1stTO2nd <- difftime(mdy(All_event_df[,"Date_2nd_Event"]),
                                       mdy(All_event_df[,"Date_1st_Event"]),units = "days")


####################################################################################################
#' Add SBCE flag
#'@1.Definition: 
#' Has 2nd event (Either 1. Recurrence of the first primary breast cancer  
#'                Or     2. the diagnosis of a second primary breast cancer
#'                Or     3. first primary BC related death
#'@2.NOTE:  The 2nd event or death must occur 6 month (6*30 = 180 days) after,
#'#'#'@NOTE: there is a bug in this code, Primary_C509$$$C502	death site C509, was not count for First priamry bc , corrected in code in HPC folder
####################################################################################################
#1.definition flag
All_event_df$SBCE <- 0
sbce_idexes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F | 
                       All_event_df[,"First_Primary_BC_related_Death"] == 1)
All_event_df[sbce_idexes,"SBCE"] <- 1
table(All_event_df$SBCE) #    0:35511  1: 5864 

#2. exclude patients who has 2nd event or death within 6 month after 1st event
exclude_idexes <- which(All_event_df[,"SBCE"] == 1 & 
                          (All_event_df[,"Days_1stTO2nd"] < 180 |
                           All_event_df[,"Days_1stEventTODeath"] < 180))
All_event_df <- All_event_df[-exclude_idexes,]
table(All_event_df$SBCE) #    0:35511  1:  4818  


write.csv(All_event_df,paste0(out_dir,"updated_All_event_df.csv"),row.names = F)


####################################################################################################
#report numbers
####################################################################################################
#first primary died with SBCE
All_event_df$Died_And_Recur <- NA
both_died_andRecur_idxes <- which(All_event_df[,"First_Primary_BC_related_Death"] == 1 & All_event_df[,"SBCE"] == 1)
All_event_df[both_died_andRecur_idxes,"Died_And_Recur"] <- 1
All_event_df[-both_died_andRecur_idxes,"Died_And_Recur"] <- 0
table(All_event_df$Died_And_Recur)     #0     1  41076   299  

#first primary died without SBCE 
died_noRecur_idxes <- which(All_event_df[,"First_Primary_BC_related_Death"] == 1 & All_event_df[,"SBCE"] == 0)
length(died_noRecur_idxes)



#has 2nd event, no 3rd
only2nd_no3rd_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & is.na(All_event_df[,"Date_3rd_Event"])==T)
length(only2nd_no3rd_idxes) #3932
only2nd_no3rd_df <- All_event_df[only2nd_no3rd_idxes,]
length(which(only2nd_no3rd_df[,"Site_2nd_Event"]== "1Recur"))  #1st->Recur  #2964
length(which(grepl("Primary",only2nd_no3rd_df[,"Site_2nd_Event"]) == T))  #1st->2nd Primary #1104


#149  both 2nd and 3rd
both23_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & is.na(All_event_df[,"Date_3rd_Event"])==F)
length(both23_idxes) #352
both23_df <- All_event_df[both23_idxes,]


####2nd event = recur ( 154)
event2_recur_idxes <- which(both23_df[,"Site_2nd_Event"]== "1Recur")
both23_event2_recur_df <- both23_df[event2_recur_idxes,]
#Recur->2nd Primary #44
length(which(grepl("Primary",both23_event2_recur_df[,"Site_3rd_Event"])==T)) 
#1st->Recur->other #110
length(which(grepl("Primary",both23_event2_recur_df[,"Site_3rd_Event"])==F)) 

####2nd event = 2nd Primary (202)
event2_pri_idxes <- which(grepl("Primary",both23_df[,"Site_2nd_Event"])==T)
both23_event2_pri_df <- both23_df[event2_pri_idxes,]
#1st-> 2nd Primary -> Primary Breast cancer #21
length(which(grepl("Primary",both23_event2_pri_df[,"Site_3rd_Event"])==T)) 
#1st->2nd Primary -> Recur #78
length(which(both23_event2_pri_df[,"Site_3rd_Event"] == "1Recur"))

#1st-> 2nd Primary -> others #99
length(which(both23_event2_pri_df[,"Site_3rd_Event"] != "1Recur" & 
               grepl("Primary",both23_event2_pri_df[,"Site_3rd_Event"])==F))



