library(lubridate)
library(openxlsx)

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


# #local
# data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

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
uh3_kcr_df$Date_LC <- uh3_kcr_extra_df[,"Date_LC"]

###########################################################################
#3. Load all event df
###########################################################################
All_event_df <- read.xlsx(paste0(outdir,"4_All_event_df.xlsx"),sheet = 1)


############################################################################################
#4. Get analysis ID
############################################################################################
analysis_IDs <- unique(All_event_df$study_id)

############################################################################################
##5.Add 
#1. Cause of death site
#2. Date of death  (Last contact Date)
#3. days from 1st event to death date 
############################################################################################
All_event_df$Death_Site <- NA
All_event_df$Date_death <- NA
All_event_df$Days_1stEventTODeath <- NA
for (i in 1:nrow(All_event_df)){
  if (i %% 1000 == 0){
    print(i)
  }
  #curr event df
  curr_event_df <- All_event_df[i,]
  
  curr_id <- curr_event_df[,"study_id"]
  
  #curr uh3_kcr_df 
  curr_uh3_kcr_df <- uh3_kcr_df[which(uh3_kcr_df[,"study_id"] == curr_id),]
  curr_1st_event_date <- curr_event_df[,"Date_1st_Event"]
  
  #Last contact date 
  curr_last_contact_date <- unique(curr_uh3_kcr_df[,"Date_LC"])
  
  #Death site
  curr_death_site <- unique(curr_uh3_kcr_df[,"CauseOfDeath"]) 
  curr_death_site <- paste0(curr_death_site,collapse = "$$$") #might have multiple death sites
  if (is.na(curr_death_site) == F){
    All_event_df[i,"Death_Site"] <- curr_death_site
    All_event_df[i,"Date_death"] <- curr_last_contact_date
    All_event_df[i,"Days_1stEventTODeath"] <-   difftime(mdy(curr_last_contact_date),mdy(curr_1st_event_date),units = "days")
  }else{
    All_event_df[i,"Death_Site"] <- NA
    All_event_df[i,"Date_death"] <- NA
  }
}

############################################################################################
#6.Add
#1. First Primary BC related death Flag
#2. Any BC related death Flag
############################################################################################
All_event_df$BC_related_Death <- 0
All_event_df$First_Primary_BC_related_Death<-0
for (i in 1:nrow(All_event_df)){
    if (i %% 1000 == 0){
      print(i)
    }
    
    #curr event df
    curr_event_df <- All_event_df[i,]
    curr_id <- curr_event_df[,"study_id"]
    
    #get current death site
    curr_death_site <- curr_event_df[,"Death_Site"]
      
    #get first event site
    curr_1st_event_site <- curr_event_df[,"Site_1st_Event"]
    curr_1st_event_site <- unlist(strsplit(curr_1st_event_site,split = "$$$",fixed = T))
    
    #get first event type  (due to merging effect, this step is needed, e.g, "First_Primary$$$Second_Primary")
    curr_1st_event_type <- curr_event_df[,"Type_1st_Event"]
    curr_1st_event_type <- unlist(strsplit(curr_1st_event_type,split = "$$$",fixed = T))
    
    #only get first primary (due to merging effect, this step is needed)
    pri_1st_indx <- which(curr_1st_event_type == "First_Primary")
    curr_first_primary_site <- curr_1st_event_site[pri_1st_indx]
    
    #make sure it is in bc_code
    curr_first_primary_site <- curr_first_primary_site[which(curr_first_primary_site %in% bc_codes)] 
    

    #check if any BC related death site
    if (grepl(paste0(bc_codes,collapse = "|"),curr_death_site)==T ){ #if curr_death_site contains any bc_codes
      All_event_df[i,"BC_related_Death"] <- 1
    }else{
      All_event_df[i,"BC_related_Death"] <- 0
    }
  
     #Check if it is first priamry related death
    if(grepl(paste0(curr_first_primary_site,collapse = "|") ,curr_death_site)==T ){  #if curr_death_site contains any of 1st primary site(might be multiple primary)
      All_event_df[i,"First_Primary_BC_related_Death"] <- 1
    }else{
      All_event_df[i,"First_Primary_BC_related_Death"] <- 0
    }
  
  
}


####################################################################################################
#7.Add  days from 1stEvent to 2ndEvent
####################################################################################################
All_event_df$Days_1stTO2nd <- difftime(mdy(All_event_df[,"Date_2nd_Event"]),
                                       mdy(All_event_df[,"Date_1st_Event"]),units = "days")

####################################################################################################
#'@NOTE:  The 2nd event or death must occur 6 month (6*30 = 180 days) after 1st event
# Exclude patients who has 2nd event or death within 6 month after 1st event
####################################################################################################
exclude_idexes <- which(All_event_df[,"Days_1stTO2nd"] < 180 |
                        All_event_df[,"Days_1stEventTODeath"] < 180) #excluded 1988
All_event_df <- All_event_df[-exclude_idexes,]

#'@TODO
####################################################################################################
#8. Add SBCE outcome flag
#'@1.Definition: 
#' Has 2nd event (Either 1. Recurrence of the first primary breast cancer  
#'                Or     2. the diagnosis of a second primary breast cancer
#'                Or     3. first primary BC related death
#'#'@NOTE: there is a bug in previous code, Primary_C509$$$C502	death site C509, was not count for First priamry bc death
####################################################################################################
#1.definition flag
All_event_df$SBCE <- 0
sbce_idexes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F | All_event_df[,"First_Primary_BC_related_Death"] == 1)
All_event_df[sbce_idexes,"SBCE"] <- 1
table(All_event_df$SBCE) #    0:34548  1:4839  

write.csv(All_event_df,paste0(outdir,"4_updated_All_event_df.csv"),row.names = F)


####################################################################################################
#Report numbers
####################################################################################################
#1.first primary died with SBCE
All_event_df$Died_And_SBCE <- NA
both_died_andRecur_idxes <- which(All_event_df[,"First_Primary_BC_related_Death"] == 1 & All_event_df[,"SBCE"] == 1)
All_event_df[both_died_andRecur_idxes,"Died_And_SBCE"] <- 1
All_event_df[-both_died_andRecur_idxes,"Died_And_SBCE"] <- 0
table(All_event_df$Died_And_SBCE)     #38195  1192   

#2.first primary died without SBCE 
died_noRecur_idxes <- which(All_event_df[,"First_Primary_BC_related_Death"] == 1 & All_event_df[,"SBCE"] == 0)
length(died_noRecur_idxes) #0

#3.has 2nd event, no 3rd
only2nd_no3rd_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & is.na(All_event_df[,"Date_3rd_Event"])==T)
length(only2nd_no3rd_idxes) #3634
only2nd_no3rd_df <- All_event_df[only2nd_no3rd_idxes,]
table(only2nd_no3rd_df[,"Type_2nd_Event"])
#3.1 1st->Recur -> no 3rd    #2850
#3.2 1st->2nd Primary -> no 3rd    784


#4. both 2nd and 3rd
both23_idxes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F & is.na(All_event_df[,"Date_3rd_Event"])==F)
length(both23_idxes) #287
both23_df <- All_event_df[both23_idxes,]
#4.1 2nd event = recur (145)
both23_2ndEvent_Recur_indxes <- which(grepl("1Recur",both23_df[,"Type_2nd_Event"]) == T)
both23_2ndEvent_Recur_df <- both23_df[both23_2ndEvent_Recur_indxes,]
#4.1 1 (1st -> Recur-> primary #42)
length(which(grepl("Primary",both23_2ndEvent_Recur_df[,"Type_3rd_Event"])==T)) 
#4.1 2 (1st -> Recur-> other #103)
length(which(grepl("Other",both23_2ndEvent_Recur_df[,"Type_3rd_Event"])==T)) 

#4.2 2nd event = 2nd Primary (142)
both23_2ndEvent_Primary_df <- both23_df[-both23_2ndEvent_Recur_indxes,]
#4.1 1 (1st -> 2nd Primary -> Recur #51)
third_event_recur_indxes <- which(grepl("1Recur",both23_2ndEvent_Primary_df[,"Type_3rd_Event"])==T)
length(third_event_recur_indxes)

#4.1 2 (1st -> 2nd Primary -> other #72)
third_event_other_indxes <- which(grepl("Other",both23_2ndEvent_Primary_df[,"Type_3rd_Event"])==T)
length(third_event_other_indxes)

#4.1 3 (1st -> 2nd Primary -> Primary #19)
third_event_primary_df <- both23_2ndEvent_Primary_df[-c(third_event_recur_indxes,third_event_other_indxes),]
nrow(third_event_primary_df)
