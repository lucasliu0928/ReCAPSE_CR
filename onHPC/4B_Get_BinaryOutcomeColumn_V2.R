library(lubridate)
library(openxlsx)

#onHPC
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/recapse/intermediate_data/"


#local
data_dir <- "/Volumes/LJL_ExtPro/Data/Testing data for UH3 - Dec 16 2020/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

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
#4. First Primary BC related death Flag
#5. Any BC related death Flag
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
      
    #get first primary site
    curr_1st_event_site <- curr_event_df[,"Site_1st_Event"]
    curr_1st_event_site <- unlist(strsplit(curr_1st_event_site,split = "$$$",fixed = T))
    
    #get first primary type 
    curr_1st_event_type <- curr_event_df[,"Type_1st_Event"]
    curr_1st_event_type <- unlist(strsplit(curr_1st_event_type,split = "$$$",fixed = T))
    
    #only get first primary 
    pri_1st_indx <- which(curr_1st_event_type == "First_Primary")
    curr_first_primary_site <- curr_1st_event_site[pri_1st_indx]
    
    #make sure it is in bc_code
    curr_first_primary_site <- curr_first_primary_site[which(curr_first_primary_site %in% bc_codes)] 
    

    #check if any BC related death site
    if (grepl(paste0(bc_codes,collapse = "|"),curr_death_site)==T ){ 
      All_event_df[i,"BC_related_Death"] <- 1
    }else{
      All_event_df[i,"BC_related_Death"] <- 0
    }
  
     #Check if it is first priamry related death
    if(grepl(paste0(curr_first_primary_site,collapse = "|") ,curr_death_site)==T ){  #might have multiple bc primary
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

#'@TODO
####################################################################################################
#' Add SBCE flag
#'@1.Definition: 
#' Has 2nd event (Either 1. Recurrence of the first primary breast cancer  
#'                Or     2. the diagnosis of a second primary breast cancer
#'                Or     3. first primary BC related death
#'@2.NOTE:  The 2nd event or death must occur 6 month (6*30 = 180 days) after 1st event
#'#'@NOTE: there is a bug in previous code, Primary_C509$$$C502	death site C509, was not count for First priamry bc death
####################################################################################################
#1.definition flag
All_event_df$SBCE <- 0
sbce_idexes <- which(is.na(All_event_df[,"Date_2nd_Event"])==F | 
                     All_event_df[,"First_Primary_BC_related_Death"] == 1)
All_event_df[sbce_idexes,"SBCE"] <- 1
table(All_event_df$SBCE) #    0:35485  1:5890  

#2. exclude patients who has 2nd event or death within 6 month after 1st event
exclude_idexes <- which(All_event_df[,"SBCE"] == 1 & 
                          (All_event_df[,"Days_1stTO2nd"] < 180 |
                             All_event_df[,"Days_1stEventTODeath"] < 180))
All_event_df <- All_event_df[-exclude_idexes,]
table(All_event_df$SBCE) #    0:35485  1:  4839  




write.csv(All_event_df,paste0(outdir,"4_updated_All_event_df.csv"),row.names = F)


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



