library(lubridate)

compare_OtherDiag_with2ndBCorRecur_func <- function(curr_other_df, second_event_date , date_toCompare,site_toCompare){
  #date_toCompare <- curr_2ndP_date
  #site_toCompare <- "Second_PrimarySite"
  #second_event_date <- curr_recurrence_date
  
  if (is.null(curr_other_df) == T){ # if no other diag, then the 3rd enf is 2nd BC
    third_event_date <- date_toCompare
    third_event_site <- site_toCompare
  }else {  #if there is any other diagnoise, check if any diag before 2nd bc but after 1Rcur
    idxes_after <- which(mdy(curr_other_df[,"Date"]) > second_event_date & 
                           mdy(curr_other_df[,"Date"]) < date_toCompare)
    if (length(idxes_after) > 0 ){
      curr_other_after_1st_df <- curr_other_df[idxes_after,]
      min_idxes <- which(curr_other_after_1st_df[,"Date"] == min(curr_other_after_1st_df[,"Date"]))
      min_date <- curr_other_after_1st_df[min_idxes,"Date"]
      min_site <- curr_other_after_1st_df[min_idxes,"Site"]
      
      third_event_date <- min_date
      third_event_site <- min_site
      
    }else {
      third_event_date <- date_toCompare
      third_event_site <- site_toCompare
    }
    
  }
  return(list(third_event_date,third_event_site))
}



check_OtherDiag_After_Event_func <- function(curr_other_df, date_toCompare){
  
  if (is.null(curr_other_df) == T){ # if no other diag, then the 3rd enf is 2nd BC
    third_event_date <- NA
    third_event_site <- NA
  }else {  #if there is any other diagnoise, check if any diag before 2nd bc but after 1Rcur
    idxes_after <- which(mdy(curr_other_df[,"Date"]) > date_toCompare)
    if (length(idxes_after) > 0 ){
      curr_other_after_1st_df <- curr_other_df[idxes_after,]
      min_idxes <- which(curr_other_after_1st_df[,"Date"] == min(curr_other_after_1st_df[,"Date"]))
      min_date <- curr_other_after_1st_df[min_idxes,"Date"]
      min_site <- curr_other_after_1st_df[min_idxes,"Site"]
      
      third_event_date <- min_date
      third_event_site <- min_site
      
    }else {
      third_event_date <- NA
      third_event_site <- NA
    }
    
  }
  return(list(third_event_date,third_event_site))
}


data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0311_21/"

uh3_kcr_df <- read.csv(paste0(data_dir,"Testing data for UH3 - Dec 16 2020/uh3_kcrdata.csv"),stringsAsFactors = F)
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

#Analysis IDs
unique_IDs <- unique(uh3_kcr_df$study_id)

#For each patient, get first primary breast cancer, second primary breast cancer
PrimaryBC_date_df <- as.data.frame(matrix(NA, nrow = length(unique_IDs),ncol = 11))
colnames(PrimaryBC_date_df) <- c("ID","First_Primary_BC_Date","Second_Primary_BC_Date","Third_Primary_BC_Date",
                                 "First_PrimarySite","Second_PrimarySite","Third_PrimarySite",
                                 "n_1stPrimary_records","n_2ndPrimary_records","n_3ndPrimary_records",
                                 "Date_1Recur")
for (p in 1:length(unique_IDs)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- unique_IDs[p]
  PrimaryBC_date_df[p,"ID"] <- curr_id
  
  #Curre kcr df
  curr_idx <- which(uh3_kcr_df[,"study_id"] == curr_id)
  curr_kcr_df <- uh3_kcr_df[curr_idx,]
  

  #First primary date
  first_pri_bc_idx <- which(curr_kcr_df[,"CentralSequenceNumber"] %in% c(0,1))
  PrimaryBC_date_df[p, "n_1stPrimary_records"] <- length(first_pri_bc_idx)
  if (length(first_pri_bc_idx) > 0 ){  #if has first primary date
     if (length(first_pri_bc_idx) > 1){ #if more than one
       PrimaryBC_date_df[p,"First_Primary_BC_Date"] <- paste0(curr_kcr_df[first_pri_bc_idx,"Date_dx"],collapse = "%%")
       #Date of recurrece of first primary bc
       PrimaryBC_date_df[p, "Date_1Recur"] <- paste0(curr_kcr_df[first_pri_bc_idx,"Date_1Recur"],collapse = "%%")
       PrimaryBC_date_df[p,"First_PrimarySite"] <- paste0(curr_kcr_df[first_pri_bc_idx,"PrimarySite"],collapse = "%%")

     }else{
       PrimaryBC_date_df[p,"First_Primary_BC_Date"] <- curr_kcr_df[first_pri_bc_idx,"Date_dx"]
       PrimaryBC_date_df[p, "Date_1Recur"] <- curr_kcr_df[first_pri_bc_idx,"Date_1Recur"]
       PrimaryBC_date_df[p,"First_PrimarySite"] <- curr_kcr_df[first_pri_bc_idx,"PrimarySite"]
     }
   }
  
  #Second primary date
  second_pri_bc_idx <- which(curr_kcr_df[,"CentralSequenceNumber"] == 2)
  PrimaryBC_date_df[p, "n_2ndPrimary_records"] <- length(second_pri_bc_idx)
  if (length(second_pri_bc_idx) > 0 ){  #if has first primary date
    if (length(second_pri_bc_idx) > 1){ #if more than one
      PrimaryBC_date_df[p,"Second_Primary_BC_Date"] <- paste0(curr_kcr_df[second_pri_bc_idx,"Date_dx"],collapse = "%%")
      PrimaryBC_date_df[p,"Second_PrimarySite"] <- paste0(curr_kcr_df[second_pri_bc_idx,"PrimarySite"],collapse = "%%")

    }else{
      PrimaryBC_date_df[p,"Second_Primary_BC_Date"] <- curr_kcr_df[second_pri_bc_idx,"Date_dx"]
      PrimaryBC_date_df[p,"Second_PrimarySite"] <- curr_kcr_df[second_pri_bc_idx,"PrimarySite"]
      
    }
  }
  
  #Third primary date
  thrid_pri_bc_idx <- which(curr_kcr_df[,"CentralSequenceNumber"] == 3)
  PrimaryBC_date_df[p, "n_3ndPrimary_records"] <- length(thrid_pri_bc_idx)
  if (length(thrid_pri_bc_idx) > 0 ){  #if has third primary date
    if (length(thrid_pri_bc_idx) > 1){ #if more than one
      PrimaryBC_date_df[p,"Third_Primary_BC_Date"] <- paste0(curr_kcr_df[thrid_pri_bc_idx,"Date_dx"],collapse = "%%")
      PrimaryBC_date_df[p,"Third_PrimarySite"] <- paste0(curr_kcr_df[thrid_pri_bc_idx,"PrimarySite"],collapse = "%%")

    }else{
      PrimaryBC_date_df[p,"Third_Primary_BC_Date"] <- curr_kcr_df[thrid_pri_bc_idx,"Date_dx"]
      PrimaryBC_date_df[p,"Third_PrimarySite"] <- curr_kcr_df[thrid_pri_bc_idx,"PrimarySite"]
      
    }
  }
  
  
}



############################################################################################
#Updated IDs and primary bc df
#Remove 1st primay BS == NA , and remove 1st = 2nd dates
############################################################################################
missing_1st_indxes <- which(is.na(PrimaryBC_date_df[,"First_Primary_BC_Date"]) == T)
first_EQ_2nd_index<- which(PrimaryBC_date_df[,"First_Primary_BC_Date"] == PrimaryBC_date_df[,"Second_Primary_BC_Date"])
length(first_EQ_2nd_index)
length(missing_1st_indxes)
updated_PrimaryBC_date_df <- PrimaryBC_date_df[-c(missing_1st_indxes,first_EQ_2nd_index),]
updated_analysis_ID <- unique(updated_PrimaryBC_date_df$ID)

#write.csv(updated_PrimaryBC_date_df,paste0(out_dir,"updated_PrimaryBC_date.csv"))
############################################################################################
#Get All other primary cancer dates every happend
############################################################################################
othercancer_df_list <- list()
for (p in 1:length(updated_analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- updated_analysis_ID[p]

  #Curr kcr df
  curr_idx <- which(uh3_kcr_df[,"study_id"] == curr_id)
  curr_kcr_df <- uh3_kcr_df[curr_idx,]
  

  other_cancer_info_cols <- c("site_o1","site_o2","site_o3","site_o4","site_o5","site_o6")
  other_cancer_date_cols <-c("date_o1","date_o2","date_o3","date_o4","date_o5","date_o6")

  
  #Get all code and dates for each row (patients may have multiple rows info)
  ct <- 1
  curr_other_sites_and_dates_list <- list()
  for (i in 1:nrow(curr_kcr_df)){
    curr_otherCancer_info_df <- curr_kcr_df[i,other_cancer_info_cols]
    curr_otherCancer_dates_df <- curr_kcr_df[i,other_cancer_date_cols]
    
    nonNa_idxes <- which(is.na(curr_otherCancer_info_df) == F)
    if (length(nonNa_idxes) > 0 ){
      other_cancer_sites <- t(curr_otherCancer_info_df[,nonNa_idxes])
      other_cancer_dates <- t(curr_otherCancer_dates_df[nonNa_idxes])
      curr_other_sites_and_dates <- cbind(other_cancer_sites,other_cancer_dates)
      colnames(curr_other_sites_and_dates) <- c("Site","Date")
      rownames(curr_other_sites_and_dates) <- NULL
      curr_other_sites_and_dates_list[[ct]] <- curr_other_sites_and_dates
      ct <- ct + 1
    }
    
  }
  
  if (length(curr_other_sites_and_dates_list) > 0 ){ #only store if they have any other site info
      curr_othercancer_df <- do.call(rbind.data.frame,curr_other_sites_and_dates_list)
      curr_othercancer_df$ID <- curr_id
      curr_othercancer_df <- curr_othercancer_df[!duplicated(curr_othercancer_df),] #remove duplicated
      othercancer_df_list[[p]] <- curr_othercancer_df
  }
}

all_othercancer_df <- do.call(rbind,othercancer_df_list)


############################################################################################
#Get 1st,2nd, 3rd event type and date for each patients
#1st event = 1st primary dates
#2nd event = Second primary dates or recurcen of the 1st  or non-breast cancer
#3rd event =  recurrence or second primary or non-brast canncer ot 34d primary?
############################################################################################
Event_df_list <- list(NA)
for (p in 1:length(updated_analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- updated_analysis_ID[p]

  #Curr updated_PrimaryBC_date_df
  curr_idx <- which(updated_PrimaryBC_date_df[,"ID"] == curr_id)
  curr_updated_PrimaryBC_date_df <- updated_PrimaryBC_date_df[curr_idx,]
  
  #1st primary dates
  curr_1stP_date <- mdy(curr_updated_PrimaryBC_date_df[,"First_Primary_BC_Date"])
  curr_1stP_site <- curr_updated_PrimaryBC_date_df[,"First_PrimarySite"]
  
  #2nd primary dates
  curr_2ndP_date <- mdy(curr_updated_PrimaryBC_date_df[,"Second_Primary_BC_Date"])
  curr_2ndP_site <- curr_updated_PrimaryBC_date_df[,"Second_PrimarySite"]
  
  #3rd primary dates
  curr_3rdP_date <- mdy(curr_updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"])
  curr_3rdP_site <- curr_updated_PrimaryBC_date_df[,"Third_PrimarySite"]
  
  #recurrence date
  curr_recurrence_date <- mdy(curr_updated_PrimaryBC_date_df[,"Date_1Recur"])
  

  
  #Curr other cancer
  curr_idx2 <- which(all_othercancer_df[,"ID"] == curr_id)
  if (length(curr_idx2) > 0 ){
    curr_other_df <- all_othercancer_df[curr_idx2,]
    curr_other_df$Site <- as.vector(curr_other_df$Site)
    curr_other_df$Date <- as.vector(curr_other_df$Date)
    curr_other_df$ID <- as.vector(curr_other_df$ID)
  }else{
    curr_other_df <- NULL
  }
  
  #Frist event must be 1st primary bc dates
  first_event_date <- curr_1stP_date
  first_event_site <- curr_1stP_site
  

  #2nd event  could be recur or secondry primary
  if (is.na(curr_recurrence_date) == T & is.na(curr_2ndP_date) == T ){ #if No 2nd event:
     second_event_date <- NA
     second_event_site <- NA
     third_event_date <- NA
     third_event_site <- NA
  }else if (is.na(curr_recurrence_date) == F & is.na(curr_2ndP_date) == T){
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"
    
    #Compare other with the third 
    if (is.na(curr_3rdP_date) == F){
      date_toCompare <- curr_3rdP_date
      site_toCompare <- curr_3rdP_site
      res <- compare_OtherDiag_with2ndBCorRecur_func(curr_other_df, second_event_date , date_toCompare,site_toCompare)
      third_event_date <- res[[1]]
      third_event_site <- res[[2]]
    }else{
      third_event_date <- NA
      third_event_site <- NA
    }
    

    
  }else if (is.na(curr_recurrence_date) == T & is.na(curr_2ndP_date) == F){
    second_event_date <- curr_2ndP_date
    second_event_site <- curr_2ndP_site
    
    #Compare other with the third 
    if (is.na(curr_3rdP_date) == F){
        date_toCompare <- curr_3rdP_date
        site_toCompare <- curr_3rdP_site
        res <- compare_OtherDiag_with2ndBCorRecur_func(curr_other_df, second_event_date , date_toCompare,site_toCompare)
        third_event_date <- res[[1]]
        third_event_site <- res[[2]]
    }else{
      third_event_date <- NA
      third_event_site <- NA
    }
    
  }else if (curr_recurrence_date < curr_2ndP_date){
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"
    
    #3rd event could be any subsequent diagnoses of breast cancer recurrences or primary breast or non-breast cancers
    #Compare other with curr_2ndP_date
    date_toCompare <- curr_2ndP_date
    site_toCompare <- curr_2ndP_site
    res <- compare_OtherDiag_with2ndBCorRecur_func(curr_other_df, second_event_date , date_toCompare,site_toCompare)
    third_event_date <- res[[1]]
    third_event_site <- res[[2]]
    
  }else if (curr_recurrence_date > curr_2ndP_date){
    second_event_date <- curr_2ndP_date
    second_event_site <- curr_2ndP_site
    
    #Compare other with curr_recurrence_date
    date_toCompare <- curr_recurrence_date
    site_toCompare <- "1Recur"
    res <- compare_OtherDiag_with2ndBCorRecur_func(curr_other_df, second_event_date , date_toCompare,site_toCompare)
    third_event_date <- res[[1]]
    third_event_site <- res[[2]]
    
  }else if (curr_recurrence_date == curr_2ndP_date){
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"
    
    #Compare other with curr_recurrence_date
    date_toCompare <- second_event_date
    res <- check_OtherDiag_After_Event_func(curr_other_df,date_toCompare)
    third_event_date <- res[[1]]
    third_event_site <- res[[2]]
    
  }
  
        


  # #event df
  event_dates <- c(first_event_date,second_event_date,third_event_date)
  event_sites <- c(first_event_site,second_event_site,third_event_site)
  
  Event_df <- cbind.data.frame(as.character(event_dates),event_sites)
  Event_df$ID <- curr_id
  colnames(Event_df) <- c("Date","Site","ID")
  rownames(Event_df) <- paste0("Event_",seq(1,3,1))
  
  Event_df_list[[p]] <- Event_df

}

all_Event_df <- do.call(rbind,Event_df_list)
write.csv(all_Event_df,paste0(out_dir,"all_Event_df.csv"),row.names = F)

############################################################################################
##Outcome
############################################################################################
all_Event_df <- read.csv(paste0(out_dir,"all_Event_df.csv"),stringsAsFactors = F)
analysis_IDs <- unique(all_Event_df$ID)
n_of_event <- NA
evnet_flat_df <- as.data.frame(matrix(NA, nrow = length(analysis_IDs),ncol = 7))
colnames(evnet_flat_df) <- c("Date_Event1","Date_Event2","Date_Event3","Site_Event1","Site_Event2","Site_Event3","ID")
for (i in 1:length(analysis_IDs)){
  if (i%% 1000 ==0){
    print(i)
  }
  curr_id <- analysis_IDs[i]
  curr_event <- all_Event_df[which(all_Event_df$ID == curr_id),]
  evnet_flat_df[i,1:7] <- unlist(curr_event)[1:7]
  n_of_event[i] <- length(which(is.na(curr_event$Date)==F))
}


#First primary BC realted death 
primary_uh3_kcr_df <- uh3_kcr_df[uh3_kcr_df$CentralSequenceNumber %in% c(0,1),]
bc_related_death_IDs <- primary_uh3_kcr_df[which(primary_uh3_kcr_df$CauseOfDeath %in% bc_codes),"study_id"]

evnet_flat_df$FirstPBC_related_Death <- 0
evnet_flat_df[which(evnet_flat_df$ID %in% bc_related_death_IDs),"FirstPBC_related_Death"] <- 1
table(evnet_flat_df$FirstPBC_related_Death) #    0     1  34262  6692 

evnet_flat_df$SBCE <- 0
evnet_flat_df[which(is.na(evnet_flat_df[,"Site_Event2"])==F),"SBCE"] <- 1
table(evnet_flat_df$SBCE) #    0     1  36558  4396 


#both died and recurence
both_died_andRecur_idxes <- which(evnet_flat_df$FirstPBC_related_Death == 1 & evnet_flat_df$SBCE == 1)
length(both_died_andRecur_idxes)
evnet_flat_df$Died_And_Recur <- NA
evnet_flat_df[both_died_andRecur_idxes,"Died_And_Recur"] <- 1
evnet_flat_df[-both_died_andRecur_idxes,"Died_And_Recur"] <- 0
table(evnet_flat_df$Died_And_Recur)     #0     1  38752  2202 

#died but no recurrence 
died_noRecur_idxes <- which(evnet_flat_df$FirstPBC_related_Death == 1 & evnet_flat_df$SBCE == 0)
length(died_noRecur_idxes)

#died with recurrence 
died_noRecur_idxes <- which(evnet_flat_df$FirstPBC_related_Death == 1 & evnet_flat_df$SBCE == 1)
length(died_noRecur_idxes)

#either died and recurence
either_died_andRecur_idxes <- which(evnet_flat_df$FirstPBC_related_Death == 1 | evnet_flat_df$SBCE == 1)
length(either_died_andRecur_idxes)
evnet_flat_df$Died_Or_Recur <- NA
evnet_flat_df[either_died_andRecur_idxes,"Died_Or_Recur"] <- 1
evnet_flat_df[-either_died_andRecur_idxes,"Died_Or_Recur"] <- 0
table(evnet_flat_df$Died_Or_Recur)     #0     1  32068  8886 


#2nd event
length(which(is.na(evnet_flat_df[,"Site_Event2"])==T)) #36558 has no 2nd event
length(which(is.na(evnet_flat_df[,"Site_Event2"])==F)) #4396 has 2nd event

#4247  has 2nd event, no 3rd
only2nd_no3rd_idxes <- which(is.na(evnet_flat_df[,"Site_Event2"])==F & is.na(evnet_flat_df[,"Site_Event3"])==T)
length(only2nd_no3rd_idxes)
only2nd_no3rd_evnet_flat_df <- evnet_flat_df[only2nd_no3rd_idxes,]
length(which(only2nd_no3rd_evnet_flat_df[,"Site_Event2"]== "1Recur"))  #1st->Recur  #3046
length(which(only2nd_no3rd_evnet_flat_df[,"Site_Event2"] %in% bc_codes))  #1st->2nd Primary #1201


#149  both 2nd and 3rd
both23_idxes <- which(is.na(evnet_flat_df[,"Site_Event2"])==F & is.na(evnet_flat_df[,"Site_Event3"])==F)
length(both23_idxes)
both23_evnet_flat_df <- evnet_flat_df[both23_idxes,]
length(which(both23_evnet_flat_df[,"Site_Event2"]== "1Recur" & 
             both23_evnet_flat_df[,"Site_Event3"] %in% bc_codes))  #1st->Recur->2nd Primary #45
length(which(both23_evnet_flat_df[,"Site_Event2"]== "1Recur" & 
            !both23_evnet_flat_df[,"Site_Event3"] %in% bc_codes))  #1st->Recur->other #3
length(which(both23_evnet_flat_df[,"Site_Event2"] %in% bc_codes & 
             both23_evnet_flat_df[,"Site_Event3"] == "1Recur"))  #1st->2nd Primary -> Recur #78
length(which(both23_evnet_flat_df[,"Site_Event2"] %in% bc_codes & 
             both23_evnet_flat_df[,"Site_Event3"] !="1Recur"))  #1st-> 2nd Primary -> 3rd Primary #23
length(which(both23_evnet_flat_df[,"Site_Event2"] %in% bc_codes & 
             both23_evnet_flat_df[,"Site_Event3"] !="1Recur" & 
             !both23_evnet_flat_df[,"Site_Event3"] %in% bc_codes)) #1st-> 2nd Primary -> others #23


check <- both23_evnet_flat_df[which(both23_evnet_flat_df[,"Site_Event2"] %in% bc_codes & 
                  both23_evnet_flat_df[,"Site_Event3"] !="1Recur"),]
