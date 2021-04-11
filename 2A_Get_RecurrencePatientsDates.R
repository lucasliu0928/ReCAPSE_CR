library(lubridate)

get_primary_dates_site_func <- function(curr_primarybc_date_df,date_cols,site_cols){
  # date_cols <- all_primary_dates_cols
  # site_cols <- all_primary_sites_cols
  # curr_primarybc_date_df <- curr_updated_PrimaryBC_date_df
  # 
  all_primary_dates <- NA
  all_primary_sites <- NA
  for (i in 1:length(date_cols)){
    curr_col1 <- date_cols[i]
    curr_col2 <- site_cols[i]
    all_primary_dates[i] <- curr_primarybc_date_df[,curr_col1]
    all_primary_sites[i] <- curr_primarybc_date_df[,curr_col2]
  }
  
  curr_dates_and_site <- cbind.data.frame(all_primary_sites,all_primary_dates)
  rownames(curr_dates_and_site) <- c("Primary1","Primary2","Primary3","Primary4","Primary5","Primary6")
  colnames(curr_dates_and_site) <- c("Site","Date")
  
  curr_dates_and_site[] <- lapply(curr_dates_and_site, as.character) #unfacter the columns
  
  #Add prefix indicate primary or not
  non_na_idxes <- which(is.na(curr_dates_and_site[,"Site"])==F)
  if (length(non_na_idxes) > 0){
    curr_dates_and_site[non_na_idxes,"Site"] <- paste0("Primary_",curr_dates_and_site[non_na_idxes,"Site"])
  }
  
  return(curr_dates_and_site)
}


get_othercaner_dates_site_func <- function(all_othercancer_df,pt_id){
  curr_idx <- which(all_othercancer_df[,"ID"] == pt_id)
  if (length(curr_idx) > 0 ){
    curr_other_df <- all_othercancer_df[curr_idx,]
    curr_other_df$Site <- as.vector(curr_other_df$Site)
    curr_other_df$Date <- as.vector(curr_other_df$Date)
    curr_other_df$ID <- as.vector(curr_other_df$ID)
    rownames(curr_other_df) <- paste0("Other",seq(1,nrow(curr_other_df)))
    
  }else{
      curr_other_df <- cbind.data.frame(NA,NA,pt_id)
      colnames(curr_other_df) <-c("Site","Date","ID")
      rownames(curr_other_df) <- "Other"
  }
  
  curr_other_df[] <- lapply(curr_other_df, as.character) #unfacter the columns
  
  return(curr_other_df)
}


data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/"
out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0318_21/"

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
#Found none in the following
# uh3_kcr_df[which(uh3_kcr_df[,"site_o2"] %in% bc_codes),"site_o2"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o3"] %in% bc_codes),"site_o3"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o4"] %in% bc_codes),"site_o4"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o5"] %in% bc_codes),"site_o5"]
# uh3_kcr_df[which(uh3_kcr_df[,"site_o6"] %in% bc_codes),"site_o6"]

###########################################################################
###########   2. Analysis IDs                                   ###########  
###########################################################################
unique_IDs <- unique(uh3_kcr_df$study_id)

##################################################################################################
# 3. Get first primary, second primary, 3rd primary, and 4th primary breast cancer Site and date
##################################################################################################
PrimaryBC_date_df <- as.data.frame(matrix(NA, nrow = length(unique_IDs),ncol = 20))
colnames(PrimaryBC_date_df) <- c("ID","First_Primary_BC_Date","Second_Primary_BC_Date","Third_Primary_BC_Date", 
                                 "Fourth_Primary_BC_Date","Fifth_Primary_BC_Date","Six_Primary_BC_Date",
                                 "First_PrimarySite","Second_PrimarySite","Third_PrimarySite", 
                                 "Fourth_PrimarySite","Fifth_PrimarySite","Sixth_PrimarySite",
                                 "n_1stPrimary_records","n_2ndPrimary_records","n_3ndPrimary_records",
                                 "n_4thPrimary_records","n_5thPrimary_records","n_6thPrimary_records",
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
  
  #Four primary date
  fourth_pri_bc_idx <- which(curr_kcr_df[,"CentralSequenceNumber"] == 4)
  PrimaryBC_date_df[p, "n_4thPrimary_records"] <- length(fourth_pri_bc_idx)
  if (length(fourth_pri_bc_idx) > 0 ){  #if has 4th primary date
    if (length(fourth_pri_bc_idx) > 1){ #if more than one
      PrimaryBC_date_df[p,"Fourth_Primary_BC_Date"] <- paste0(curr_kcr_df[fourth_pri_bc_idx,"Date_dx"],collapse = "%%")
      PrimaryBC_date_df[p,"Fourth_PrimarySite"] <- paste0(curr_kcr_df[fourth_pri_bc_idx,"PrimarySite"],collapse = "%%")
    }else{
      PrimaryBC_date_df[p,"Fourth_Primary_BC_Date"] <- curr_kcr_df[fourth_pri_bc_idx,"Date_dx"]
      PrimaryBC_date_df[p,"Fourth_PrimarySite"] <- curr_kcr_df[fourth_pri_bc_idx,"PrimarySite"]
      
    }
  }
}



############################################################################################
#4. Remove 1st primay BS == NA 
# Updated IDs and primary bc df
############################################################################################
missing_1st_indxes <- which(is.na(PrimaryBC_date_df[,"First_Primary_BC_Date"]) == T)
length(missing_1st_indxes) #5755
updated_PrimaryBC_date_df <- PrimaryBC_date_df[-missing_1st_indxes,]
updated_analysis_ID <- unique(updated_PrimaryBC_date_df$ID) # 41375

############################################################################################
#'@NOTE: if 1st = 2nd or 2nd=3rd date and etc, treat them as one event
#so we remove the latter event date and merge the site for computation purposes
############################################################################################
#1st = 2nd cases, #421 treat them as one event, 
equal1_2_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Second_Primary_BC_Date"])
for (i in 1:length(equal1_2_idxes)){
  curr_idx <- equal1_2_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Second_Primary_BC_Date"] <- NA
  curr_2nd_site <- updated_PrimaryBC_date_df[curr_idx,"Second_PrimarySite"]
  curr_1st_site <- updated_PrimaryBC_date_df[curr_idx,"First_PrimarySite"]
  updated_PrimaryBC_date_df[curr_idx,"First_PrimarySite"] <- paste0(curr_1st_site,"$$$",curr_2nd_site)
  updated_PrimaryBC_date_df[curr_idx,"Second_PrimarySite"] <-NA
} 

#1st = 3rd cases #1
equal1_3_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"])
for (i in 1:length(equal1_3_idxes)){
  curr_idx <- equal1_3_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Third_Primary_BC_Date"] <- NA
  curr_3rd_site <- updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"]
  curr_1st_site <- updated_PrimaryBC_date_df[curr_idx,"First_PrimarySite"]
  updated_PrimaryBC_date_df[curr_idx,"First_PrimarySite"] <- paste0(curr_1st_site,"$$$",curr_3rd_site)
  updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"] <-NA
} 

#1st = 4th cases: none
equal1_4_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Fourth_Primary_BC_Date"])

#1st = 5th cases: none
equal1_5_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Fifth_Primary_BC_Date"])
equal1_6_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Six_Primary_BC_Date"])


#2nd = 3rd cases :4 
equal2_3_idxes <- which(updated_PrimaryBC_date_df[,"Second_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"])
for (i in 1:length(equal2_3_idxes)){
  curr_idx <- equal2_3_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Third_Primary_BC_Date"] <- NA
  curr_3rd_site <- updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"]
  curr_2nd_site <- updated_PrimaryBC_date_df[curr_idx,"Second_PrimarySite"]
  updated_PrimaryBC_date_df[curr_idx,"Second_PrimarySite"] <- paste0(curr_2nd_site,"$$$",curr_3rd_site)
  updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"] <-NA
}

#3rd = 4th cases: 1
equal3_4_idxes <- which(updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Fourth_Primary_BC_Date"])
for (i in 1:length(equal3_4_idxes)){
  curr_idx <- equal3_4_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Fourth_Primary_BC_Date"] <- NA
  curr_3rd_site <- updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"]
  curr_4th_site <- updated_PrimaryBC_date_df[curr_idx,"Fourth_PrimarySite"]
  updated_PrimaryBC_date_df[curr_idx,"Third_PrimarySite"] <- paste0(curr_3rd_site,"$$$",curr_4th_site)
  updated_PrimaryBC_date_df[curr_idx,"Fourth_PrimarySite"] <-NA
}

#3 == 5
equal3_5_idxes <- which(updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Fifth_Primary_BC_Date"])
equal3_6_idxes <- which(updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Six_Primary_BC_Date"])

equal4_5_idxes <- which(updated_PrimaryBC_date_df[,"Fourth_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Fifth_Primary_BC_Date"])
equal4_6_idxes <- which(updated_PrimaryBC_date_df[,"Fourth_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Six_Primary_BC_Date"])
equal5_6_idxes <- which(updated_PrimaryBC_date_df[,"Fifth_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Six_Primary_BC_Date"])


#1st = recur cases,
equal1_recur_idxes <- which(updated_PrimaryBC_date_df[,"First_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])
equal2_recur_idxes <- which(updated_PrimaryBC_date_df[,"Second_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])
for (i in 1:length(equal2_recur_idxes)){
  curr_idx <- equal2_recur_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Second_Primary_BC_Date"] <- NA #only keep recur
}

equal3_recur_idxes <- which(updated_PrimaryBC_date_df[,"Third_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])
for (i in 1:length(equal3_recur_idxes)){
  curr_idx <- equal3_recur_idxes[i]
  updated_PrimaryBC_date_df[curr_idx,"Third_Primary_BC_Date"] <- NA #only keep recur
}

equal4_recur_idxes <- which(updated_PrimaryBC_date_df[,"Fourth_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])
equal5_recur_idxes <- which(updated_PrimaryBC_date_df[,"Fifth_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])
equal6_recur_idxes <- which(updated_PrimaryBC_date_df[,"Six_Primary_BC_Date"]  == updated_PrimaryBC_date_df[,"Date_1Recur"])


write.csv(updated_PrimaryBC_date_df,paste0(out_dir,"updated_PrimaryBC_date.csv"),row.names = F)


############################################################################################
#5. Get All other primary cancer dates
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
      othercancer_df_list[[p]] <- curr_othercancer_df
  }
}
all_othercancer_df <- do.call(rbind,othercancer_df_list)

#Remove the duplicates for each ID
all_othercancer_df <- all_othercancer_df[!duplicated(all_othercancer_df),] #remove duplicated
write.csv(all_othercancer_df,paste0(out_dir,"updated_Other_Cancer_date.csv"),row.names = F)

############################################################################################
#Get 1st,2nd, 3rd event type and date 
#1st event:  1st primary dates
#2nd event:  recurrence of the 1st or second primary
#3rd event:  second primary or recurrence or non-brast canncer or 3rd/4th primary?
############################################################################################
All_event_df <- as.data.frame(matrix(NA, nrow = length(updated_analysis_ID), ncol = 7))
colnames(All_event_df) <- c("ID","Date_1st_Event","Date_2nd_Event","Date_3rd_Event","Site_1st_Event","Site_2nd_Event","Site_3rd_Event")
for (p in 1:length(updated_analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- updated_analysis_ID[p]
  All_event_df[p,"ID"] <- curr_id
  #Curr updated_PrimaryBC_date_df
  curr_idx <- which(updated_PrimaryBC_date_df[,"ID"] == curr_id)
  curr_updated_PrimaryBC_date_df <- updated_PrimaryBC_date_df[curr_idx,]
  
  #Get 1st,2nd,3rd, and 4th primary date and site
  all_primary_dates_cols <- c("First_Primary_BC_Date","Second_Primary_BC_Date","Third_Primary_BC_Date","Fourth_Primary_BC_Date","Fifth_Primary_BC_Date","Six_Primary_BC_Date")
  all_primary_sites_cols <- c("First_PrimarySite","Second_PrimarySite","Third_PrimarySite","Fourth_PrimarySite","Fifth_PrimarySite","Sixth_PrimarySite")
  curr_all_primary_df <- get_primary_dates_site_func(curr_updated_PrimaryBC_date_df,all_primary_dates_cols,all_primary_sites_cols)
    
  #recurrence date
  curr_recurrence_date <- curr_updated_PrimaryBC_date_df[,"Date_1Recur"]
  curr_recurrence_df <- cbind.data.frame("1Recur",curr_recurrence_date) #create a df so we can merge this with other dates df
  colnames(curr_recurrence_df) <- c("Site","Date")
  rownames(curr_recurrence_df) <- "1Recur"
  curr_recurrence_df[] <- lapply(curr_recurrence_df, as.character) #unfacter the columns
  
  
  #other cancer
  curr_other_df <- get_othercaner_dates_site_func(all_othercancer_df,curr_id)
  curr_other_df <- curr_other_df[,-which(colnames(curr_other_df)=="ID")] #  #remove the ID col
  
  #Combine all dates
  curr_all_dates_df <- rbind(curr_all_primary_df,curr_other_df,curr_recurrence_df)


  #Get individual components
  curr_recurrence_date <- curr_all_dates_df[which(rownames(curr_all_dates_df) == "1Recur"),"Date"]
  curr_1stP_date <- curr_all_dates_df[which(rownames(curr_all_dates_df) == "Primary1"),"Date"]
  curr_1stP_site <- curr_all_dates_df[which(rownames(curr_all_dates_df) == "Primary1"),"Site"]
  
  curr_2ndP_date <- curr_all_dates_df[which(rownames(curr_all_dates_df) == "Primary2"),"Date"]
  curr_2ndP_site <- curr_all_dates_df[which(rownames(curr_all_dates_df) == "Primary2"),"Site"]
  
  
  #'@Fristevent must be 1st primary bc dates
  first_event_date <- curr_1stP_date
  first_event_site <- curr_1stP_site
  

  #'@2ndevent  could be recur or secondry primary
  if (is.na(curr_recurrence_date) == T & is.na(curr_2ndP_date) == T ){ #if No 2nd event:
     second_event_date <- NA
     second_event_site <- NA
  }else if (is.na(curr_recurrence_date) == F & is.na(curr_2ndP_date) == T){#if 2nd event is recurence
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"

  }else if (is.na(curr_recurrence_date) == T & is.na(curr_2ndP_date) == F){ #if 2nd nevent is 2nd primary
    second_event_date <- curr_2ndP_date
    second_event_site <- curr_2ndP_site
    
  }else if (mdy(curr_recurrence_date) < mdy(curr_2ndP_date)){
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"
  
  }else if (mdy(curr_recurrence_date) > mdy(curr_2ndP_date)){
    second_event_date <- curr_2ndP_date
    second_event_site <- curr_2ndP_site
  }else if (mdy(curr_recurrence_date) == mdy(curr_2ndP_date)){
    second_event_date <- curr_recurrence_date
    second_event_site <- "1Recur"
  }
  
  #'@ThridEvent  #3rd event could be any subsequent diagnoses after 2nd event (recurrences or primary breast or non-breast cancers)
  if (is.na(curr_recurrence_date) == T & is.na(curr_2ndP_date) == T ){ #if No 2nd SCBE event, then no 3rd
    third_event_date <- NA
    third_event_site <- NA
  }else{
    #Order all_dates_df by dates
    ordered_curr_all_dates_df <- curr_all_dates_df[order(mdy(curr_all_dates_df[,"Date"])),]
    
    #remove everything before the 2nd event (including the 2nd)
    endidxs_toremove <- which(ordered_curr_all_dates_df$Date == second_event_date)
    
    #If two same dates, always keep priamry or 1recur, remove others
    if (length(endidxs_toremove) >1 ){
      equal_df <- ordered_curr_all_dates_df[endidxs_toremove,]
      to_remove <- equal_df[which(rownames(equal_df) != "1Recur" 
                             & grepl("Primary",rownames(equal_df)) == F),]
      idx_toremove <- which(rownames(ordered_curr_all_dates_df) == rownames(to_remove))
      ordered_curr_all_dates_df <- ordered_curr_all_dates_df[-idx_toremove, ]
      
      #update end index to remove
      endidxs_toremove <- which(ordered_curr_all_dates_df$Date == second_event_date)
    }
    
    ordered_curr_all_dates_df <- ordered_curr_all_dates_df[-c(1:endidxs_toremove), ]
    
    #Then the first one is the 3rd event or NA
    third_event_date <- ordered_curr_all_dates_df[1,"Date"]
    third_event_site <- ordered_curr_all_dates_df[1,"Site"]
  }


  #event df
  event_dates <- c(first_event_date,second_event_date,third_event_date)
  event_sites <- c(first_event_site,second_event_site,third_event_site)
  
  All_event_df[p,"Date_1st_Event"] <- first_event_date
  All_event_df[p,"Date_2nd_Event"] <- second_event_date
  All_event_df[p,"Date_3rd_Event"] <- third_event_date
  All_event_df[p,"Site_1st_Event"] <- first_event_site
  All_event_df[p,"Site_2nd_Event"] <- second_event_site
  All_event_df[p,"Site_3rd_Event"] <- third_event_site

  
}
write.csv(All_event_df,paste0(out_dir,"All_event_df.csv"),row.names = F)

############################################################################################
#####Just checking
############################################################################################
#'@Question1 Next time , Check which 1st event and 2nd event happend different time, but same site
equal12_idxes <- which(All_event_df$Site_1st_Event == All_event_df$Site_2nd_Event )
check12 <- All_event_df[equal12_idxes,]

#Check 1 = 3, 
equal13_idxes <- which(All_event_df$Site_1st_Event == All_event_df$Site_3rd_Event )
check13 <- All_event_df[equal13_idxes,]

#Check 2= 3, none
equal23_idxes <- which(All_event_df$Site_2nd_Event == All_event_df$Site_3rd_Event )
check23 <- All_event_df[equal23_idxes,]

#Check 1=2=3
cond1 <- All_event_df$Site_1st_Event == All_event_df$Site_2nd_Event
cond2 <- All_event_df$Site_2nd_Event == All_event_df$Site_3rd_Event
equal123_idxes <- which(cond1 & cond2 == T)
check123 <- All_event_df[equal123_idxes,]

############################################################################################
##Add Outcome flag
############################################################################################
All_event_df <- read.csv(paste0(out_dir,"All_event_df.csv"),stringsAsFactors = F)
analysis_IDs <- unique(All_event_df$ID)

#Add BC related Death flag
#Add Cause of death site
All_event_df$BC_related_Death <- 0
All_event_df$BC_related_Death_Site <- NA
All_event_df$First_Primary_BC_related_Death<-0

#'@Question2: #What is death code 0000 or 7777 mean
for (i in 1:length(analysis_IDs)){
  curr_id <- analysis_IDs[i]
  
  curr_first_primary_site <- All_event_df[which(All_event_df[,"ID"] == curr_id),"Site_1st_Event"]
  curr_first_primary_site <- gsub("Primary_","",curr_first_primary_site)
  
  curr_idx <- which(uh3_kcr_df[,"study_id"] == curr_id)
  curr_death_site <- unique(uh3_kcr_df[curr_idx,"CauseOfDeath"])
  if (length(curr_death_site) > 1){
    All_event_df[i,"BC_related_Death"] <- "More than 1 death codes"
    All_event_df[i,"BC_related_Death_Site"] <- paste0(curr_death_site,collapse = "$$$")
    All_event_df[i,"First_Primary_BC_related_Death"] <- "More than 1 death codes"
  }else{
  if (curr_death_site %in% bc_codes){ #BC related death
    All_event_df[i,"BC_related_Death"] <- 1
    All_event_df[i,"BC_related_Death_Site"] <- curr_death_site
    
    #Check if it is first priamry related death
    if(curr_death_site == curr_first_primary_site){
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
  
}

table(All_event_df$BC_related_Death) #    0 :34561, 1: 6813 , morethan 1 codes1
table(All_event_df$First_Primary_BC_related_Death) #0:39631 1:1743 

#Add a flag to see if the 3rd event is other cancer or breast cancer event (recur or primary)
All_event_df$Event3_Flag <- NA
no3rd_idxes <- which(is.na(All_event_df[,"Site_3rd_Event"])==T)
All_event_df$Event3_Flag[no3rd_idxes] <- "No 3rd"
priBC_3rd_idxes <- which(grepl("Primary",All_event_df[,"Site_3rd_Event"])==T)
All_event_df[priBC_3rd_idxes,"Event3_Flag"] <- "Primary BC"
recurBC_3rd_idxes <- which(All_event_df[,"Site_3rd_Event"]== "1Recur")
All_event_df[recurBC_3rd_idxes,"Event3_Flag"] <- "1Recur"
All_event_df[-c(no3rd_idxes,priBC_3rd_idxes,recurBC_3rd_idxes),"Event3_Flag"] <- "Other Cancer"

####################################################################################################
#report number of patients who has 2nd event
####################################################################################################
All_event_df$SBCE <- 0
All_event_df[which(is.na(All_event_df[,"Date_2nd_Event"])==F),"SBCE"] <- 1
table(All_event_df$SBCE) #    0:36955  1: 4420

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

write.csv(All_event_df,paste0(out_dir,"updated_All_event_df.csv"),row.names = F)


