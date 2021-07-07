library(lubridate)
library(openxlsx)

get_primary_site_date_func <- function(pt_kcr_df,CentralSequence_N) {
  pri_idx <- which(pt_kcr_df[,"CentralSequenceNumber"] %in% CentralSequence_N) #primary index
  if (length(pri_idx) > 0 ){  #if has primary idxes, it coulde be more than one
    pri_dates <- paste0(pt_kcr_df[pri_idx,"Date_dx"],collapse = "$$$")
    pri_sites <- paste0(pt_kcr_df[pri_idx,"PrimarySite"],collapse = "$$$")
    if (identical(CentralSequence_N,c(0,1)) == T){ #if it is 1st primary, get recurrence date if there is recurrece
      recur_date <- pt_kcr_df[pri_idx,"Date_1Recur"]
    }else{
      recur_date <- NA
    }
  }else{
    pri_dates <- NA
    pri_sites <- NA
    recur_date <- NA
  }
  
  return(list(pri_sites,pri_dates,recur_date))
}

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
###########   2. Analysis IDs                                   ###########  
###########################################################################
unique_IDs <- sort(unique(uh3_kcr_df[,"study_id"]))

##################################################################################################
# 3. Get All patient cancer Site and date and type (primary or other cancer or recur of primary)
##################################################################################################
cancer_site_list <- list(NA)
for (p in 1:length(unique_IDs)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- unique_IDs[p]

  #Curr kcr df
  curr_idx <- which(uh3_kcr_df[,"study_id"] == curr_id)
  curr_kcr_df <- uh3_kcr_df[curr_idx,]
  
  #1. get 1st,2nd,3rd and 4th primary site and dates
  sequence_n_list <- list(c(0,1),c(2),c(3),c(4),c(5),c(6))
  squence_n_name <- c("First","Second","Third","Fourth","Fifth","Sixth")
  primary_df <- as.data.frame(matrix(NA, nrow = length(sequence_n_list), ncol = 3))
  colnames(primary_df) <- c("Site","Date","Type")
  for (i in 1:length(sequence_n_list)){
    curr_sqeuence_n <- sequence_n_list[[i]]
    curr_sqeuqence_name <- squence_n_name[i]
    curr_res <- get_primary_site_date_func(curr_kcr_df,curr_sqeuence_n)
    primary_df[i,"Site"] <- curr_res[[1]]
    primary_df[i,"Date"] <- curr_res[[2]]
    primary_df[i,"Type"] <-  paste0(curr_sqeuqence_name,"_Primary")
  }
  
  #2.Get 1st primary BC recurance if there is one
  recur_1stBC_df <- as.data.frame(matrix(NA, nrow = 1, ncol = 3))
  colnames(recur_1stBC_df) <- c("Site","Date","Type")
  curr_res <- get_primary_site_date_func(curr_kcr_df,c(0,1))
  curr_1st_pri_site <- curr_res[[1]]
  curr_recur_dates <- curr_res[[3]]
  recur_1stBC_df[1,"Site"] <- curr_1st_pri_site
  recur_1stBC_df[1,"Date"] <- curr_recur_dates
  recur_1stBC_df[1,"Type"] <- "1Recur"

  
  #3.Get Other cancer info
  n_sites <- 6
  n_rows_curr_kcr <- nrow(curr_kcr_df)
  other_cancer_df <- as.data.frame(matrix(NA, nrow = n_sites *n_rows_curr_kcr, ncol = 3))
  colnames(other_cancer_df) <- c("Site","Date","Type")
  ct <- 1
  for (i in 1:n_sites){ #for each site col
    curr_site_col <- paste0("site_o",i)
    curr_date_col <- paste0("date_o",i)
    for (j in 1:n_rows_curr_kcr){
      other_cancer_df[ct,"Site"] <-  curr_kcr_df[j,curr_site_col]
      other_cancer_df[ct,"Date"] <-  curr_kcr_df[j,curr_date_col]
      other_cancer_df[ct,"Type"] <- "Other"
      ct <- ct + 1
    }
  }
  
  #4.Combine all site and date info
  comb_site_date_df <- rbind(primary_df,recur_1stBC_df,other_cancer_df)
  comb_site_date_df$study_id <- curr_id
  
  #5. Remove NA site or NA date
  comb_site_date_df <- comb_site_date_df[-which(is.na(comb_site_date_df[,"Site"])==T | is.na(comb_site_date_df[,"Date"])==T),]
  #6.Remove duplicated columns (cuz other cancer might have duplicates in different rows)
  comb_site_date_df <- comb_site_date_df[!duplicated(comb_site_date_df),]
  
  cancer_site_list[[p]] <- comb_site_date_df
}

cancer_site_df <- do.call(rbind,cancer_site_list)

############################################################################################
#4. Exclude pt has no 1st primay cancer info
# Updated IDs and primary bc df
############################################################################################
exclude_IDs <- NA
ct <- 1
for (p in 1:length(unique_IDs)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- unique_IDs[p]
  curr_all_cancer_site <- cancer_site_df[which(cancer_site_df[,"study_id"] == curr_id),]
  
  curr_1st_pri_indxes <- which(curr_all_cancer_site$Type == "First_Primary")
  if (length(curr_1st_pri_indxes) == 0){
    exclude_IDs[ct] <- curr_id
    ct <- ct + 1
  }
}

length(exclude_IDs) #5755 has no 1st primary cancer info

updated_cancer_site_df <- cancer_site_df[-which(cancer_site_df[,"study_id"] %in% exclude_IDs),] #41375

############################################################################################
#5.If there are pts who primary cancer is not breast cancer, then exclude
############################################################################################
#first check (none)
pri_1st_df <- updated_cancer_site_df[which(updated_cancer_site_df$Type == "First_Primary"),]
table(pri_1st_df$Site)

#double check (none)
bc_codes_search <- paste0(paste0("C50",seq(0,9,1)),collapse = "|")
indxed_to_include <- which(updated_cancer_site_df$Type == "First_Primary" & 
                             grepl(bc_codes_search,updated_cancer_site_df$Site)==F)

############################################################################################
#5.Update Anlaysis ID
############################################################################################
updated_analysis_ID <- unique(updated_cancer_site_df$study_id) # 41375

############################################################################################
#'@NOTE 6. Merge two date and site, if the time is the same
############################################################################################
merged_cancer_site_list <- list(NA)
for (p in 1:length(updated_analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  
  curr_id <- 7195
  curr_id <- updated_analysis_ID[p]
  curr_all_cancer_site <- updated_cancer_site_df[which(updated_cancer_site_df[,"study_id"] == curr_id),]
  
  #1.get unique dates
  unique_dates <- unique(curr_all_cancer_site[,"Date"])
  sametime_idxes <- list()
  for (i in 1:length(unique_dates)){
    curr_date <- unique_dates[i]
    sametime_idxes[[i]] <- which(curr_all_cancer_site[,"Date"] == curr_date)
  }
  
  #2.get merge site when date are the same
  merged_df <- as.data.frame(matrix(NA, nrow = length(unique_dates), ncol = 4))
  colnames(merged_df) <- c("Site","Date","Type","study_id")
  merged_df$study_id <- curr_id
  for (i in 1:length(sametime_idxes)){
    curr_sametime_indxes <- sametime_idxes[[i]]
    curr_merged_sites <- paste0(curr_all_cancer_site[curr_sametime_indxes,"Site"],collapse = "$$$")
    curr_merged_dates <- unique(curr_all_cancer_site[curr_sametime_indxes,"Date"])
    curr_merged_types <- paste0(curr_all_cancer_site[curr_sametime_indxes,"Type"],collapse = "$$$")
    merged_df[i,"Site"] <- curr_merged_sites
    merged_df[i,"Date"] <- curr_merged_dates
    merged_df[i,"Type"] <- curr_merged_types
  }

  
  #3.remove previous rows
  curr_all_cancer_site_updated <- curr_all_cancer_site[-unlist(sametime_idxes),]
  
  #4.Add merged rows
  curr_all_cancer_site_updated <- rbind(curr_all_cancer_site_updated,merged_df)
  
  merged_cancer_site_list[[p]] <- curr_all_cancer_site_updated
}

merged_cancer_site_df <- do.call(rbind,merged_cancer_site_list)

#This table shows how many are merged
table(merged_cancer_site_df$Type)

write.xlsx(merged_cancer_site_df,paste0(outdir,"4_All_cancer_site_date_df.xlsx"))

############################################################################################
#Get 1st,2nd, 3rd event type and date 
#1st event:  1st primary dates
#2nd event:  recurrence of the 1st or second primary
#3rd event:  subsqeunt after 2nd event
############################################################################################
All_event_df <- as.data.frame(matrix(NA, nrow = length(updated_analysis_ID), ncol = 10))
colnames(All_event_df) <- c("study_id","Date_1st_Event","Date_2nd_Event","Date_3rd_Event",
                                 "Site_1st_Event","Site_2nd_Event","Site_3rd_Event",
                                 "Type_1st_Event","Type_2nd_Event","Type_3rd_Event")
for (p in 1:length(updated_analysis_ID)){
  if (p %% 1000 == 0){
    print(p)
  }
  curr_id <- updated_analysis_ID[p]
  All_event_df[p,"study_id"] <- curr_id
  
  #1. get curernt cancer info
  curr_idx <- which(merged_cancer_site_df[,"study_id"] == curr_id)
  curr_cancer_df <- merged_cancer_site_df[curr_idx,]
  curr_cancer_df_ordered <- curr_cancer_df[order(mdy(curr_cancer_df$Date), decreasing = F),] #reorder
  

  #'@1stevent must be 1st primary bc dates
  first_idx <- which(grepl("First_Primary",curr_cancer_df_ordered[,"Type"]) == T) #due to merged process, as long as it contains First primary, we consider it, just ignore the other ones
  All_event_df[p,"Site_1st_Event"] <- curr_cancer_df_ordered[first_idx,"Site"]
  All_event_df[p,"Date_1st_Event"] <- curr_cancer_df_ordered[first_idx,"Date"]
  All_event_df[p,"Type_1st_Event"] <- curr_cancer_df_ordered[first_idx,"Type"]
  
  #'@2ndevent  could be recur or secondry primary or merged both
  curr_cancer_df_ordered <- curr_cancer_df_ordered[-first_idx,] #remove the first index, due to the case "First and Second" being merged, aslongas it is first, it was the 1st event
  second_idx <- which(grepl("Second_Primary|1Recur",curr_cancer_df_ordered[,"Type"]) == T)[1]  # index [1]choose the first one as the 2nd event, then the next one is the 3rd event. (e.g,2nd date <  1Recur)
  

  
  if (length(second_idx) == 0){ #if no 2nd event, then no 3rd 
      All_event_df[p,"Site_2nd_Event"] <- NA
      All_event_df[p,"Date_2nd_Event"] <- NA
      All_event_df[p,"Type_2nd_Event"] <- NA
      All_event_df[p,"Site_3rd_Event"] <- NA
      All_event_df[p,"Date_3rd_Event"] <- NA
      All_event_df[p,"Type_3rd_Event"] <- NA
  }else {
      All_event_df[p,"Site_2nd_Event"] <- curr_cancer_df_ordered[second_idx,"Site"]
      All_event_df[p,"Date_2nd_Event"] <- curr_cancer_df_ordered[second_idx,"Date"]
      All_event_df[p,"Type_2nd_Event"] <- curr_cancer_df_ordered[second_idx,"Type"]
    
      #'@ThridEvent  #3rd event could be any subsequent diagnoses after 2nd event 
      #'#It could be a recurrences or primary breast or non-breast cancers)
      second_event_date <-  All_event_df[p,"Date_2nd_Event"]
      subsequent_idx <- which(mdy(curr_cancer_df_ordered$Date) > mdy(second_event_date))[1] #since it is ordered, so the first one is the subsquent one
      
      if (length(subsequent_idx) > 0){ #if there are subsqeuent one
        All_event_df[p,"Site_3rd_Event"] <- curr_cancer_df_ordered[subsequent_idx,"Site"]
        All_event_df[p,"Date_3rd_Event"] <- curr_cancer_df_ordered[subsequent_idx,"Date"]
        All_event_df[p,"Type_3rd_Event"] <- curr_cancer_df_ordered[subsequent_idx,"Type"]
      }else{
        All_event_df[p,"Site_3rd_Event"] <- NA
        All_event_df[p,"Date_3rd_Event"] <- NA
        All_event_df[p,"Type_3rd_Event"] <- NA
      }
  }


}

write.xlsx(All_event_df,paste0(outdir,"4_All_event_df.xlsx"))


############################################################################################
#Just checking if any 1st event happens before 2nd event
############################################################################################
which(mdy(All_event_df$Date_1st_Event) >= mdy(All_event_df$Date_2nd_Event))
which(mdy(All_event_df$Date_1st_Event) >= mdy(All_event_df$Date_3rd_Event))
which(mdy(All_event_df$Date_2nd_Event) >= mdy(All_event_df$Date_3rd_Event))

