library(lubridate)

data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
intermediate_data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/"

out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0225_21/"

#date df 
date_df <- read.csv(paste0(intermediate_data_dir,"0225_21/primary_date_df.csv"),stringsAsFactors = F)
date_ID <- date_df$ID
length(date_ID) #1359

#Medicaid
medicaid_df <- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
medicaid_ID <- unique(medicaid_df$study_id) 
length(medicaid_ID) #12959
updated_medicaid_df <- medicaid_df[which(medicaid_df$study_id %in% date_ID),]
length(unique(updated_medicaid_df$study_id))
write.csv(updated_medicaid_df,paste0(out_dir,"Filtered_Medicaid_df.csv"))


#Medicare
#Read seperated files
medicare_dir <-"/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/Medicare_Claims_seperated/"
file_names <- list.files(medicare_dir)

original_IDs_list <- list()
udpated_IDs_list <- list()
for (f in 1:length(file_names)){
  print(f)
  medicare_df <- read.csv(paste0(medicare_dir,file_names[f]),stringsAsFactors = F)
  original_IDs_list[[f]] <- medicare_df[,"study_id"]
  #get ID idxes in medicare  and also in date df
  idxes <- which(medicare_df[,"study_id"] %in% date_ID)
  if(length(idxes) > 0 ){ #If ID exsit in date df, then keep it
    updated_medicare_df <- medicare_df[idxes,]
    udpated_IDs_list[[f]] <- updated_medicare_df[,"study_id"]
    write.csv(updated_medicare_df,paste0(out_dir,"Filtered_Medicare_df/","Filtered_Medicare_df",f,".csv"),row.names = F)
    
  }
  
}

#count number of original ID
length(unique(unlist(original_IDs_list)))

#count number of udpated ID
length(unique(unlist(udpated_IDs_list)))

