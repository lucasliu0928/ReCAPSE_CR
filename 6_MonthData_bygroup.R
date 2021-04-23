
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


perday_data <- read.csv(paste0(data_dir,"filtered_inValidMonth_comb_perday_df.csv"),stringsAsFactors = F)
analysis_Ids <- unique(perday_data$study_id)
length(analysis_Ids) #this is the finaly anlyaisi Id who has valid claim month less than "finaly_anlaysi Id file, cuz valid month filtered

################################################################################ 
### Load outcome 
################################################################################ 
outcome_df <- read.csv(paste0(outdir,"updated_All_event_df.csv"),stringsAsFactors = F)


################################################################################ 
### Load Valid month
################################################################################ 
Valid_Month_df <- read.csv(paste0(outdir,"All_Final_Valid_month.csv"),stringsAsFactors = F)


#load group file of codes
diag_df <- read.csv(paste0(data_dir,"Grouped_Diag_codes.csv"),stringsAsFactors = F)
Proc_df <- read.csv(paste0(data_dir,"Grouped_Proc_codes.csv"),stringsAsFactors = F)
Drug_df <- read.csv(paste0(data_dir,"Grouped_Drug_codes.csv"),stringsAsFactors = F)

code_type <- "Diag_Codes"
analysis_df <- diag_df
group_name <- "Chubak_type"
unique_groups <- unique(analysis_df[,group_name])
unique_groups <- unique(unlist(strsplit(unique_groups,split = "$$$$",fixed = T))) #multiple grp situtation

library(lubridate)
month_df_list <- list()
for (i in  1: length(analysis_Ids)){
  if (i %% 100 == 0){
    print(i)
  }
  curr_id <- analysis_Ids[i]
  curr_perday_df <- perday_data[which(perday_data$study_id ==curr_id ),]
  curr_valid_month <- sort(Valid_Month_df[which(Valid_Month_df$ID ==curr_id),"Valid_Month"])
  curr_outcome_df <-  outcome_df[which(outcome_df$ID == curr_id ),]
  curr_SBCE_flag <- curr_outcome_df$SBCE
  curr_SBCE_time <- mdy(curr_outcome_df$Date_2nd_Event)
    
  ####
  curr_df <- as.data.frame(matrix(NA, nrow = length(curr_valid_month) ,ncol = length(unique_groups) + 3))
  colnames(curr_df) <- c("ID","Month_Start","outcome",unique_groups)
  curr_df$ID <- curr_id
  curr_df$Month_Start <- curr_valid_month
  
    for (j in 1:length(curr_valid_month)){
      curr_mon_start <- ymd(curr_valid_month[j]) 
      curr_mon_end <- curr_mon_start + months(1)
      curr_month_df<- curr_perday_df[which(ymd(curr_perday_df$claims_date) >= curr_mon_start & 
                                  ymd(curr_perday_df$claims_date) < curr_mon_end),]
      curr_month_codes <- unlist(strsplit(curr_month_df[,code_type],"$$$$",fixed = T))
     
      #find each codes group
      matched_groups <- analysis_df[which(analysis_df$Code %in% curr_month_codes),group_name]
      matched_groups <- unlist(strsplit(matched_groups,split = "$$$$",fixed = T))
      na_indxes <- which(is.na(matched_groups)==T)
      if (length(na_indxes) > 0){
          matched_groups <- matched_groups[-na_indxes]
      }
      count_tb <- as.data.frame(table(matched_groups))
      df_indxes <- which(colnames(curr_df) %in% count_tb[,1])
      curr_df[j,df_indxes] <- count_tb[,"Freq"]
      #print(df_indxes)
      #add outcome 

      if (curr_SBCE_flag == 0){
        curr_df[j,"outcome"] <- 0  #"Pre"
      }else{
        #compare if curr month before or after SBCE
        if (curr_mon_start < curr_SBCE_time){
          curr_df[j,"outcome"] <- 0  #0
        }else{
          curr_df[j,"outcome"] <- 1 #post
          
        }
      }
    
    }

  month_df_list[[i]] <- curr_df
}

#combine all 
month_data <- do.call(rbind, month_df_list)

write.csv(month_data, paste0(outdir, "diag_monthly_df.csv"),row.names = F)
