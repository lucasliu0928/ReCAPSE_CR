proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


################################################################################ 
#### Load combined per day data from medicaid and medicare
################################################################################ 
Comb_perday_df <- read.csv(paste0(data_dir, "Comb_PerDay_data.csv"),stringsAsFactors = F)
Comb_perday_df <- Comb_perday_df[,-1]
length(unique(Comb_perday_df[,"study_id"])) #35697

################################################################################ 
### Load Valid month
################################################################################ 
Valid_Month_df <- read.csv(paste0(outdir,"All_Final_Valid_month.csv"),stringsAsFactors = F)
length(unique(Valid_Month_df[,"ID"])) #28675


################################################################################ 
#Count number of valid month per pateints 
################################################################################ 
analysis_ID_df <- read.csv(paste0(outdir,"Final_analysis_ID.csv"),stringsAsFactors = F) # 27722
length(analysis_ID_df$ID)
analysis_ID <- analysis_ID_df$ID

n_valid_month <- NA
valid_perday_df_list <- list()
for (i in 1:length(analysis_ID)){
  if (i %% 1000 ==0){
    print(i)
  }
  curr_id <- analysis_ID[i]
  curr_enroll_months <- unique(Valid_Month_df[which(Valid_Month_df[,"ID"] == curr_id),"Valid_Month"])
  n_valid_month[i] <- length(curr_enroll_months)
  
  min_month <- min(ymd(curr_enroll_months))
  max_month <- max(ymd(curr_enroll_months))
  
  #Curr_per_day  data
  curr_perday_df <- Comb_perday_df[which(Comb_perday_df[,"study_id"] == curr_id),]
  
  index_tokeep <- which(ymd(curr_perday_df[,"claims_date"]) >= min_month & ymd(curr_perday_df[,"claims_date"]) <= max_month )
  if (length(index_tokeep) > 0 ){
    valid_perday_df_list[[i]] <- curr_perday_df[index_tokeep,]
  }else{
    valid_perday_df_list[[i]] <- NULL
  }
  
}

#output filtered valid per day data.
filtered_Comb_perday_df <- do.call(rbind, valid_perday_df_list)
write.csv(filtered_Comb_perday_df,paste0(outdir,"filtered_inValidMonth_comb_perday_df.csv"))
sum(n_valid_month)
