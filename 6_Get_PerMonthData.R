proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


################################################################################ 
#### Load combined per day data from medicaid and medicare
################################################################################ 
Comb_perday_df <- read.csv(paste0(data_dir, "filtered_inValidMonth_comb_perday_df.csv"),stringsAsFactors = F)
Comb_perday_df <- Comb_perday_df[,-1]
length(unique(Comb_perday_df[,"study_id"])) #35697

################################################################################ 
### Load Valid month
################################################################################ 
Valid_Month_df <- read.csv(paste0(outdir,"All_Final_Valid_month.csv"),stringsAsFactors = F)
length(unique(Valid_Month_df[,"ID"])) #28675

################################################################################ 
#'TODO Need space to hold this
#Per month codes count
n_valid_month <- NA
for (i in 1:length(analysis_ID)){
  curr_id <- analysis_ID[i]
  curr_enroll_months <- unique(Valid_Month_df[which(Valid_Month_df[,"ID"] == curr_id),"Valid_Month"])
  n_valid_month[i] <- length(curr_enroll_months)
  
  #Curr_per_day  data
  curr_perday_df <- Comb_perday_df[which(Comb_perday_df[,"study_id"] == curr_id),]
  
  #per month table for diag, pro and drug
  res <- create_perMon_count_table(curr_id,n_valid_month,curr_enroll_months,curr_perday_df)
  curr_pt_month_table_diag <- res[[1]]
  curr_pt_month_table_proc <- res[[2]]
  curr_pt_month_table_drug <- res[[3]]
  
  outdir1 <- paste0(outdir,"PerMonth_CountTable/Diag/")
  write.csv(curr_pt_month_table_diag,paste0(outdir1,curr_id,".csv"))
  outdir2 <- paste0(outdir,"PerMonth_CountTable/Proc/")
  write.csv(curr_pt_month_table_proc,paste0(outdir2,curr_id,".csv"))
  outdir3 <- paste0(outdir,"PerMonth_CountTable/Drug/")
  write.csv(curr_pt_month_table_drug,paste0(outdir3,curr_id,".csv"))
  
}

################################################################################ 
#'TODO: Add patient outcome, pre-post recurrence, first primary dates
################################################################################ 
