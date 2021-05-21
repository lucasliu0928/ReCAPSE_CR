proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


################################################################################ 
#### Load combined per day data
################################################################################ 
Comb_perday_df <- read.csv(paste0(data_dir, "filtered_inValidMonth_comb_perday_df.csv"),stringsAsFactors = F)
Comb_perday_df <- Comb_perday_df[,-1]
length(unique(Comb_perday_df[,"study_id"])) # 26735

################################################################################ 
### Load Event type
################################################################################ 
updated_All_event_df<- read.csv(paste0(outdir,"updated_All_event_df.csv"),stringsAsFactors = F)
length(unique(updated_All_event_df[,"ID"])) # 40329

################################################################################ 
### Load Valid month
################################################################################ 
Valid_Month_df <- read.csv(paste0(outdir,"All_Final_Valid_month.csv"),stringsAsFactors = F)
length(unique(Valid_Month_df[,"ID"])) #27774

################################################################################ 
#Final anlaysis ID
################################################################################ 
analysis_ID <- unique(intersect(Comb_perday_df[,"study_id"],updated_All_event_df[,"ID"])) # 26735
analysis_ID_udpated <- unique(intersect(analysis_ID,Valid_Month_df[,"ID"])) # 26735
analysis_ID_udpated<- as.data.frame(analysis_ID_udpated)
colnames(analysis_ID_udpated) <- "ID"
write.csv(analysis_ID_udpated,paste0(outdir,"Final_analysis_ID.csv"))
