proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/" 
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_file1 <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/Medicaid_intermediate_Data/Medicaid_PateintPerDay_Data/All_PerDay_Data_Medicaid.csv")
data_file2 <- paste0(proj_dir,"ReCAPSE_Intermediate_Data/0318_21/Medicare_Intermediate_Data/Medicare_PateintPerDay_Data/All_PerDay_Data_Medicare.csv")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")

################################################################################ 
############################## Read  per day data ############################## 
################################################################################ 
Medicaid_perdaydf <- read.csv(data_file1,stringsAsFactors = F)
Medicare_perdday_df <- read.csv(data_file2,stringsAsFactors = F)
Comb_perday_df <- rbind(Medicaid_perdaydf,Medicare_perdday_df)
length(unique(Medicare_perdday_df[,"study_id"])) #31295
length(unique(Medicaid_perdaydf[,"study_id"])) #13021
length(unique(Comb_perday_df[,"study_id"])) #35697

#Reformat date to be consistent
idexes_toupdate <- which(grepl("/",Comb_perday_df[,"claims_date"])==T)
Comb_perday_df[idexes_toupdate,"claims_date"] <- as.character(mdy(Comb_perday_df[idexes_toupdate,"claims_date"]))

idexes_toupdate2 <- which(grepl("-",Comb_perday_df[,"claims_date"])==T)
Comb_perday_df[idexes_toupdate2,"claims_date"] <- as.character(ymd(Comb_perday_df[idexes_toupdate2,"claims_date"]))

nrow(Comb_perday_df) #13258796
write.csv(Comb_perday_df,paste0(outdir,"Comb_PerDay_data.csv"))
