library(openxlsx)

count_ptsFrac_perCode_func <- function(per_pts_data_df,code_col_name,uniuqe_code_df){
  #per_pts_data_df <- per_pts_data
  #uniuqe_code_df <- unique_Diag_Grp_df
  #code_col_name <- "Unique_Diag_Codes_inAllValidMonth"
  
  uniuqe_code_df$N_PtsHasCode <- NA
  uniuqe_code_df$Fraction_PtsHasCode <- NA
  
  #check_codes <- uniuqe_code_df[1:500,"Code"]
  #check_codes <- paste0("\\b",check_codes,"\\b")
  #codes_binary_df <- sapply(check_codes, grepl, per_pts_data_df[,"Unique_Diag_Codes_inAllValidMonth"])
  #colSums(check)
  
  for (i in 1:nrow(uniuqe_code_df)){
    if (i %% 500 == 0){print(i)}
    curr_code <- uniuqe_code_df[i,"Code"]
    uniuqe_code_df[i,"N_PtsHasCode"] <- length(which(grepl(paste0("\\b",curr_code,"\\b"),per_pts_data_df[,code_col_name]) == T))
    uniuqe_code_df[i,"Fraction_PtsHasCode"]  <- uniuqe_code_df[i,"N_PtsHasCode"]/nrow(per_pts_data_df)
  }
  
  return(uniuqe_code_df)
}


proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")


#########################################################################################################
####1. Load final anlaysis ID
#########################################################################################################
final_anlaysisID_df <- read.csv(paste0(data_dir,"Final_analysis_ID.csv"), stringsAsFactors = F)
final_analysis_ID <- unique(final_anlaysisID_df$ID) #26735

################################################################################ 
###2. Load outcome 
################################################################################ 
outcome_df <- read.csv(paste0(outdir,"updated_All_event_df.csv"),stringsAsFactors = F)
updated_outcome_df <- outcome_df[which(outcome_df[,"ID"] %in% final_analysis_ID),]

################################################################################ 
####3. Load codes df
################################################################################ 
unique_Diag_Grp_df <- read.csv(paste0(data_dir,"Grouped_Diag_codes.csv"),stringsAsFactors = F)
unique_Proc_Grp_df <- read.csv(paste0(data_dir,"Grouped_Proc_codes.csv"),stringsAsFactors = F)
unique_Drug_Grp_df <- read.csv(paste0(data_dir,"Grouped_Drug_codes.csv"),stringsAsFactors = F)


################################################################################ 
####4. Load per pts data
################################################################################ 
per_pts_data <- read.csv(paste0(outdir,"per_pts_data.csv"),stringsAsFactors = T)
for (j in 2:4){ #convert to character, not factor
  per_pts_data[,j] <- as.character(per_pts_data[,j])
}

################################################################################ 
# add outcome to per_pts_data
#'################################################################################ 
per_pts_data$SBCE <- NA
per_pts_data$SBCE <- updated_outcome_df$SBCE[match(per_pts_data$ID,updated_outcome_df$ID)]


################################################################################ 
# get seperated data for each group
#'################################################################################ 
noSBCE_indxes <- which(per_pts_data$SBCE==0)
noSBCE_per_pts_data <- per_pts_data[noSBCE_indxes,]
yesSBCE_per_pts_data <- per_pts_data[-noSBCE_indxes,]
  
################################################################################ 
### Count For SBCE group
################################################################################ 
#1.diag code #2hours for this
diag_codes_freq_SBCE1 <- count_ptsFrac_perCode_func(yesSBCE_per_pts_data,
                                             "Unique_Diag_Codes_inAllValidMonth",
                                              unique_Diag_Grp_df)
#2.proc code
proc_codes_freq_SBCE1 <-count_ptsFrac_perCode_func(yesSBCE_per_pts_data,
                                                   "Unique_Proc_Codes_inAllValidMonth",
                                                   unique_Proc_Grp_df)
#3. drug code
drug_codes_freq_SBCE1 <-count_ptsFrac_perCode_func(yesSBCE_per_pts_data,
                                                   "Unique_Drug_Codes_inAllValidMonth",
                                                   unique_Drug_Grp_df)

write.csv(diag_codes_freq_SBCE1,paste0(outdir,"Code_FreqTable/diag_codes_freq_SBCE1.csv"),row.names = F)
write.csv(proc_codes_freq_SBCE1,paste0(outdir,"Code_FreqTable/proc_codes_freq_SBCE1.csv"),row.names = F)
write.csv(drug_codes_freq_SBCE1,paste0(outdir,"Code_FreqTable/drug_codes_freq_SBCE1.csv"),row.names = F)


################################################################################ 
### Count For no SBCE group
################################################################################ 
#1.diag code
start_time <- Sys.time()
diag_codes_freq_SBCE0 <- count_ptsFrac_perCode_func(noSBCE_per_pts_data,
                                                    "Unique_Diag_Codes_inAllValidMonth",
                                                    unique_Diag_Grp_df)
end_time <- Sys.time()

#2.proc code
proc_codes_freq_SBCE0 <-count_ptsFrac_perCode_func(noSBCE_per_pts_data,
                                                   "Unique_Proc_Codes_inAllValidMonth",
                                                   unique_Proc_Grp_df)
#3. drug code
drug_codes_freq_SBCE0 <-count_ptsFrac_perCode_func(noSBCE_per_pts_data,
                                                   "Unique_Drug_Codes_inAllValidMonth",
                                                   unique_Drug_Grp_df)

write.csv(diag_codes_freq_SBCE0,paste0(outdir,"Code_FreqTable/diag_codes_freq_SBCE0.csv"),row.names = F)
write.csv(proc_codes_freq_SBCE0,paste0(outdir,"Code_FreqTable/proc_codes_freq_SBCE0.csv"),row.names = F)
write.csv(drug_codes_freq_SBCE0,paste0(outdir,"Code_FreqTable/drug_codes_freq_SBCE0.csv"),row.names = F)


################################################################################## 
#'@ToDo. remove code no pts has it, cuz filter valid month data
#'################################################################################ 
#write.csv(diag_codes_freq,paste0(outdir,"Code_FreqTable/diag_codes_freq.csv"),row.names = F)
#write.csv(proc_codes_freq,paste0(outdir,"Code_FreqTable/proc_codes_freq.csv"),row.names = F)
#write.csv(drug_codes_freq,paste0(outdir,"Code_FreqTable/drug_codes_freq.csv"),row.names = F)
