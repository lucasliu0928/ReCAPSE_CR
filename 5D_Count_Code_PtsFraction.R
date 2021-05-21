library(openxlsx)

count_ptsFrac_perCode_func <- function(per_pts_data_df,code_col_name,uniuqe_code_df){
  #per_pts_data_df <- per_pts_data
  #uniuqe_code_df <- unique_Drug_Grp_df
  #code_col_name <- "Unique_Drug_Codes_inAllValidMonth"
  
  uniuqe_code_df$N_PtsHasCode <- NA
  uniuqe_code_df$Fraction_PtsHasCode <- NA
  for (i in 1:nrow(uniuqe_code_df)){
    if (i %% 500 == 0){print(i)}
    curr_code <- uniuqe_code_df[i,"Code"]
    uniuqe_code_df[i,"N_PtsHasCode"] <- length(which(grepl(paste0("\\b",curr_code,"\\b"),per_pts_data[,code_col_name]) == T))
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
### Count
################################################################################ 

start_time <- Sys.time()
diag_codes_freq <-count_ptsFrac_perCode_func(per_pts_data,
                                             "Unique_Diag_Codes_inAllValidMonth",
                                              unique_Diag_Grp_df)
end_time <- Sys.time()





################################################################################ 
#'@ToDo1. add outcome to per_pts_data
#'################################################################################ 
per_pts_data$ID
table(updated_outcome_df$SBCE)
################################################################################ 
#'@ToDo2. get seperated data for each group
#'################################################################################ 
noSBCE_per_pts_data <-
yesSBCE_per_pts_data <- 

  ################################################################################ 
#'@ToDo2. remove code no pts has it, cuz filter valid month data
#'################################################################################ 
#'
get_pts_code_df <- function(analysis_df,analysis_code_df,code_type){
  #analysis_df <- per_pts_data
  #analysis_code_df <- unique_Drug_Grp_df
  #code_type <- "Drug_Codes"
  
  anlaysis_ID <- unique(analysis_df[,"ID"])
  anlaysis_code <-   unique(analysis_code_df[,"Code"])
  
  pts_code_df <- as.data.frame(matrix(NA, nrow = length(anlaysis_ID), ncol = length(anlaysis_code)))
  colnames(pts_code_df) <- anlaysis_code
  rownames(pts_code_df) <- anlaysis_ID
  for (i in 1:length(anlaysis_ID)){
    if (i %% 100 == 0){
      print(i)
    }
    curr_id <- anlaysis_ID[i]
    curr_df <- analysis_df[i,]
    curr_codes <- unlist(strsplit(as.character(curr_df[,code_type]),split = "$$$$",fixed = T))
    curr_code_idxes <- which(colnames(pts_code_df) %in% curr_codes)
    if (length(curr_code_idxes) > 0 ){
      pts_code_df[i,curr_code_idxes] <- 1 #pts has the code once in all valid month
    }
  }
  return(pts_code_df)
}


pts_diag_code_df <- get_pts_code_df(per_pts_data,unique_Diag_Grp_df , "Diag_Codes")
save(pts_diag_code_df,file = paste0(outdir,"pts_diag_code_df.rda"))

pts_proc_code_df <- get_pts_code_df(per_pts_data,unique_Proc_Grp_df , "Proc_Codes")
save(pts_proc_code_df,file = paste0(outdir,"pts_proc_code_df.rda"))

pts_drug_code_df <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df , "Drug_Codes")


pts_drug_code_df1 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[1: 10000,], "Drug_Codes")
pts_drug_code_df2 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[10001: 20000,], "Drug_Codes")
pts_drug_code_df3 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[20001: 30000,], "Drug_Codes")
pts_drug_code_df4 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[30001: 35000,], "Drug_Codes")
pts_drug_code_df5 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[35001: 37000,], "Drug_Codes")
pts_drug_code_df6 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[37001: 37600,], "Drug_Codes")
pts_drug_code_df7 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[37601: 38000,], "Drug_Codes")
pts_drug_code_df8 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[38001: 38100,], "Drug_Codes")
pts_drug_code_df9 <- get_pts_code_df(per_pts_data,unique_Drug_Grp_df[38101: 41271,], "Drug_Codes")

pts_drug_code_df <- cbind(pts_drug_code_df1,pts_drug_code_df2,pts_drug_code_df3,pts_drug_code_df4,
                          pts_drug_code_df5,pts_drug_code_df6,pts_drug_code_df7,pts_drug_code_df8,
                          pts_drug_code_df9)
save(pts_drug_code_df,file = paste0(outdir,"pts_drug_code_df.rda"))


##Get code freq/fraction table
get_Code_freq_func2 <-function(analysis_df,code_type,pts_code_df){
  #analysis_df <- unique_Diag_Grp_df
  #code_type <- "Diag_Codes"
  #pts_code_df <- pts_diag_code_df
    
  total_n_pts <- nrow(pts_code_df)
  
  analysis_df$N_PTS_HASCODE <- NA
  analysis_df$Frac_PTS_HASCODE <- NA
  for (i in 1:nrow(analysis_df)){
    if (i %% 1000 == 0){
      print(i)
    }
    curr_code <- as.character(analysis_df[i,"Code"])
    curr_col <- pts_code_df[,curr_code]
    n_pts_hascode <- length(which(curr_col == 1))
    frac_pts_has_code <- n_pts_hascode/total_n_pts
    
    analysis_df[i,"N_PTS_HASCODE"] <- n_pts_hascode
    analysis_df[i,"Frac_PTS_HASCODE"] <- frac_pts_has_code
  }
  analysis_df$Frac_PTS_HASCODE <- round(analysis_df$Frac_PTS_HASCODE,2)
  return(analysis_df)
}

diag_codes_freq <- get_Code_freq_func2(unique_Diag_Grp_df,"Diag_Codes",pts_diag_code_df)
write.csv(diag_codes_freq,paste0(outdir,"Code_FreqTable/diag_codes_freq.csv"),row.names = F)

proc_codes_freq <- get_Code_freq_func2(unique_Proc_Grp_df,"Proc_Codes",pts_proc_code_df)
write.csv(proc_codes_freq,paste0(outdir,"Code_FreqTable/proc_codes_freq.csv"),row.names = F)


drug_codes_freq <- get_Code_freq_func2(unique_Drug_Grp_df,"Drug_Codes",pts_drug_code_df)
write.csv(drug_codes_freq,paste0(outdir,"Code_FreqTable/drug_codes_freq.csv"),row.names = F)
