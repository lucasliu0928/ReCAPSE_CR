source("Recapse_Ultility.R")
get_codes_func <- function(cols_inData,code_type){
  colnames_indata <- cols_inData[which(grepl(code_type,cols_inData)==T)]
  codes   <- gsub(paste0(code_type,"_"),"",colnames_indata)
  
  code_df <- data.frame(COLNAMES = colnames_indata, CODE = codes,TYPE = code_type)
  return(code_df)
}

find_grp_func <- function(list_of_codes,type_of_codes,diag_grp_df,proc_grp_df,drug_grp_df){
  list_of_codes <- as.character(all_code_df$CODE)
  type_of_codes <- as.character(all_code_df$TYPE)
  GRPs <- NA
  for (i in 1:length(list_of_codes)){
    curr_code <- list_of_codes[i]
    curr_type <- type_of_codes[i]
    if (curr_type == "DIAG_ICD"){
      idx <- which(diag_grp_df[,"CODE"] == curr_code & 
                     diag_grp_df[,"TYPE"] == "DIAG_ICD9or10")
      if(length(idx) > 0){
        curr_grp <- diag_grp_df[idx,"CCS_CATEGORY"]
      }else{
        curr_grp <- NA
      }
    }else if (curr_type == "PROC_ICD"){
      idx <- which(proc_grp_df[,"CODE"] == curr_code & 
                     proc_grp_df[,"TYPE"] == "PROC_ICD9or10")
      if(length(idx) > 0){
        curr_grp <- proc_grp_df[idx,"CCS_CATEGORY"]
      }else{
        curr_grp <- NA
      }
    }else if (curr_type == "PROC_HCPCS"){
      idx <- which(proc_grp_df[,"CODE"] == curr_code & 
                     proc_grp_df[,"TYPE"] == "PROC_HCPCS")
      if(length(idx) > 0){
        curr_grp <- proc_grp_df[idx,"CCS_CATEGORY"]
      }else{
        curr_grp <- NA
      }
    }else if (curr_type == "DRUG_AHFS"){
      idx <- which(drug_grp_df[,"CODE"] == curr_code & 
                     drug_grp_df[,"TYPE"] == "DRUG_THERA_CLS_AHFS")
      if(length(idx) > 0){
        curr_grp <- drug_grp_df[idx,"specific_group"]
      }else{
        curr_grp <- NA
      }
    }else if (curr_type == "DRUG_NDC"){
      idx <- which(drug_grp_df[,"CODE"] == curr_code & 
                     drug_grp_df[,"TYPE"] == "DRUG_NDC")
      if(length(idx) > 0){
        curr_grp <- drug_grp_df[idx,"specific_group"]
      }else{
        curr_grp <- NA
      }
    }
    
    GRPs[i] <- curr_grp
    
  }
  return(GRPs)
}

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")
data_dir2  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes/")
data_dir3  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "/10A_CCSGrpedFeatures_inValidMonth/WithPossibleMonthsHasNoCodes")
outdir2  <- paste0(proj_dir, "/Pts_Level_Unique_Grps_inValidMonths/WithPossibleMonthsHasNoCodes")
################################################################################
#1.Load group df
################################################################################
diag_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Diag_And_Groups_inALLClaims.xlsx"),sheet = 1)
proc_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Proc_And_Groups_inALLClaims.xlsx"),sheet = 1)
drug_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Drug_And_Groups_inALLClaims.xlsx"),sheet = 1)

diag_grp_df$CCS_CATEGORY <- paste0("CCS_DIAG_",diag_grp_df$CCS_CATEGORY)
proc_grp_df$CCS_CATEGORY <- paste0("CCS_PROC_",proc_grp_df$CCS_CATEGORY)
drug_grp_df$specific_group <- paste0("DM3_SPE_",drug_grp_df$specific_group)
drug_grp_df$general_group <- paste0("DM3_GEN_",drug_grp_df$general_group)

################################################################################
#get per months files
################################################################################
perMonth_files <- list.files(data_dir2)

################################################################################
#3.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir3,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
ID_processed <- as.numeric(gsub("_Month_Grp_Feature.xlsx|ID","",list.files(outdir)))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))

foreach (i = 1: length(analysis_IDs)) %dopar% {
  curr_id <- analysis_IDs[i]
  curr_file <- paste0("ID",curr_id,"_perMonthData_Enrolled_inPredictionWindow.xlsx")
  
  #per month df
  curr_perMonth_df <- read.xlsx(paste0(data_dir2,curr_file),sheet = 1)
  
  #'@TODO: this can also be updated later in previous code when generate in predictino Window
  #updated per month df, remove code has all NAs in rows (This is due to when filter valid month, code in non-valid month are still kept)
  curr_perMonth_df <- curr_perMonth_df[,colSums(is.na(curr_perMonth_df))<nrow(curr_perMonth_df)]  
  col_names <- colnames(curr_perMonth_df)
  
  #current codes
  curr_diag_ICD_codes   <- get_codes_func(col_names,"DIAG_ICD")
  curr_proc_ICD_codes   <-  get_codes_func(col_names,"PROC_ICD")
  curr_proc_HCPCS_codes <-   get_codes_func(col_names,"PROC_HCPCS")
  curr_drug_AHFS_codes  <-   get_codes_func(col_names,"DRUG_AHFS")
  curr_drug_NDC_codes   <-  get_codes_func(col_names,"DRUG_NDC")
  
  all_code_df <- rbind(curr_diag_ICD_codes,curr_proc_ICD_codes,curr_proc_HCPCS_codes,curr_drug_AHFS_codes,curr_drug_NDC_codes)
  all_code_df$GRPS <- find_grp_func(all_code_df[,"CODE"],all_code_df[,"TYPE"],diag_grp_df,proc_grp_df,drug_grp_df)

  #update per month df col names with ccs grp
  unique_grps <- unique(all_code_df[,"GRPS"])
  
  curr_grp_feature_df <- curr_perMonth_df[,1:4] #keep id and month
  curr_grp_feature_df[,unique_grps] <- NA #new grp feature cols
  
  for (j in 5:ncol(curr_grp_feature_df)){
    curr_grp <- colnames(curr_grp_feature_df)[j]
    curr_codes_ingrp <- as.character(all_code_df[which(all_code_df[,"GRPS"] == curr_grp),"COLNAMES"])
    curr_col_idx_ingrps <- which(colnames(curr_perMonth_df) %in% curr_codes_ingrp)
    
    curr_df <- as.data.frame(curr_perMonth_df[,curr_col_idx_ingrps])
    curr_grp_feature_df[,j] <- rowSums(curr_df,na.rm = T)
    
  }
  write.xlsx(curr_grp_feature_df,paste0(outdir,"ID",curr_id,"_Month_Grp_Feature.xlsx"))
  
  #Ouput unique grps for each patients
  unique_grps_df <- as.data.frame(unique_grps)
  write.xlsx(unique_grps_df,paste0(outdir2,"ID",curr_id,"_Month_Unique_Grps.xlsx"))
}