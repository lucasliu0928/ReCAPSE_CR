source("Recapse_Ultility.R")
#This scrip generate CCS diag feature per month for each final ID (All enrolls)

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
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")
data_dir2  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes3/")
data_dir3  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "10B_CCSDiagFeature_inValidMonth/WithPossibleMonthsHasNoCodes/")

################################################################################
#1.Load group df
################################################################################
diag_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Diag_And_Groups_inALLClaims.xlsx"),sheet = 1)
#proc_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Proc_And_Groups_inALLClaims.xlsx"),sheet = 1)
#drug_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Drug_And_Groups_inALLClaims.xlsx"),sheet = 1)

#reformat 
#Diag:
diag_grp_df[,"TYPE"] <- "DIAG_ICD"  #change type name
diag_grp_df[,"CCS_CATEGORY"] <- paste0("CCS_DIAG_",diag_grp_df[,"CCS_CATEGORY"])
colnames(diag_grp_df)[which(colnames(diag_grp_df) == "CCS_CATEGORY")] <- "CCS_DIAG" #change column names

#Procedure:
indx1 <- which(proc_grp_df[,"TYPE"] == "PROC_ICD9or10")
proc_grp_df[indx1,"TYPE"] <- "PROC_ICD"  #change type name
proc_grp_df[,"CCS_CATEGORY"] <- paste0("CCS_PROC_",proc_grp_df[,"CCS_CATEGORY"])
colnames(proc_grp_df)[which(colnames(proc_grp_df) == "CCS_CATEGORY")] <- "CCS_PROC" #change column names

#Drug:
indx1 <- which(drug_grp_df[,"TYPE"] == "DRUG_THERA_CLS_AHFS")
drug_grp_df[indx1,"TYPE"] <- "DRUG_AHFS"  #change type name
drug_grp_df[,"specific_group"] <- paste0("DM3_SPE_",drug_grp_df[,"specific_group"])
colnames(drug_grp_df)[which(colnames(drug_grp_df) == "specific_group")] <- "DM3_SPE" #change column names


drug_grp_df[,"general_group"] <- paste0("DM3_GEN_",drug_grp_df[,"general_group"])
colnames(drug_grp_df)[which(colnames(drug_grp_df) == "general_group")] <- "DM3_GEN" #change column names

drug_grp_df[,"short_GNN"] <- paste0("S_GNN_",drug_grp_df[,"short_GNN"])
colnames(drug_grp_df)[which(colnames(drug_grp_df) == "short_GNN")] <- "S_GNN" #change column names

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
  
  #Make sure no code has all NAs rows
  #NOTE this was also done in previous code when generate in prediction Window
  curr_perMonth_df <- curr_perMonth_df[,colSums(is.na(curr_perMonth_df))<nrow(curr_perMonth_df)]
  
  if (ncol(curr_perMonth_df) > 4){ #Make sure there is any code left in the df, if not, this pts should be excluded for final
    code_names <- colnames(curr_perMonth_df)[5:ncol(curr_perMonth_df)]
    
    #current codes
    curr_diag_ICD_codes   <- get_codes_func(code_names,"DIAG_ICD")
    curr_proc_ICD_codes   <-  get_codes_func(code_names,"PROC_ICD")
    curr_proc_HCPCS_codes <-   get_codes_func(code_names,"PROC_HCPCS")
    curr_drug_AHFS_codes  <-   get_codes_func(code_names,"DRUG_AHFS")
    curr_drug_NDC_codes   <-  get_codes_func(code_names,"DRUG_NDC")
    
    all_code_df <- rbind(curr_diag_ICD_codes)
    all_code_df$GRPS <- find_listofcode_grp_func(all_code_df,"CCS_DIAG",diag_grp_df)
    
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
}


