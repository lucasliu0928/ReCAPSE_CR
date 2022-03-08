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
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1  <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")
data_dir2  <- paste0(proj_dir, "6_CleanClaims_InValidMonth/EnrolledMonths_WithPossibleMonthsHasNoCodes3/")
data_dir3  <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir   <- paste0(proj_dir, "10F2_VAL2NDFeature_inValidMonth/WithPossibleMonthsHasNoCodes/")

################################################################################
#1.Load group df
################################################################################
drug_grp_df <- read.xlsx(paste0(data_dir1,"Unique_Drug_And_Groups_inALLClaims.xlsx"),sheet = 1)

#reformat 
#Drug:
indx1 <- which(drug_grp_df[,"TYPE"] == "DRUG_THERA_CLS_AHFS")
drug_grp_df[indx1,"TYPE"] <- "DRUG_AHFS"  #change type name

#replace space, "(" and "/" by "_"
# drug_grp_df[,"VAL_SECONDARY_group"] <- gsub(" |/|\\(","_",drug_grp_df[,"VAL_SECONDARY_group"])
# drug_grp_df[,"VAL_SECONDARY_group"] <- gsub(")","",drug_grp_df[,"VAL_SECONDARY_group"])
# drug_grp_df[,"VAL_SECONDARY_group"] <- gsub("___","_",drug_grp_df[,"VAL_SECONDARY_group"])
# drug_grp_df[,"VAL_SECONDARY_group"] <- gsub("__","_",drug_grp_df[,"VAL_SECONDARY_group"])
#add prefix
drug_grp_df[,"VAL_SECONDARY_group"] <- paste0("VAL_2ND_",drug_grp_df[,"VAL_SECONDARY_group"])
colnames(drug_grp_df)[which(colnames(drug_grp_df) == "VAL_SECONDARY_group")] <- "VAL_2ND" #change column names




################################################################################
#2.Final IDs
################################################################################
Final_ID_df <- read.xlsx(paste0(data_dir3,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
analysis_IDs <- Final_ID_df[,"study_id"]

########################################################################################################################
#Use the following code to run in case out of memory when procssing all at one time
########################################################################################################################
grp_type <- "VAL_2ND"
ID_processed <- as.numeric(gsub(paste0("_Month_",grp_type, "_Feature.xlsx|ID"),"",list.files(paste0(outdir, "Feature/"))))
if (length(ID_processed) != 0 ){
  analysis_IDs <- analysis_IDs[-which(analysis_IDs %in% ID_processed)]
}
print(length(analysis_IDs))


########################################################################################################################
#Generate per Month group feature, and get unique groups per patient
########################################################################################################################
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
    
    #unique codes
    curr_drug_AHFS_codes  <-   get_codes_func(code_names,"DRUG_AHFS")
    curr_drug_NDC_codes   <-  get_codes_func(code_names,"DRUG_NDC")
    unique_codes_df   <- rbind(curr_drug_AHFS_codes,curr_drug_NDC_codes)
    
    if (is.null(unique_codes_df) == T){ #if does not have any code in this type
      curr_grp_feature_df <- curr_perMonth_df[,1:4] #only keep id and month
      unique_grps_df <- data.frame("unique_grps" = "NONE") #craete an emtpy unique grp df
    }else{
      #find grp info for each unique code
      unique_codes_df[,"GRPS"] <- find_listofcode_grp_func(unique_codes_df,grp_type,drug_grp_df)
      
      #unique groups
      unique_grps <- unique(unique_codes_df[,"GRPS"])
      unique_grps_df   <- as.data.frame(unique_grps)
      
      #Get group feature df
      curr_grp_feature_df <- create_grp_feature_df_func(curr_perMonth_df,unique_grps,unique_codes_df)
    }   
    #Output group feature df 
    write.xlsx(curr_grp_feature_df,paste0(outdir,"Feature/","ID",curr_id,"_Month_", grp_type, "_Feature.xlsx"))
    
    #Ouput unique grps 
    write.xlsx(unique_grps_df,paste0(outdir,"UniqueGrp/","ID",curr_id,"_Month_",grp_type, "_UniqueGrps.xlsx"))
  }
}

                