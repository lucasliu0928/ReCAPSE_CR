get_all_unique_code_perPt <- function(one_pt_data,code_col_name){
  #this function get all uique codes the one patient ever had
  
  #get all specific code
  all_codes <- one_pt_data[,code_col_name]
  #remove NA day data
  all_codes <- all_codes[-which(is.na(all_codes) == T)]
  
  #unique codes ever had
  #'@NOTE if code_col_name == "Drug_Codes", after split $$$$, convert then to numeric due to the prepending 0s
  if (code_col_name == "Drug_Codes"){
    unique_codes <- unique(as.numeric(unlist(strsplit(all_codes,split = "$$$$",fixed = T))))
  }else{
    unique_codes <- unique(unlist(strsplit(all_codes,split = "$$$$",fixed = T)))
  }
  
  return(unique_codes)
  
}

get_perPtsData_func <- function(perday_data,analysis_Ids){

  #this functions converts patient per day data to per pts data
  #Each row is one patient, columns are unique diag, proc and durg codes the patient every had
  
  per_pts_data<- as.data.frame(matrix(NA, nrow = length(analysis_Ids), ncol = 4))
  colnames(per_pts_data) <- c("ID","Unique_Diag_Codes_inAllValidMonth",
                              "Unique_Proc_Codes_inAllValidMonth",
                              "Unique_Drug_Codes_inAllValidMonth")
  for (i in 1:length(analysis_Ids)){
    if (i %% 500 == 0){
      print(i)
    }
    #curr id
    curr_id <- analysis_Ids[i]  
    #curr per day data
    curr_df <- perday_data[which(perday_data[,"study_id"] == curr_id),]
    #curr all unique diag codes
    curr_all_diag_codes <- get_all_unique_code_perPt(curr_df,"Diag_Codes")
    curr_all_diag_codes <- paste0(curr_all_diag_codes,collapse = "$$$$")
    
    curr_all_proc_codes <- get_all_unique_code_perPt(curr_df,"Proc_Codes")
    curr_all_proc_codes <- paste0(curr_all_proc_codes,collapse = "$$$$")
    
    curr_all_drug_codes <- get_all_unique_code_perPt(curr_df,"Drug_Codes")
    options(scipen = 999)  #turn off scentict notation for 1e+07
    curr_all_drug_codes <- paste0(as.character(curr_all_drug_codes),collapse = "$$$$") #convert back to char for "1e+07"

    per_pts_data[i,"ID"] <- curr_id
    per_pts_data[i,"Unique_Diag_Codes_inAllValidMonth"] <- curr_all_diag_codes
    per_pts_data[i,"Unique_Proc_Codes_inAllValidMonth"] <- curr_all_proc_codes
    per_pts_data[i,"Unique_Drug_Codes_inAllValidMonth"] <- curr_all_drug_codes
    
  }
  
  return(per_pts_data)
  
}

library(openxlsx)
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
#### Load per day data
################################################################################ 
perday_data <- read.csv(paste0(data_dir,"filtered_inValidMonth_comb_perday_df.csv"),stringsAsFactors = F)
perday_data_Ids <- unique(perday_data$study_id)
length(perday_data_Ids) #this is the finaly anlyaisi ID which is the same in  "finaly_anlaysi Id file"

################################################################################ 
#Get per pts data
################################################################################ 
per_pts_data <- get_perPtsData_func(perday_data,final_analysis_ID)
write.csv(per_pts_data,paste0(outdir,"per_pts_data.csv"),row.names = F)
