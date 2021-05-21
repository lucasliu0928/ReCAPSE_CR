library(lubridate)
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/" 
source(paste0(proj_dir,"ReCAPSE_Code/Ultilities.R"))


data_dir <- paste0(proj_dir,"/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/")
intermediate_dir <-  paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
grp_dir <- paste0(proj_dir,"ReCAPSE_Data/")
outdir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")

########################################################################   
#####                 load chuback file:                       #########  
########################################################################   
chuback_group_df <- read.csv(paste0(grp_dir,"BRAVA_lookup.20180502.edit.csv"),stringsAsFactors = F)
length(unique(chuback_group_df$Type)) #216
length(unique(chuback_group_df$Category)) #22

########################################################################   
#####                 load Ritzwoller file:                    #########  
######################################################################## 
Ritzwoller_group_df <- read.csv(paste0(grp_dir,"Ritzwoller_code_table.edited.csv"),stringsAsFactors = F)
length(unique(Ritzwoller_group_df$Category)) #6

########################################################################   
######                 load HCUP CCS file:                      ########
########################################################################   
HCUP_Diag1_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-9.diag_ref.edit.csv"),stringsAsFactors = F)
HCUP_Diag2_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-10.diag_ref.edit.csv"),stringsAsFactors = F)
HCUP_Proc1_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-9.proc_ref.edit.csv"),stringsAsFactors = F)
HCUP_Proc2_df <- read.csv(paste0(grp_dir,"HCUP_CCS_tables/CCS.ICD-10.proc_ref.edit.csv"),stringsAsFactors = F)

#Change col name to comb
colnames(HCUP_Diag1_df)[which(colnames(HCUP_Diag1_df) == "ICD.9.CM.CODE")] <- "Code"
colnames(HCUP_Diag2_df)[which(colnames(HCUP_Diag2_df) == "ICD.10.CM.CODE")] <- "Code"
colnames(HCUP_Proc1_df)[which(colnames(HCUP_Proc1_df) == "ICD.9.CM.CODE")] <- "Code"
colnames(HCUP_Proc2_df)[which(colnames(HCUP_Proc2_df) == "ICD.10.PCS.CODE")] <- "Code"

HCUP_Diag1_df$CODE_TYPE <- "ICD9_Diag"
HCUP_Diag2_df$CODE_TYPE <- "ICD10_Diag"
HCUP_Proc1_df$CODE_TYPE <- "ICD9_Proc"
HCUP_Proc2_df$CODE_TYPE <- "ICD10_Proc"

HCUP_comb <- rbind(HCUP_Diag1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Diag2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Proc1_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")],
                   HCUP_Proc2_df[,c("Code","CCS.CATEGORY","CCS.CATEGORY.DESCRIPTION","CODE_TYPE")])

length(unique(HCUP_comb$CCS.CATEGORY)) #276

########################################################################   
#####                         Clean codes                          ##### 
########################################################################   
chuback_group_df[,"Code"]<- clean_code_func(chuback_group_df[,"Code"])
Ritzwoller_group_df[,"Code"] <- clean_code_func(Ritzwoller_group_df[,"Code"])
HCUP_comb[,"Code"]  <- clean_code_func(HCUP_comb[,"Code"])
HCUP_comb[,"CCS.CATEGORY"]  <- clean_code_func(HCUP_comb[,"CCS.CATEGORY"]) #Clean category in HCUP

########################################################################   
##                  remove all blanks and NAs                         ##    
######################################################################## 
chuback_group_df <- remove_NA_from_df(chuback_group_df,"Code")
Ritzwoller_group_df <- remove_NA_from_df(Ritzwoller_group_df,"Code")
HCUP_comb <- remove_NA_from_df(HCUP_comb,"Code")
#remove the unspecified codes, cuz they will result codes grped into multiple groups, (e.g, E8342 is a qulified codes in ICD10, but unspecified in ICD9)
unspecified_idxes <- which(grepl("\\be codes|\\bExternal cause codes",HCUP_comb[,"CCS.CATEGORY.DESCRIPTION"],ignore.case = T)==T)
HCUP_comb<- HCUP_comb[-unspecified_idxes,]

########################################################################   
##  Orgnized type for dianoises or procedure                          ##    
######################################################################## 
#chuback
diag_values <- c("ICD-9 diagnosis")
proc_values <- c("HCPC","ICD-9 procedure","CPT","CPT category II","CPT category III")
chuback_group_df <- updated_codetype_DorP(chuback_group_df,"Code.type",diag_values,proc_values)

#Ritzwoller has a column for D or P , just change the column name
col_idx <- which(colnames(Ritzwoller_group_df) == "D_or_P")
colnames(Ritzwoller_group_df)[col_idx] <- "Updated_Code_Type"

#HCUP
diag_values <- c("ICD9_Diag", "ICD10_Diag")
proc_values <- c("ICD9_Proc", "ICD10_Proc")
HCUP_comb <- updated_codetype_DorP(HCUP_comb,"CODE_TYPE",diag_values,proc_values)

d_idxes <- which(chuback_group_df$Updated_Code_Type == "Diagnostic")
p_idxes <- which(chuback_group_df$Updated_Code_Type == "Procedure")
length(unique(chuback_group_df$Type[d_idxes])) #77
length(unique(chuback_group_df$Type[p_idxes])) #156

######################################################################## 
##  Group Diagnoise codes
######################################################################## 
#Load unique diagnoise code
unique_all_diag_df <- read.csv(paste0(intermediate_dir,"All_unique_Diag_codes_Cleaned.csv"),stringsAsFactors = F)
#Grouping
grouped_df_diag <- grouping_DandP_func(unique_all_diag_df[,"unique_Diag_codes"],"Diagnostic",chuback_group_df,Ritzwoller_group_df,HCUP_comb)

###Report no group type numbers
length(which(is.na(grouped_df_diag[,"Chubak_type"])==T)) #29238 No Chubak_type
length(which(is.na(grouped_df_diag[,"Chubak_catogory"])==T)) #29238 No Chubak_catogory
length(which(is.na(grouped_df_diag[,"Ritzwoller_catogory"])==T)) #30010 No Ritzwoller_catogory
length(which(is.na(grouped_df_diag[,"CCS_catogory"])==T)) #3409 No CCS_catogory

#Report unique group type numbers
length(unique(grouped_df_diag[,"Chubak_type"])) #808 codes -> 79 groups
length(unique(grouped_df_diag[,"Chubak_catogory"])) #808 codes -> 16 groups
length(unique(grouped_df_diag[,"Ritzwoller_catogory"])) #36 codes -> 5 groups
length(unique(grouped_df_diag[,"CCS_catogory"])) #26637 codes -> 265 groups



#'@Note: It is possible that one code mapped into two type or category, use $$$$ to cacatenate
#check which code grouped into two catogory
codes1_multi_grp <- check_code_Multiple_grps(grouped_df_diag,"Chubak_type")
codes2_multi_grp <- check_code_Multiple_grps(grouped_df_diag,"Chubak_catogory")
codes3_multi_grp <- check_code_Multiple_grps(grouped_df_diag,"Ritzwoller_catogory")
codes4_multi_grp <- check_code_Multiple_grps(grouped_df_diag,"CCS_catogory")



######################################################################## 
##  Group Procedure codes
######################################################################## 
#Load unique procedure code
unique_all_proc_df <- read.csv(paste0(intermediate_dir,"All_unique_Proc_codes_Cleaned.csv"),stringsAsFactors = F)
#Group
grouped_df_proc <- grouping_DandP_func(unique_all_proc_df[,"unique_Proc_codes"],"Procedure",chuback_group_df,Ritzwoller_group_df,HCUP_comb)

###Report has group type numbers
length(which(is.na(grouped_df_proc[,"Chubak_type"])==F)) #1471 has  Chubak_type
length(which(is.na(grouped_df_proc[,"Chubak_catogory"])==F)) #1471 has  Chubak_catogory
length(which(is.na(grouped_df_proc[,"Ritzwoller_catogory"])==F)) #655  Ritzwoller_catogory
length(which(is.na(grouped_df_proc[,"CCS_catogory"])==F)) # 4238 has CCS_catogory

###Report no group type numbers
length(which(is.na(grouped_df_proc[,"Chubak_type"])==T)) #16265 No Chubak_type
length(which(is.na(grouped_df_proc[,"Chubak_catogory"])==T)) #16265 No Chubak_catogory
length(which(is.na(grouped_df_proc[,"Ritzwoller_catogory"])==T)) #17081 No Ritzwoller_catogory
length(which(is.na(grouped_df_proc[,"CCS_catogory"])==T)) #13498 No CCS_catogory

#Report unique group type numbers
length(unique(grouped_df_proc[,"Chubak_type"])) #1471 codes -> 137 groups
length(unique(grouped_df_proc[,"Chubak_catogory"])) #1471 codes -> 34 groups
length(unique(grouped_df_proc[,"Ritzwoller_catogory"])) #655 codes -> 7 groups
length(unique(grouped_df_proc[,"CCS_catogory"])) #4238 codes -> 222 groups



#'@Note: It is possible that one code mapped into two type or category, use $$$$ to cacatenate
#check which code grouped into two catogory
codes1_multi_grp <- check_code_Multiple_grps(grouped_df_proc,"Chubak_type")
codes2_multi_grp <- check_code_Multiple_grps(grouped_df_proc,"Chubak_catogory")
codes3_multi_grp <- check_code_Multiple_grps(grouped_df_proc,"Ritzwoller_catogory")
codes4_multi_grp <- check_code_Multiple_grps(grouped_df_proc,"CCS_catogory")



######################################################################## 
##  Group Drug codes
######################################################################## 
#1.Load drug group df
DM3_df <- read.csv(paste0(grp_dir,"Drug Code Groups-DM3.sorted.csv"),stringsAsFactors = F)
DM3_df <- DM3_df[,-1]

#Clean drug name by removing the source prefix
DM3_df[,"desc"] <- gsub("NC: |NH: |NO: |NS: ","",DM3_df[,"desc"])
DM3_df[,"desc"] <- trimws(DM3_df[,"desc"], which = c("both"), whitespace = "[ \t\r\n]")
length(unique(DM3_df[,"desc"])) #553
length(unique(DM3_df[,"specific_group"])) #76
length(unique(DM3_df[,"general_group"])) #14


#2Load drug unique codes
unique_all_drugs_df <- read.csv(paste0(intermediate_dir,"All_unique_Drug_codes_Cleaned.csv"),stringsAsFactors = F)

#Group Drugs
grouped_df_drug <- unique_all_drugs_df
grouped_df_drug$specific_group <- NA
grouped_df_drug$general_group <- NA

for (i in 1:nrow(unique_all_drugs_df)){
  curr_durg_name <- unique_all_drugs_df[i,"drug_name"]
  if (is.na(curr_durg_name) == F){
     grp_idxes <- which(DM3_df[,"desc"] == curr_durg_name)
     if (length(grp_idxes) >0){
       curr_dm3_df <-  DM3_df[grp_idxes,]
       grouped_df_drug[i,"specific_group"] <- paste0(unique(curr_dm3_df[,"specific_group"]),collapse = "$$$$")
       grouped_df_drug[i,"general_group"] <- paste0(unique(curr_dm3_df[,"general_group"]),collapse = "$$$$")
     }
  }
  
}

colnames(grouped_df_drug)[1] <- "Code"

###Report has group type numbers
length(which(is.na(grouped_df_drug[,"specific_group"])==F)) #16041 has  specific_group
length(which(is.na(grouped_df_drug[,"general_group"])==F)) #16041 has  general_group

###Report no group type numbers
length(which(is.na(grouped_df_drug[,"specific_group"])==T)) #25230 has no  specific_group
length(which(is.na(grouped_df_drug[,"general_group"])==T)) #25230 has no general_group


#Report unique group type numbers
length(unique(grouped_df_drug[,"specific_group"])) #16041 codes -> 58 groups
length(unique(grouped_df_drug[,"general_group"])) #16041 codes -> 14 groups

#'@Note: It is possible that one code mapped into two type or category, use $$$$ to cacatenate
#check which code grouped into two catogory
codes1_multi_grp <- check_code_Multiple_grps(grouped_df_drug,"specific_group") #none
codes2_multi_grp <- check_code_Multiple_grps(grouped_df_drug,"general_group") #none

write.csv(grouped_df_diag,paste0(outdir,"Grouped_Diag_codes.csv"),row.names = F)
write.csv(grouped_df_proc,paste0(outdir,"Grouped_Proc_codes.csv"),row.names = F)
write.csv(grouped_df_drug,paste0(outdir,"Grouped_Drug_codes.csv"),row.names = F)

##############################################################################
####Count # of types and groups
####do not count one code in multiple grps as one extra type
##############################################################################
g_diag_df <- read.csv(paste0(outdir,"Grouped_Diag_codes.csv"),stringsAsFactors = F)
g_proc_df <- read.csv(paste0(outdir,"Grouped_Proc_codes.csv"),stringsAsFactors = F)
g_drug_df <- read.csv(paste0(outdir,"Grouped_Drug_codes.csv"),stringsAsFactors = F)

# 
# chuback_type_diag <- strsplit(g_drug_df$general_group,split = "$$$$",fixed = T)
# unique_chuback_type_diag <- unlist(chuback_type_diag)
# unique_chuback_type_diag <- unique_chuback_type_diag[-which(is.na(unique_chuback_type_diag)==T)]
# length(unique(unique_chuback_type_diag))
