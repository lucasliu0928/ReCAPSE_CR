clean_code_func <-function(analysis_grp_df,code_col){
  #analysis_grp_df <- chuback_group_df
  #code_col <- "Code"
  
  #omitting any codes with non-alphanumeric characters,
  analysis_grp_df[,code_col] <- gsub("[^[:alnum:]]", " ", analysis_grp_df[,code_col])
  
  #space
  analysis_grp_df[,code_col] <- trimws(analysis_grp_df[,code_col], which = c("both"), whitespace = "[ \t\r\n]")
  
  #decimal
  analysis_grp_df[,code_col] <- gsub("\\.","",analysis_grp_df[,code_col])
 
  analysis_grp_df[,code_col] <- gsub("[[:space:]]", "", analysis_grp_df[,code_col])
  
  #prepending '0' to numeric codes less than 3 characters long
  n_char <- NA
  for (i in 1:nrow(analysis_grp_df)){
    cur_code <- analysis_grp_df[i,code_col]
    n_char[i] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
  #.Frist check if they are numeric
  l3_idxes <- which(n_char< 3)
  l3_codes <- analysis_grp_df[l3_idxes,code_col]
  
  if(length(l3_codes) > 0){
      updated_code <- NA
      for (i in 1:length(l3_codes)){
        cur_code <- l3_codes[i]
        if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric #mitting codes that were less than 3 characters long
          updated_code[i] <- NA #remove
        }else{ #it is numeric , then prepending '0'
          updated_code[i] <- paste0("0",cur_code)
        }
      }
      analysis_grp_df[l3_idxes,code_col] <- updated_code
      
      #Then Remove the NAs(from converting to numeric when it is char) from orignal df
      na_idxes <- which(is.na(analysis_grp_df[,code_col]==T))
      if (length(na_idxes) > 0){
        analysis_grp_df <- analysis_grp_df[-na_idxes,]
      }
  }
  
  return(analysis_grp_df)
}


grouping_func <- function(analayis_codes,data_type ){
  #analayis_codes <- unique_all_diag
  #data_type <- "Diag"
  
  problem_list_CCS <- list()
  problem_list_ritz <- list()
  problem_list_chubak<- list()
  ct <- 1
  ct2 <- 1
  ct3 <- 1
  group_df <- as.data.frame(matrix(NA, nrow = length(analayis_codes) , ncol = 5))
  colnames(group_df) <- c("Code","Chubak_type", "Chubak_catogory", "Ritzwoller_catogory", "CCS_catogory")
  for (i in 1:length(analayis_codes)){
    if (i %% 1000 == 0){
      print(i)
    }
    curr_code <- analayis_codes[i]
    group_df[i,"Code"] <- curr_code
    if (data_type == "Diag"){
      chuback_phase <- "Diagnosis"
      chuback_codetype <- c()
      ritz_phase <- "Diagnostic"
      code_type <- c("ICD9_Diag", "ICD10_Diag")
    }else if (data_type == "Proc"){
      chuback_phase <- "Treatment"
      chuback_codetype <- c()
      ritz_phase <- "Procedure"
      code_type <- c("ICD9_Proc", "ICD10_Proc")
    }
    
    curr_chubak <- updated_chuback_group_df[which(updated_chuback_group_df[,"Code"] == curr_code &
                                                    updated_chuback_group_df[,"Phase"] == chuback_phase &
                                                    updated_chuback_group_df[,"Code.type"] %in%  chuback_codetype),]
    curr_Ritzwoller <- updated_Ritzwoller_group_df[which(updated_Ritzwoller_group_df[,"Code"] == curr_code &
                                                           updated_Ritzwoller_group_df[,"D_or_P"] ==  ritz_phase),]
    

    curr_CCS <- updated_HCUP_comb[which(updated_HCUP_comb[,"Code"] == curr_code &
                                        updated_HCUP_comb[,"CODE_TYPE"] %in% code_type),]
    
        if (nrow(curr_chubak) > 0){
          if (nrow(curr_chubak) >1){
            problem_list_chubak[[ct]] <- curr_chubak
            ct <- ct + 1
          }else{
            group_df[i,"Chubak_type"] <- curr_chubak[,"Type"]
            group_df[i,"Chubak_catogory"] <- curr_chubak[,"Category"]
          }
          
        }

    
      if (nrow(curr_Ritzwoller) > 0){
        if (nrow(curr_Ritzwoller) >1){
           problem_list_ritz[[ct2]] <- curr_Ritzwoller
           ct2 <- ct2 + 1
        }else{
          group_df[i,"Ritzwoller_catogory"] <- curr_Ritzwoller[,"Category"]
        }
      }
      if (nrow(curr_CCS) > 0){
        if (nrow(curr_CCS) >1){ #if still have duplicates
          problem_list_CCS[[ct3]] <- curr_CCS
          ct3 <- ct3 + 1
        }else{
         group_df[i,"CCS_catogory"] <- curr_CCS[,"CCS.CATEGORY"]
        }
      }
    
  }
  
  return(list(group_df,problem_list_chubak,problem_list_ritz,problem_list_CCS))
}


out_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0318_21/"
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
#load chuback file:
grp_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/"
chuback_group_df <- read.csv(paste0(grp_dir,"BRAVA_lookup.20180502.edit.csv"),stringsAsFactors = F)

#load Ritzwoller file:
Ritzwoller_group_df <- read.csv(paste0(grp_dir,"Ritzwoller_code_table.edited.csv"),stringsAsFactors = F)
#load HCUP CCS file:
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


#Clean codes
updated_chuback_group_df<- clean_code_func(chuback_group_df,"Code")
updated_Ritzwoller_group_df <- clean_code_func(Ritzwoller_group_df,"Code")
updated_HCUP_comb  <- clean_code_func(HCUP_comb ,"Code")
#Clean category in HCUP
updated_HCUP_comb[,"CCS.CATEGORY"] <- gsub("[^[:alnum:]]", " ", updated_HCUP_comb[,"CCS.CATEGORY"])
updated_HCUP_comb[,"CCS.CATEGORY"] <- trimws(updated_HCUP_comb[,"CCS.CATEGORY"], which = c("both"), whitespace = "[ \t\r\n]")


#Load claims
health_claims<- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
health_claims$DTE_FIRST_SVC <- mdy(health_claims$DTE_FIRST_SVC)

pharm_claims<- read.csv(paste0(data_dir,"KCR_MEDICAID_PHARMCLAIMS_FB0015.csv"),stringsAsFactors = F)
#convert date col
pharm_claims$DTE_FIRST_SVC <- dmy(pharm_claims$DTE_FIRST_SVC)

diag_cols <- c("CDE_DIAG_PRIM","CDE_DIAG_2","CDE_DIAG_3","CDE_DIAG_4")
proc_cols <- c("CDE_PROC_PRIM")
durg_cols <- c("CDE_THERA_CLS_AHFS","CDE_NDC")

#get all diag_codes
all_diag_cols <- health_claims[,diag_cols]
all_diag <- unlist(all_diag_cols)
all_diag <- all_diag[-which(all_diag == "")]
unique_all_diag <-  unique(all_diag)

unique_all_diag_df <-as.data.frame(unique_all_diag)
colnames(unique_all_diag_df) <- "Code"
unique_all_diag_df <- clean_code_func(unique_all_diag_df,"Code")

#Find groups
res <- grouping_func(unique_all_diag_df$Code,"Diag")
diag_group_df <- res[[1]]
problem_list_chubak <- res[[2]]
problem_list_ritz <- res[[3]]
problem_list_CCS <- res[[4]]

problem_chubak_df <- do.call(rbind,problem_list_chubak)
problem_ritz_df <- do.call(rbind,problem_list_ritz)
problem_ccs_df <- do.call(rbind,problem_list_CCS)



#get all proc 
all_proc_cols <- health_claims[,proc_cols]
all_proc <- unlist(all_proc_cols)
all_proc <- all_proc[-which(all_proc == "")]
unique_all_proc <-  unique(all_proc)

unique_all_proc_df <-as.data.frame(unique_all_proc)
colnames(unique_all_proc_df) <- "Code"
unique_all_proc_df <- clean_code_func(unique_all_proc_df,"Code")

#Find groups
res <- grouping_func(unique_all_proc_df$Code,"Proc")
proc_group_df <- res[[1]]
problem_list_chubak <- res[[2]]
problem_list_ritz <- res[[3]]
problem_list_CCS <- res[[4]]

problem_chubak_df2 <- do.call(rbind,problem_list_chubak)
problem_ritz_df2 <- do.call(rbind,problem_list_ritz)
problem_ccs_df2 <- do.call(rbind,problem_list_CCS)
