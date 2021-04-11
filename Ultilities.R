#remove Na and blanks from vector/list
remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}

#remove NA from dataframe
remove_NA_from_df <- function(input_df,col_tocheck){
  na_indexes <- which(is.na(input_df[,col_tocheck])==T)
  if (length(na_indexes) > 0){
    input_df <- input_df[-na_indexes,]
  }
  return(input_df)
}

clean_code_func <-function(list_of_codes){
  #list_of_codes <- HCUP_comb[,"Code"]
  
  #1.omitting any codes with non-alphanumeric characters,
  updated_list_of_codes<- gsub("[^[:alnum:]]", " ", list_of_codes)
  
  #2. space
  updated_list_of_codes <- trimws(updated_list_of_codes, which = c("both"), whitespace = "[ \t\r\n]")
  
  #3.decimal
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
  #.if it is non-numeric, then exclude codes 
  # if it is numeric , then prepending '0'
  l3_idxes <- which(n_char< 3)
  l3_codes <- updated_list_of_codes[l3_idxes]
  if(length(l3_codes) > 0){
        updated_code <- NA
        for (c in 1:length(l3_codes)){
          cur_code <- l3_codes[c]
          if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric
            updated_code[c] <- NA #remove
          }else{ #it is numeric , then prepending '0'
            updated_code[c] <- paste0("0",cur_code)
          }
        }
        
    updated_list_of_codes[l3_idxes] <- updated_code
    
    #This might result in NAs(from converting to numeric when it is char) from orignal 
  }
  
  return(updated_list_of_codes)
}


###################################################################################################
####  Funcstions for grouping
###################################################################################################
#Add a new column for diag or procedure instead of original (ICD9 procedure, HCPC ...)
updated_codetype_DorP <- function(input_df, code_type_col,original_d_values,original_p_values){
  input_df$Updated_Code_Type <- NA #create new col
  #For diagnoise:
  diag_indexes <- which(input_df[,code_type_col] %in% original_d_values)
  input_df[diag_indexes,"Updated_Code_Type"] <- "Diagnostic"
  #For Procedure:
  proc_indexes <- which(input_df[,code_type_col] %in% original_p_values)
  input_df[proc_indexes,"Updated_Code_Type"] <- "Procedure"
  return(input_df)
}

#'@Note: It is possible that one code mapped into two type or category, use $$$$ to cacatenate
grouping_DandP_func <- function(analayis_codes,code_type,chuback_grps,ritzwoller_grps,HCUP_grps){
  # analayis_codes <- unique_all_diag_df[,"unique_Diag_codes"]
  # code_type <- "Diagnostic"
  # chuback_grps <- chuback_group_df
  # ritzwoller_grps <- Ritzwoller_group_df
  # HCUP_grps <- HCUP_comb
  # 
  
  group_df <- as.data.frame(matrix(NA, nrow = length(analayis_codes) , ncol = 5))
  colnames(group_df) <- c("Code","Chubak_type", "Chubak_catogory", "Ritzwoller_catogory", "CCS_catogory")
  for (i in 1:length(analayis_codes)){
    if (i %% 1000 == 0){
      print(i)
    }
    curr_code <- analayis_codes[i]
    group_df[i,"Code"] <- curr_code
    
    
    curr_chubak <- chuback_grps[which(chuback_grps[,"Code"] == curr_code & 
                                        chuback_grps[,"Updated_Code_Type"] == code_type),]
    curr_Ritzwoller <- ritzwoller_grps[which(ritzwoller_grps[,"Code"] == curr_code &
                                               ritzwoller_grps[,"Updated_Code_Type"] ==  code_type),]
    curr_CCS <- HCUP_grps[which(HCUP_grps[,"Code"] == curr_code &
                                  HCUP_grps[,"Updated_Code_Type"] %in% code_type),]
    
    if (nrow(curr_chubak) > 0){
      group_df[i,"Chubak_type"] <- paste0(unique(curr_chubak[,"Type"]),collapse = "$$$$")
      group_df[i,"Chubak_catogory"] <- paste0(unique(curr_chubak[,"Category"]),collapse = "$$$$") 
    }
    
    if (nrow(curr_Ritzwoller) > 0){
      group_df[i,"Ritzwoller_catogory"] <- paste0(unique(curr_Ritzwoller[,"Category"]),collapse = "$$$$")
    }
    if (nrow(curr_CCS) > 0){
      group_df[i,"CCS_catogory"] <- paste0(unique(curr_CCS[,"CCS.CATEGORY"]),collapse = "$$$$")
    }
    
  }
  
  return(group_df)
}

#return the codes that grouped into multiple types
check_code_Multiple_grps <- function(input_df, grp_type){
  #input_df <- grouped_df_diag
  #grp_type <- "Chubak_type"
  
  all_splited_type <-  strsplit(input_df[,grp_type],split = "$$$$",fixed = T)
  n_cat <- NA
  for (i in 1:length(all_splited_type)){
    n_cat[i] <- length(all_splited_type[[i]])
  }
  multi_grp_idxes <- which(n_cat>1)
  multi_grps_codes <- input_df[multi_grp_idxes,"Code"]
  return(multi_grps_codes)
}

###################################################################################################


#Get code per day
get_code_func <-function(curr_perday_df,record_date_col,typeofcode_cols,curr_start,curr_end){
  # record_date_col <- "claims_date"
  # curr_start <- curr_mon_start
  # curr_end <-curr_mon_end
  #typeofcode_cols <- "Proc_Codes"    # "Proc_Codes","Drug_Codes"
  
  curr_interval_index <- which(curr_perday_df[,record_date_col] >= curr_start &
                                 curr_perday_df[,record_date_col]  < curr_end  )
  
  if (length(curr_interval_index) > 0){ #if any claims in the time window
    curr_interval_clamins <- curr_perday_df[curr_interval_index,]
    curr_codes <- as.character(unlist(curr_interval_clamins[,typeofcode_cols]))
    if(length(curr_codes) >0){ #if any codes
      curr_codes <- remove_NA_func(curr_codes)
      curr_codes <- unlist(strsplit(curr_codes,split = "$$$$",fixed = T))
    }else{
      curr_codes <- NA
    }
    
  }else{
    curr_codes <- NA
  }
  return(curr_codes)
}


create_perMon_count_table <- function(curr_id,n_valid_month,curr_enroll_months,curr_perday_df){
  
  #Diag table
  code_to_analysis_diag <- paste0("Diag_",as.vector(unique_Diag_codes[,"unique_Diag_codes"]))
  code_to_analysis_proc <- paste0("Proc_",as.vector(unique_Proc_codes[,"unique_Proc_codes"]))
  code_to_analysis_drug <- paste0("Drug_",as.vector(unique_Drug_codes[,"unique_Drug_codes"]))
  
  curr_pt_month_table_diag <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_diag) + 2))
  colnames(curr_pt_month_table_diag) <- c("ID","Month_Start_Date",code_to_analysis_diag)
  curr_pt_month_table_diag$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  curr_pt_month_table_proc <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_proc) + 2))
  colnames(curr_pt_month_table_proc) <- c("ID","Month_Start_Date",code_to_analysis_proc)
  curr_pt_month_table_proc$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  curr_pt_month_table_drug <- as.data.frame(matrix(0, nrow = n_valid_month[i],ncol = length(code_to_analysis_drug) + 2))
  colnames(curr_pt_month_table_drug) <- c("ID","Month_Start_Date",code_to_analysis_drug)
  curr_pt_month_table_drug$ID <- paste0("OriginalID_",curr_id,"_Mon",seq(1,n_valid_month[i]))
  
  for (j in 1:length(curr_enroll_months)){
    curr_mon_start <- ymd(curr_enroll_months[j]) 
    curr_mon_end <- curr_mon_start + months(1)
    
    #getunique codes in this interval
    curr_code_list_diag <- get_code_func(curr_perday_df,'claims_date',"Diag_Codes",curr_mon_start,curr_mon_end)
    curr_code_list_proc <- get_code_func(curr_perday_df,'claims_date',"Proc_Codes",curr_mon_start,curr_mon_end)
    curr_code_list_drug <- get_code_func(curr_perday_df,'claims_date',"Drug_Codes",curr_mon_start,curr_mon_end)
    
    
    #count each code in interval
    curr_tb <- table(curr_code_list_diag)
    curr_col_indexes <- which(colnames(curr_pt_month_table_diag) %in% paste0("Diag_",names(curr_tb)))
    curr_pt_month_table_diag[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    #count each code in interval
    curr_tb <- table(curr_code_list_proc)
    curr_col_indexes <- which(colnames(curr_pt_month_table_proc) %in% paste0("Proc_",names(curr_tb)))
    curr_pt_month_table_proc[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    #count each code in interval
    curr_tb <- table(curr_code_list_drug)
    curr_col_indexes <- which(colnames(curr_pt_month_table_drug) %in% paste0("Drug_",names(curr_tb)))
    curr_pt_month_table_drug[j,curr_col_indexes] <- as.numeric(curr_tb)
    
    
    curr_pt_month_table_diag[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    curr_pt_month_table_proc[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    curr_pt_month_table_drug[j,"Month_Start_Date"] <- as.character(curr_mon_start)
    
  }
  
  return(list(curr_pt_month_table_diag,curr_pt_month_table_proc,curr_pt_month_table_drug))
}

