source("Recapse_Ultility.R")
compute_mean_sd_func <- function(data_col,round_digit){
  mean_val <- round(mean(data_col),round_digit)
  sd_val <-  round(sd(data_col),round_digit)
  comb_val <- paste0(mean_val," \u00b1 ",sd_val)
  
  return(comb_val)
}
compute_n_perc_func <- function(data_col,round_digit){
  # data_col <- curr_values
  
  total_n <- length(data_col)
  count_tb <- table(data_col)
  count_tb_perc <- round(count_tb/total_n*100,round_digit)
  count_cato_names <- names(count_tb)
  count_final <-""
  count_final <- paste0(count_cato_names,": ", count_tb, " (",count_tb_perc,")", collapse = "\n")
  count_final <- paste0(count_final,"\n Total(NA excluded):",total_n)
  return(count_final)
}


compute_median_p25andp75_func <- function(data_col,round_digit){
  med_val <- round(median(data_col),round_digit)
  quant_res <- quantile(data_col,c(0.25,0.75))
  p25 <- round(quant_res[1],round_digit)
  p75 <- round(quant_res[2],round_digit)
  comb_val <- paste0(med_val," [",p25,"-",p75,"]")
  
  return(comb_val)
}


compute_stats_func <- function(input_df,cohort_name,ordered_parameters,n_perc_variables_list){
  Final_table <- as.data.frame(matrix(NA, nrow = length(ordered_parameters), ncol = 3))
  colnames(Final_table) <- c("Var","Stats","Missingness")
  Final_table$Var <- ordered_parameters
  
  for (i in 1:length(ordered_parameters)){
    curr_f <- ordered_parameters[i]
    
    #get index column of current feature
    curr_colindex <- which(colnames(input_df) == curr_f)
    
    
    if (length(curr_colindex) == 0){ #if feature is not in curret data input
      Final_table[i,2] <- NA
    }else{
      #Get current values
      curr_values <- input_df[,curr_f]
      
      #report and remove NAs
      na_indexes <- which(is.na(curr_values) == T)
      n_NA <-  length(na_indexes)
      prec_NA <- round((n_NA/length(curr_values)*100),2)
      Final_table[i,3] <- paste0(n_NA," (",prec_NA,"%)")
      
      if(n_NA > 0){
        curr_values <- curr_values[-na_indexes]
      }
     
      
      if (curr_f %in% n_perc_variables_list){ #compte n perc
        Final_table[i,2] <- compute_n_perc_func(curr_values,2)
      }else{
        Final_table[i,2] <- compute_median_p25andp75_func(curr_values,2)
      }
      
    }
  }
  colnames(Final_table) <- paste0(cohort_name,"_",colnames(Final_table))
  
  return(Final_table)
}


################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")

outdir           <- paste0(proj_dir, "17_Discrip_Statistics/")

################################################################################ 
#1. Load pts level char for final IDs
################################################################################ 
Final_PTs_Char_df <- read.xlsx(paste0(data_dir1,"/9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
final_Ids <- Final_PTs_Char_df$study_id

#Recode Type 2 event:
#1. As long as the string has "1Recur", it counted as "first primary recurrence", no matter if it has same date with others
recode_idxes1 <- which(grepl("1Recur",Final_PTs_Char_df[,"Type_2nd_Event"]) == T)
Final_PTs_Char_df[recode_idxes1,"Type_2nd_Event"] <- "1Recur"

#Recode Race
Final_PTs_Char_df[-which(Final_PTs_Char_df[,"Race"] %in% c(1,2)),"Race"] <- "Others"

################################################################################ 
#Get SBCE and non-SBCE pts char df
################################################################################ 
SBCE_PTs_Char_df   <- Final_PTs_Char_df[which(Final_PTs_Char_df[,"SBCE"] == 1),]
noSBCE_PTs_Char_df <- Final_PTs_Char_df[which(Final_PTs_Char_df[,"SBCE"] == 0),]

################################################################################ 
#3. Report some stats
#'@NOTE: Need to add DAJCC_M,DAJCC_N,DAJCC_T and num_nobc later
#'@Note: These table does not show all features for training (e.g, age at each month, monthes since diagnosis)
################################################################################ 
all_variables <- c("SBCE","Medicaid_OR_Medicare","reg_age_at_dx","Diagnosis_Year",
                   "most_recent_enrollment_year",
                   "Num_Month_before_2ndEvent","Num_Month_AfterOrEqual_2ndEvent",
                   "Num_Enrolled_Prediction_Months",
                   "Type_2nd_Event",
                   "Race" , "Site" , "Stage","Grade",
                   "Laterality" ,"er_stat","pr_stat",	"her2_stat",	
                    "reg_nodes_exam", "reg_nodes_pos", "surg_prim_site", 
                    "cs_tum_size", "cs_tum_ext", 
                     "cs_tum_nodes", "regional")
n_perc_variables <- c("SBCE", "Medicaid_OR_Medicare", "Race","Site","Stage","Grade","Laterality",
                      "er_stat","pr_stat","her2_stat","surg_prim_site","regional","most_recent_enrollment_year","Diagnosis_Year","Type_2nd_Event")

table_all <- compute_stats_func(Final_PTs_Char_df,"ALL",all_variables,n_perc_variables)
table_sbce <- compute_stats_func(SBCE_PTs_Char_df,"SBCE",all_variables,n_perc_variables)
table_nonsbce <- compute_stats_func(noSBCE_PTs_Char_df,"non-SBCE",all_variables,n_perc_variables)

table_comb <- cbind(table_all,table_sbce,table_nonsbce)
write.csv(table_comb, paste0(outdir,"discrip_table.csv"))

#Histgram
plot_hist <- function(in_data, x_name, grp_name, xbreaks,x_label){
  p <- ggplot(in_data, aes_string(x=x_name, color=grp_name)) +
    geom_histogram(fill="white", alpha=0.8, position="identity",binwidth = 0.5) +
    scale_x_continuous(breaks= xbreaks) +
    scale_color_discrete(name = "",labels = c("non-recurrent", "recurrent")) +
    theme(legend.position="top") + 
    labs(x = x_label, y = "Count") +
    theme(text = element_text(size=20))
  return(p)
}

Final_PTs_Char_df$SBCE <- as.character(Final_PTs_Char_df$SBCE)
p <- plot_hist(Final_PTs_Char_df,"Diagnosis_Year","SBCE",seq(2004, 2015, 1),"Diagnosis Year")
png(paste0(outdir,"diagnosis_yr.png"), width = 1000, height = 800, res = 120)
print(p)
dev.off()

p <- plot_hist(Final_PTs_Char_df,"most_recent_enrollment_year","SBCE",seq(2005, 2020, 1),"Most Recent Enrollment Year")
png(paste0(outdir,"most_recent_enroll_yr.png"), width = 1200, height = 800, res = 120)
print(p)
dev.off()

