library(lubridate)

proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
month_data <- read.csv(paste0(data_dir, "diag_monthly_df.csv"),stringsAsFactors = F)
month_data <- month_data[,-5] #remove Na code columns
length(unique(month_data$ID)) #27607
nrow(month_data) #2095430
table(month_data$outcome) #0:1985309, 1:110121

####Load patinet char file
data_dir2 <- paste0(proj_dir,"/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/")
kcr_data <- read.csv(paste0(data_dir2, "uh3_kcrdata.csv"),stringsAsFactors = F)

###Load event data so that we can select the correpdoning row in kcr
event_data <- read.csv(paste0(data_dir, "updated_All_event_df.csv"),stringsAsFactors = F)



#####
for (i in 1:nrow(month_data)){
  i <- 1
  curr_id <- month_data[i,"ID"]
  curr_month <- month_data[i,"Month_Start"]
  
  
  curr_event <- event_data[which(event_data[,"ID"] == curr_id),]
  curr_1stevent_date <- curr_event[,"Date_1st_Event"]
  curr_1stevent_site <- unlist(strsplit(curr_event[,"Site_1st_Event"],split= "_"))[2]
  
  curr_kcr <- kcr_data[which(kcr_data[,"study_id"] == curr_id & 
                             kcr_data[,"PrimarySite"] ==  curr_1stevent_site &
                             kcr_data[,"Date_dx"] ==  curr_1stevent_date),]
  
  curr_dob <- curr_kcr[,"date_Birth"]
  curr_age <- as.numeric(difftime(ymd(curr_month),mdy(curr_dob), units = "days"))/365 
  
  curr_months_since_dx <- as.numeric(difftime(ymd(curr_month),mdy(curr_1stevent_date), units = "days"))
  
  curr_race <- curr_kcr[,"Race1"]
  curr_site <- curr_kcr[,"PrimarySite"]
  curr_grade <- curr_kcr[,"Grade"]
  curr_laterality <- curr_kcr[,"Laterality"]
  curr_er_stat <- curr_kcr[,"er_stat"]
  curr_pr_stat <- curr_kcr[,"pr_stat"]
  curr_her2_stat <- curr_kcr[,"her2_stat"]
  curr_surg_prim_site <- curr_kcr[,"RXSummSurgPrimSite"]
  
  curr_reg_nodes_exam <- curr_kcr[,"RegNodesExamined"]
  curr_reg_nodes_pos <- curr_kcr[,"RegNodesPositive"]
  curr_cs_tum_size <- curr_kcr[,"CSTumorSize"]
  curr_cs_tum_ext <- curr_kcr[,"CSTumorSizeExtEval"]

  #'@TODO: ?DAJCC_T, ?DAJCC_M, ?DAJCC_N, ?reg_age_at_dx,?cs_tum_nodes,  ?stage , 

}