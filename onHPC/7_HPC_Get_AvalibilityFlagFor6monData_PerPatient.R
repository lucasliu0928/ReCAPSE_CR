source("Recapse_Ultility.R")

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

###########################################################################################################################
#This script get exclusion flag for patients does not qualify the following: 
#1).  If SBCE, and 2nd event is not NA,  (e.g, 1st primary -> 2nd event -> death or other diagnise)
#     3 months of claims before or after the SBCE    (have claims at least 3 months  before 2nd event or after)
#2):  if SBCE, but 2nd event is NA: (e.g, 1st primary then death)
#     # 6 month data
#2).  or any 6 months of claims for non-SBCE patients (At least 6 month data avaiable)
###########################################################################################################################
#onHPC
data_dir  <- "/recapse/intermediate_data/6_CleanClaims_InValidMonth/"
outdir    <- "/recapse/intermediate_data/6_CleanClaims_InValidMonth/"

# #local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_CleanClaims_InValidMonth/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/6_CleanClaims_InValidMonth/"



#########################################################################################################
#1.Load pateint event type and date data
#########################################################################################################
All_event_df <- read.xlsx(paste0(outdir,"4_updated_All_event_df.xlsx"),sheet = 1)


############################################################
#PerMonth files
############################################################
perMonth_files <- list.files(data_dir)


############################################################
#Get filter flag
############################################################
FilterFlag_PerMonthData_df <- as.data.frame(matrix(NA, nrow = length(perMonth_files), ncol =3))
colnames(FilterFlag_PerMonthData_df) <- c("study_id","SBCE","Has_ValidClaims_inRange")

for (i in  1: length(perMonth_files)) {
  if (i %% 1000 == 0){
    print(i)
  }
  curr_file <- perMonth_files[i]
  curr_id <- as.numeric(gsub("_perMonthData_inValidMonth.xlsx|ID","",curr_file))
  FilterFlag_PerMonthData_df[i,"study_id"] <- curr_id
  
  #per month df
  curr_perMonth_df <- read.xlsx(paste0(data_dir,curr_file),sheet = 1)
  
  #Check if per month df has all NAs
  allNA_flag <- all(is.na(curr_perMonth_df[,c("Diag_Codes","Proc_Codes" ,"Drug_Codes")]))
  
  #curr event time df
  curr_event_df <- All_event_df[which(All_event_df$study_id == curr_id),]

  if (nrow(curr_event_df) > 0 & allNA_flag == F){ #only process if pts has event time info and not all NA per month data
        #SBCE flag and 2nd event date
        curr_SBCE_flag <- curr_event_df$SBCE
        curr_2ndevent_date <- mdy(curr_event_df$Date_2nd_Event)
        FilterFlag_PerMonthData_df[i,"SBCE"] <- curr_SBCE_flag
        
        #per month cliams
        claims_end <- max(ymd(curr_perMonth_df[,"Month_Start"]))
        claims_start <- min(ymd(curr_perMonth_df[,"Month_Start"]))
        
        #Check if SBCE or not
        if (curr_SBCE_flag == 0){ #if no SBCE, make sure has at least 6 month claims available 
              curr_claims_duration <- as.numeric(difftime(claims_end,claims_start,units = "days"))
              if (curr_claims_duration >= 30*6){
                FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 1
              }else{
                FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 0
              }
        }else if (curr_SBCE_flag == 1 & is.na(curr_2ndevent_date) == F) { #if SBCE, and has 2nd event date,
              curr_2ndevent_date_minus3mon <-  curr_2ndevent_date - days(3*30)
              curr_2ndevent_date_plus3mon <-  curr_2ndevent_date + days(3*30)
              #check if overlapp between +- 3 month of  2nd event, and claims data
              if ((claims_start <= curr_2ndevent_date_plus3mon) & (claims_end >= curr_2ndevent_date_minus3mon)){
                FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 1
              }else{
                FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 0
                
              }
        }else if (curr_SBCE_flag == 1 & is.na(curr_2ndevent_date) == T){ # if SBCE, no 2nd event date, 
          curr_claims_duration <- as.numeric(difftime(claims_end,claims_start,units = "days"))
          if (curr_claims_duration >= 30*6){
            FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 1
          }else{
            FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- 0
          }
        }

  }else{
    FilterFlag_PerMonthData_df[i,"SBCE"] <- NA
    FilterFlag_PerMonthData_df[i,"Has_ValidClaims_inRange"] <- NA
    
  }
  
}


write.xlsx(FilterFlag_PerMonthData_df,paste0(outdir, "7_PerMonthData_FilterFlag_df.xlsx"))

