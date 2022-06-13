source("Recapse_Ultility.R")
exclusion_func <- function(in_data){
  exclusion1_indxes <- which(in_data[,"HasEnoughMonths_InWindow"] ==0 | 
                               is.na(in_data[,"HasEnoughMonths_InWindow"])==T) #999
  exclusion2_indxes <- which(in_data[,"Stage"]  %in% c(0,4) |
                               is.na(in_data[,"Stage"])==T) #3491
  exclusion3_indxes <- which(in_data[,"Comb_SEERSummStg"]  %in% c(0,7,9) |
                               is.na(in_data[,"Comb_SEERSummStg"])==T) #3382
  exclusion4_indxes <- which(in_data[,"Diagnosis_Year"]<2004 | in_data[,"Diagnosis_Year"]>2015| 
                               is.na(in_data[,"Diagnosis_Year"])==T) #7393
  all_exc_indexes <- unique(c(exclusion1_indxes,exclusion2_indxes,exclusion3_indxes,exclusion4_indxes))
  
  updated_in_data <- in_data[-all_exc_indexes,]
  return(updated_in_data)
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
data_dir1  <- paste0(proj_dir, "8_Characteristics2/Patient_Level/")
outdir   <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")



################################################################################ 
#1. Load pts level char
################################################################################ 
patient_level_char_df1 <- read.xlsx(paste0(data_dir1,"8_PatientLevel_char_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
#patient_level_char_df2 <- read.xlsx(paste0(data_dir1,"8_PatientLevel_char_WithEveryMonthsHasCodes.xlsx"),sheet = 1)


################################################################################ 
#3.Analysis ID
################################################################################ 
analysis_ID1 <- unique(patient_level_char_df1$study_id)
#analysis_ID2 <- unique(patient_level_char_df2$study_id)

################################################################################ 
#Exclusion 1: Has_ValidClaims_inRange == 0 
#Exclusion 2: Stage 0 (0-2), Stage IV [70-80) ,Unknown (88,99) (Stage(BestStageGrp) )
#Exclusion 3 : non- local or regional stage for 1st priamry bc (SEERSummStg2000 stages  != 1,2,3,4,5) #In-situ 0; Localized 1; Regional 2-5; Distant 7
#Exclusion 4: diagnoise of first primary BC not in 2004 to 2015
################################################################################ 
#1.For enrollment moths record that allow no codes in the months
patient_level_char_df1 <- exclusion_func(patient_level_char_df1)
#patient_level_char_df2 <- exclusion_func(patient_level_char_df2)

#Check
write.xlsx(patient_level_char_df1,paste0(outdir,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx")) #18239
#write.xlsx(patient_level_char_df2,paste0(outdir,"9_PtsCharForFinalID_WithEveryMonthsHasCodes.xlsx")) #17468


final_IDs1 <- as.data.frame(patient_level_char_df1[,"study_id"])
colnames(final_IDs1) <- "study_id"
#final_IDs2 <- as.data.frame(patient_level_char_df2[,"study_id"])
#colnames(final_IDs2) <- "study_id"

write.xlsx(final_IDs1,paste0(outdir,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx")) #18239
#write.xlsx(final_IDs2,paste0(outdir,"9_Final_ID2_WithEveryMonthsHasCodes.xlsx")) #17468

