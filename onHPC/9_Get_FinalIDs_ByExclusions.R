library(openxlsx)
library(lubridate)
#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# #local
# data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
# outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


################################################################################ 
#1. Filter flag for per month data
#this excludes pts has no valid claims in range 
#1).  If SBCE, and 2nd event is not NA,  (e.g, 1st primary -> 2nd event -> death or other diagnise)
#     3 months of claims before or after the SBCE    (have claims at least 3 months  before 2nd event or after)
#2):  if SBCE, but 2nd event is NA: (e.g, 1st primary then death)
#     # 6 month data
#2).  or any 6 months of claims for non-SBCE patients (At least 6 month data avaiable)
################################################################################ 
PerMonthData_FilterFlag_df_df <- read.xlsx(paste0(data_dir,"7_PerMonthData_FilterFlag_df.xlsx"),sheet = 1)

################################################################################ 
#2. Load patient level charateristics
################################################################################ 
Patient_Char_df <- read.xlsx(paste0(data_dir,"8_PatientLevel_charecteristics.xlsx"),sheet = 1)

#########################################################################################################
#3. Load outcome/event type data
#########################################################################################################
updated_All_event_df <- read.xlsx(paste0(data_dir,"4_updated_All_event_df.xlsx"),sheet = 1)

################################################################################ 
#3.Analysis ID
################################################################################ 
analysis_ID <- unique(Reduce(intersect, list(updated_All_event_df$study_id,PerMonthData_FilterFlag_df_df$study_id, Patient_Char_df$study_id)))

################################################################################ 
#4.Get exclusion dataframe
################################################################################ 
exclusion_df <- as.data.frame(matrix(NA, nrow = length(analysis_ID), ncol = 4))
colnames(exclusion_df) <- c("study_id","SEERSummStg2000","BestStageGrp","Has_ValidClaims_inRange")

for (i in 1:length(analysis_ID)){
  if (i %% 1000 == 0){ print(i)}
  curr_id <- analysis_ID[i]
   
  #curr Patient_Char_df
  curr_Patient_Char_df <- Patient_Char_df[which(Patient_Char_df[,"study_id"] == curr_id),]
  
  #curr PerMonthData_FilterFlag_df_df
  curr_filterflag_df <- PerMonthData_FilterFlag_df_df[which(PerMonthData_FilterFlag_df_df[,"study_id"] == curr_id),]
  
  exclusion_df[i,"study_id"] <- curr_id
  exclusion_df[i,"SEERSummStg2000"] <- curr_Patient_Char_df[,"Comb_SEERSummStg"]
  exclusion_df[i,"BestStageGrp"] <- curr_Patient_Char_df[,"Stage"]
  exclusion_df[i,"Has_ValidClaims_inRange"] <- curr_filterflag_df[,"Has_ValidClaims_inRange"]

}

################################################################################ 
#Exclusion 1: Has_ValidClaims_inRange == 0 
#Exclusion 2 : non- local or regional stage for 1st priamry bc (SEERSummStg2000 stages  != 1,2,3,4,5) #In-situ 0; Localized 1; Regional 2-5; Distant 7
#Exclusion 3: Stage 0 (0-2), Stage IV [70-80) ,Unknown (88,99) (BestStageGrp) 
################################################################################ 
exclusion1_indxes <- which(exclusion_df$Has_ValidClaims_inRange ==0 | is.na(exclusion_df$Has_ValidClaims_inRange)==T) #584
exclusion2_indxes <- which((!exclusion_df$SEERSummStg2000 %in% c(1,2,3,4,5)) | is.na(exclusion_df$SEERSummStg2000)==T) #3491
exclusion3_indxes <- which(exclusion_df$BestStageGrp %in% c(0,1,2,seq(70,99,1)) | is.na(exclusion_df$BestStageGrp)==T) #2789

final_analysis_ID_df <- exclusion_df[-unique(c(exclusion1_indxes,exclusion2_indxes,exclusion3_indxes)),]
colnames(final_analysis_ID_df)[2:4] <- paste0("ExcCrit_",colnames(final_analysis_ID_df)[2:4]) 
write.xlsx(final_analysis_ID_df,paste0(outdir,"9_Final_Analysis_ID.xlsx")) #23378

table(final_analysis_ID_df$ExcCrit_BestStageGrp)
table(final_analysis_ID_df$ExcCrit_BestStageGrp)

