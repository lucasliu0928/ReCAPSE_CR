source("Recapse_Ultility.R")

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
pts_level_char_df <- read.xlsx(paste0(data_dir1,"/9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
final_Ids <- pts_level_char_df$study_id

################################################################################ 
#3. Report some stats
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
n_sbce1   <- length(sbce_pt_Ids)
n_sbce0   <- length(nosbce_pt_Ids)
n_sbce0/n_sbce1


#type of 2nd event
type_2ndevnet_tb <- as.data.frame(table(pts_level_char_df$Type_2nd_Event))

missingtable <- get_missing_rate_table(pts_level_char_df, colnames(pts_level_char_df))

write.csv(missingtable, paste0("/Users/lucasliu/Desktop/","missingtable.csv"))

table(pts_level_char_df$DAJCC_M)
table(pts_level_char_df$DAJCC_N)
table(pts_level_char_df$DAJCC_T)
