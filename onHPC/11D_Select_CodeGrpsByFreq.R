source("Recapse_Ultility.R")

#onHPC
data_dir <- "/recapse/intermediate_data/11C_CodeGroup_Freq_tb/"
discrip_dir <- "/recapse/intermediate_data/11C_CodeGroup_Freq_tb/3B_Codes_And_Groups"
outdir <- "/recapse/intermediate_data/"

#local
data_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/11C_CodeGroup_Freq_tb/"
discrip_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/3B_Codes_And_Groups/"
outdir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

################################################################################ 
#1. Load all code grp freq tb
################################################################################ 
freq_tb_files <- list.files(data_dir,full.names = T)
all_freq_tb <- do.call(rbind,lapply(freq_tb_files, read.csv))
all_freq_tb$Perc_SamplesHASCode <- round(all_freq_tb$Perc_SamplesHASCode,2)

################################################################################ 
#2. Load code group discription
################################################################################ 
Diag_discrip_df <- read.csv(paste0(discrip_dir,"Unique_Diag_And_Groups.csv"),stringsAsFactors = F)
Proc_discrip_df <- read.csv(paste0(discrip_dir,"Unique_Proc_And_Groups.csv"),stringsAsFactors = F)
Drug_discrip_df <- read.csv(paste0(discrip_dir,"Unique_Drug_And_Groups.csv"),stringsAsFactors = F)

################################################################################ 
#3. Add CCS code group discription, other groups has name so no discription are added
################################################################################ 
all_freq_tb$Group_Discription <- NA
for (i in 1:nrow(all_freq_tb)){
  curr_code_grp <- as.character(all_freq_tb$Code_Group[i])
  
  if (grepl("CCS_D_",curr_code_grp) == T){ 
     all_freq_tb[i,"Group_Discription"] <- get_grp_discription_func(curr_code_grp,"CCS_D_",Diag_discrip_df,"CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION")
  }else if (grepl("CCS_P_",curr_code_grp) == T){ 
    all_freq_tb[i,"Group_Discription"] <- get_grp_discription_func(curr_code_grp,"CCS_P_",Proc_discrip_df,"CCS_CATEGORY","CCS_CATEGORY_DESCRIPTION")
  }

}

write.csv(all_freq_tb,paste0(outdir,"11D_AllCodeGroups_freq_tb.csv"),row.names = F)

################################################################################ 
#4.Select Code groups > threhold 0.05
################################################################################ 
selected_Features_df <- all_freq_tb[which(all_freq_tb$Perc_SamplesHASCode >= 0.05),]
write.csv(selected_Features_df,paste0(outdir,"11D_Selcted_CodeGroups_freqGT005_tb.csv"),row.names = F)
