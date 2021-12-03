data_dir1 <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/15_XGB_Input/"
check_Df <- read.csv(paste0(data_dir1,"TrainSampleIDsAndLabels1.csv"),stringsAsFactors = F)

pre_idnxes  <- which(check_Df$y_PRE_OR_POST_2ndEvent==0)
post_idnxes <- which(check_Df$y_PRE_OR_POST_2ndEvent==1)

pre_pt_IDs <- unique(check_Df$study_id[pre_idnxes])
post_pt_IDs <- unique(check_Df$study_id[post_idnxes])


length(pre_pt_IDs)
length(post_pt_IDs)
