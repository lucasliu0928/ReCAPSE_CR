source("Recapse_Ultility.R")
library("SHAPforxgboost")
library("ggplot2")
library("xgboost")
library("data.table")
library("here")

#Change feature name to the actual group name
change_feautre_name <- function(indata,ccs_disp_df){
  # indata <- shap_input_df
  # ccs_disp_df <- ccs_all_df
  # 
  #Change all VAL_2ND to GPI2ND
  colnames(indata) <- gsub("VAL_2ND","GPI2ND",colnames(indata))
  
  #Change all cumul_ratio/time_since/time_until
  colnames(indata) <- gsub("cumul_ratio_","(Cumulative Ratio)",colnames(indata))
  colnames(indata) <- gsub("time_since_", "(Time Since)",colnames(indata))
  colnames(indata) <- gsub("time_until_", "(Time Until)",colnames(indata))
  
  #Find the discription of CCS codes
  updated_names <- NA
  for (i in 1:length(colnames(indata))){
    cur_f <-  colnames(indata)[i]

    if (grepl("(",cur_f,fixed = T) == T & grepl("GPI|DM3", cur_f) == F){
        cur_transf_type <- paste0(strsplit(cur_f, ')')[[1]][1],")")
        cur_f <- strsplit(cur_f, ')')[[1]][2]
        curr_code_type <- paste0(strsplit(cur_f,"_")[[1]][1:2],collapse = "_")
        
        if (grepl("CCS",curr_code_type)== T){
          curr_disp <- ccs_disp_df[which(ccs_disp_df[,"Code_Grp"] == cur_f),"Grp_Discrip"]
        }else{
          curr_disp <- NA
        }
        cur_new_name <- paste0(curr_disp,"[",curr_code_type,"]", cur_transf_type)
    }else if (grepl("GPI|DM3", cur_f) == T){
      cur_transf_type <- paste0(strsplit(cur_f, ')')[[1]][1],")")
      curr_code_type <- strsplit(strsplit(cur_f,"_")[[1]][1],")")[[1]][2]
      curr_disp <- strsplit(cur_f,"_")[[1]][2]
      cur_new_name <- paste0(curr_disp,"[",curr_code_type,"]", cur_transf_type)
      
    }else{
      cur_new_name <- cur_f
    }
    updated_names[i] <- cur_new_name
  }

  colnames(indata) <- updated_names
  
  return(indata)
}

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


################################################################################ 
#User input
#'@NOTE: 
#'For CCSandVAL2nd:
#'ds_index = 3 for SBCE
#'ds_index = 5 for SBCE_Excluded_DeathPts
#'ds_index = 1 for SBCE_Excluded_DeathLabel


#'For CCSandDM3SPE:
#'ds_index = 4 for SBCE
#'ds_index = 6  for SBCE_Excluded_DeathPts
################################################################################ 
feature_set_name  <- "CCSandVAL2nd"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE_Excluded_DeathPts" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"
ds_index          <- 5

if ((SBCE_ID_Folder == "SBCE") | (SBCE_ID_Folder == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent" 
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath" 
}


################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1 <- paste0(proj_dir,"15_XGB_Input/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/")
data_dir2 <- paste0(proj_dir,"16B_Trained_ImportantFeatureModel/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/")
data_dir3 <- paste0(proj_dir,"/10G_Counts_UniqueGrp_PtsLevel/WithPossibleMonthsHasNoCodes/")

outdir <- paste0(proj_dir, "17_Performance/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/")

#Create directory
ds_out <- paste0("DS",ds_index,"/SHAP/")
dir.create(file.path(outdir, ds_out), recursive = TRUE)

################################################################################
#Load feature CCS group discrption.names, drug disription is already in the feature name
################################################################################
ccs_diag <- read.xlsx(paste0(data_dir3 , "Count_CCS_Diag_Unique_Grps.xlsx"))
ccs_proc <- read.xlsx(paste0(data_dir3 , "Count_CCS_proc_Unique_Grps.xlsx"))
ccs_all_df <- rbind(ccs_diag[,c(1,6)],ccs_proc[,c(1,6)])
ccs_all_df[,"Grp_Discrip"] <- gsub("'","",ccs_all_df[,"Grp_Discrip"])
ccs_all_df[,"Grp_Discrip"] <- trimws(ccs_all_df[,"Grp_Discrip"])

################################################################################
#Load train and test
################################################################################
load(file = paste0(data_dir1, "Train/train_nonobv_DS", ds_index, ".rda"))
load(file = paste0(data_dir1, "Test/test_pos_data.rda"))
load(file = paste0(data_dir1, "Test/test_neg_data.rda"))
load(file = paste0(data_dir1, "Test/test_nonobv_data.rda"))

################################################################################
#Load important features for each DS and optimal model
################################################################################
important_f_df <- read.csv(paste0(data_dir2,"importance_matrix_DS",ds_index,"_topf.csv"), stringsAsFactors = F)
top_fs <- important_f_df[,"Feature"]
#Load Optimal model
mod_optimal <- xgb.load(paste0(data_dir2,"train_DS",ds_index,"_topf",".model"))


################################################################################
#Create xgb input
################################################################################
train_data <- train_nonobv_ds_df
test_data <- rbind(test_pos_df,test_neg_df,test_nonobv_df)
  
train_label      <- as.numeric(train_data[,label_col])
train_data_part  <- train_data[,top_fs] #top features
dtrain           <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

test_label       <- as.numeric(test_data[,label_col])
test_data_part   <- test_data[,top_fs] #top features
dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)

test_data_neg <- test_data[which(test_data[,"sample_id"] %in% test_neg_df$sample_id),]
test_data_pos <- test_data[which(test_data[,"sample_id"] %in% test_pos_df$sample_id),]
test_data_nonobv <- test_data[which(test_data[,"sample_id"] %in% test_nonobv_df$sample_id),]


################################################################################
#SHAP for Train
################################################################################
shap_input_df <- as.matrix(train_data_part)
shap_input_df <- change_feautre_name(shap_input_df,ccs_all_df)

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score
#Plot
p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)

outfile_name <- paste0(outdir, ds_out,"SHAP_train.png")
png(outfile_name,width = 1000,height = 500,res=100)
print(p)
dev.off()

################################################################################
#SHAP for Test
################################################################################
shap_input_df <- as.matrix(test_data_part) ##make sure the feature is the same order as the ones in the model
shap_input_df <- change_feautre_name(shap_input_df,ccs_all_df)

# To return the SHAP values and ranked features by mean|SHAP|
shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
# The ranked features by mean |SHAP|
shap_values$mean_shap_score

#Plot
p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)
outfile_name <- paste0(outdir, ds_out,"SHAP_test.png")
png(outfile_name,width = 1000,height = 500,res=100)
print(p)
dev.off()


################################################################################
#SHAP for Test obv neg,obv pos, and no-obv, seperately
################################################################################
test_data_list <- list(test_data_neg, test_data_pos, test_data_nonobv)
test_data_type <- c("obvNEG","obvPOS","nonobv")
for (i in 1:3){
  curr_data        <- test_data_list[[i]]
  curr_test_data_type   <- test_data_type[i]
  
  test_label       <- as.numeric(curr_data[,label_col])
  test_data_part   <- curr_data[,top_fs] #top 50 features
  dtest            <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)
  
  shap_input_df <- as.matrix(test_data_part) ##make sure the feature is the same order as the ones in the model
  
  # To return the SHAP values and ranked features by mean|SHAP|
  shap_values <- shap.values(xgb_model = mod_optimal, X_train = shap_input_df)
  # The ranked features by mean |SHAP|
  shap_values$mean_shap_score
  #Plot
  p <- shap.plot.summary.wrap1(model = mod_optimal, X = shap_input_df,top_n = 10)
  outfile_name <- paste0(outdir, ds_out,"SHAP_test_",curr_test_data_type,".png")
  png(outfile_name,width = 1000,height = 500,res=100)
  print(p)
  dev.off()
}




