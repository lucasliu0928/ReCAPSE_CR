source("Recapse_Ultility.R")
#This scrip generate input for training and test samples
# A.	Test: all testing sample
#   a)	Non-obvious samples
#   b)	Obvious negative samples
#   c)	Obvious positive samples
# B.	Train: 
#   a)non-obvious samples
#     Option1: all non-obvious samples without sampling
#     Option2: Down sample 10 times
#   b)Obvious negative samples
#   c)Obvious positive samples

Data_Sampling_Func <- function(upsample_flag,train_data,label_col_name,seed_num,random_perc = 0.8){
  # upsample_flag <- 0
  # train_data <- train_data
  # label_col_name <- "y_PRE_OR_POST_2ndEvent"
  # seed_num <- 1
  
  #Get label col index
  label_col_index <- which(colnames(train_data) == label_col_name)
  
  #Sampling
  if(upsample_flag==1){ #upsampling
    set.seed(seed_num)
    up_train <- upSample(x = train_data[, -label_col_index],
                         y = as.factor(train_data[,label_col_name]), yname = label_col_name)  
    sampled_train_data <- up_train
    
  }else if(upsample_flag==0){ #downsample
    set.seed(seed_num)
    down_train <- downSample(x = train_data[, -label_col_index],
                             y = as.factor(train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
    
  }else if(upsample_flag==2){ #random sample 90% of orignal data
    set.seed(seed_num)
    sampled_indxes <- sample(nrow(train_data), nrow(train_data)*random_perc, replace = TRUE, prob = NULL)
    sampled_train_data <- train_data[sampled_indxes,]
  }else if (upsample_flag == 3){ #random sample then down sample
    set.seed(seed_num)
    sampled_indxes <- sample(nrow(train_data), nrow(train_data)*random_perc, replace = TRUE, prob = NULL)
    randomsampled_train_data <- train_data[sampled_indxes,]
    down_train <- downSample(x = randomsampled_train_data[, -label_col_index],
                             y = as.factor(randomsampled_train_data[,label_col_name]), yname = label_col_name)      
    sampled_train_data <- down_train
  }else{
    original_train <- train_data
    sampled_train_data <- original_train
  }
  
  return(sampled_train_data)
}

print_n_prepostsamples_func <- function(in_data, data_name){
  #in_data <- model_data
  tb      <- table(in_data[,"y_PRE_OR_POST_2ndEvent"])
  n_pre   <- as.numeric(tb[which(names(tb)==0)])
  n_post  <- as.numeric(tb[which(names(tb)==1)])
  
  print(paste0(data_name, "Pre:" , n_pre, " Post:",n_post, " Total:", nrow(in_data)))
}

get_SampleIDs_withDSLabels <-function(in_data,downsampled_sampleIDs,ds_index){
  model_data_IDandLabels <- in_data[,c("study_id","sample_id","y_PRE_OR_POST_2ndEvent")]
  
  #Add downsample train flag col
  model_data_IDandLabels$DownSampled_Train <- NA
  train_idxes2 <- which(model_data_IDandLabels[,"sample_id"] %in% downsampled_sampleIDs)
  model_data_IDandLabels[train_idxes2,"DownSampled_Train"] <- ds_index
  
  return(model_data_IDandLabels)
}



get_data_inCategory_func <- function(in_data, id_dir, id_file){
  id_df  <- read.csv(paste0(id_dir,id_file),stringsAsFactors = F)
  sample_IDs  <- id_df[,"sample_id"]
  sample_df <- in_data[which(in_data[,"sample_id"] %in% sample_IDs),]
  #Print num of pre and post samples 
  cohort_name <- gsub(".csv|_Samples","",id_file)
  print_n_prepostsamples_func(sample_df,cohort_name)
  return(sample_df)
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

SBCE_col    <- "SBCE_Excluded_DeathPts" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
feature_set_name <- "CCSandVAL2nd"
if ((SBCE_col == "SBCE") | (SBCE_col == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent"  
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath"   
}

#data dir
data_dir1  <- paste0(proj_dir, "11E_AllPTs_ModelReadyData/",feature_set_name,"/")
data_dir2  <- paste0(proj_dir, "12A_PCA_VarContri_Train/",feature_set_name,"/",SBCE_col,"/")
data_dir3  <- paste0(proj_dir,"12E_OBVandNONOBV_SamplesIDs/",feature_set_name,"/",SBCE_col,"/")


newout1 <- paste0("15_XGB_Input/",feature_set_name,"/",SBCE_col,"/Test/")
outdir   <- paste0(proj_dir, newout1)
dir.create(file.path(proj_dir, newout1), recursive = TRUE)

newout2 <- paste0("15_XGB_Input/",feature_set_name,"/",SBCE_col,"/Train/")
outdir   <- paste0(proj_dir, newout2)
dir.create(file.path(proj_dir, newout2), recursive = TRUE)

######################################################################################################## 
#1. Load and combine all patient model ready data
######################################################################################################## 
load(file = paste0(data_dir1, "All_PTS_ModelReadyData.rda"))

#remove the const feature (e.g: DM3_SPE_muscle.relaxant)
const_feature_file <- paste0(data_dir2,"ConstFeature_removed_ForPCAandtSNE.csv")
if (file.exists(const_feature_file)==TRUE){
  const_fs <- read.csv(const_feature_file,stringsAsFactors = F)
  model_data <- model_data[, -which(colnames(model_data) %in% const_fs[,2])]
}

################################################################################ 
#2. Get model ready test data
################################################################################ 
#A. samples data
test_neg_df <- get_data_inCategory_func(model_data, data_dir3,"ObviousNeg_Samples_Test.csv")
test_pos_df <- get_data_inCategory_func(model_data, data_dir3,"ObviousPos_Samples_Test.csv")
test_nonobv_df <- get_data_inCategory_func(model_data, data_dir3,"NON_Obvious_Samples_Test.csv")

#B. Output model ready binary data
save(test_neg_df, file=paste0(proj_dir, newout1, "test_neg_data.rda"))
save(test_pos_df, file=paste0(proj_dir, newout1, "test_pos_data.rda"))
save(test_nonobv_df, file=paste0(proj_dir, newout1, "test_nonobv_data.rda"))

################################################################################ 
#2.  Get model ready train data 
################################################################################ 
#A. samples data
train_neg_df <- get_data_inCategory_func(model_data, data_dir3,"ObviousNeg_Samples_Train.csv")
train_pos_df <- get_data_inCategory_func(model_data, data_dir3,"ObviousPos_Samples_Train.csv")
train_nonobv_df <- get_data_inCategory_func(model_data, data_dir3,"NON_Obvious_Samples_Train.csv")

save(train_neg_df, file=paste0(proj_dir, newout2, "train_neg_data.rda"))
save(train_pos_df, file=paste0(proj_dir, newout2, "train_pos_data.rda"))


#B. Output model ready binary data (non-obvs)
#a) without down sampling
train_nonobv_ds_df <- train_nonobv_df
save(train_nonobv_ds_df, file=paste0(proj_dir, newout2, "train_nonobv","_DS" ,"0",".rda"))

#b) down sampled non-obvs train  10 times
n_sampling <- 10
for (i in 1:n_sampling){
  seed_num <- 122 + i 
  
  #Down sampled 
  train_nonobv_ds_df <- Data_Sampling_Func(0,train_nonobv_df,"y_PRE_OR_POST_2ndEvent",seed_num)
  train_nonobv_ds_df$y_PRE_OR_POST_2ndEvent <- as.numeric(train_nonobv_ds_df$y_PRE_OR_POST_2ndEvent) -1

  #Print num of pre and post samples 
  print_n_prepostsamples_func(train_nonobv_ds_df,"Train: ")
  
  #Output model ready binary data
  save(train_nonobv_ds_df, file=paste0(proj_dir, newout2, "train_nonobv","_DS" ,i,".rda"))

}







