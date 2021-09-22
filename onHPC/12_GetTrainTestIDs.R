source("Recapse_Ultility.R")
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

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "9_FinalIDs_And_UpdatedPtsChar/")
outdir           <- paste0(proj_dir, "12_TrainTestIDs/")

################################################################################ 
#1. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
Final_ID_df <- read.xlsx(paste0(data_dir1,"9_Final_ID1_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
Final_ID    <- unique(Final_ID_df$study_id)

pts_level_char_df <- read.xlsx(paste0(data_dir1,"9_PtsCharForFinalID_WithPossibleMonthsHasNoCodes.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% Final_ID),] #only keep char for final ID
print("Original non-SBCE vs SBCE : ")
table(pts_level_char_df$SBCE) #16917  1322

################################################################################ 
#2. Get SBCE and non-SBCE IDs
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids)) #13: 1

######################################################################################################## 
#3. make sure no overlapping in original Ids in train and test
#Test: 20% of the original IDs
#Train: 80% of the orginal IDs
#Validation sets will be auto generated when doing CV
########################################################################################################
total_n <- length(Final_ID)

test_ID  <- sample(Final_ID,0.2*total_n)
table(pts_level_char_df[which(pts_level_char_df$study_id %in% test_ID),"SBCE"])

#1.Testing : 
set.seed(123)
n <- 100
test_ID_SBCE   <- sample(sbce_pt_Ids,n)
test_ID_noSBCE <- sample(nosbce_pt_Ids,n*original_noSBCE_toSBCEratio)
test_IDs <- c(test_ID_SBCE,test_ID_noSBCE)
print(paste0("# Test Original IDs: ", length(test_IDs)))
#remove test ID from 
remaining_ID <- Final_ID[which(!Final_ID %in% test_IDs)]

#1. Training 80% of the remaining_ID
training_ID <- sample(remaining_ID,length(remaining_ID)*0.8)
print(paste0("# Train Original IDs: ", length(training_ID)))

#2. validation 20% of the remaining_ID
validation_ID <- remaining_ID[which(!remaining_ID %in% training_ID)]
print(paste0("# Validation Original IDs: ", length(validation_ID)))


####Get data
train_data <- model_data[which(model_data$study_id %in% training_ID),]
print("Training:")
table(train_data$y_PRE_OR_POST_2ndEvent) 
#Down sampling
upsample_flag <- 0
seed_num <- 123
train_data <- Data_Sampling_Func(upsample_flag,train_data,"y_PRE_OR_POST_2ndEvent",seed_num)
print("Down Sampled:")
table(train_data$y_PRE_OR_POST_2ndEvent) 

#Create xgb input
train_label <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])-1
train_data_part<-train_data[,!(names(train_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

validation_data <- model_data[which(model_data$study_id %in% validation_ID),]
print("Validation:")
table(validation_data$y_PRE_OR_POST_2ndEvent) 
validation_label <- as.numeric(validation_data[,"y_PRE_OR_POST_2ndEvent"])
validation_data_part<-validation_data[,!(names(validation_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dvalidation <- xgb.DMatrix(data = as.matrix(validation_data_part), label = validation_label)

test_data <- model_data[which(model_data$study_id %in% test_IDs),]
print("Test:")
table(test_data$y_PRE_OR_POST_2ndEvent) 
test_label <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part<-test_data[,!(names(test_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)
