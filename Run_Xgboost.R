proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
month_data <- read.csv(paste0(data_dir, "diag_monthly_df.csv"))
month_data <- month_data[,-5] #remove Na code columns
length(unique(month_data$ID)) #27607
nrow(month_data) #2095430
table(month_data$outcome) #0:1985309, 1:110121

nonrecurrent_pts_data <- month_data[which(month_data$outcome == 0),]
recurrent_pts_data <- month_data[which(month_data$outcome == 1),]

#Select the groups that match or exceed the threshold of the fraction of patients with at least one code in that group; 
#the nonrecurrent patient thresholds are 0.15, 0.15, and 0.05, 
#and the recurrent patient thresholds are 0.10, 0.10, and 0.01 for the diagnostic, procedure, and drug groups
perc_pts <- NA
analysis_data <- recurrent_pts_data
for (j in 4:length(analysis_data)){
   curr_col <- analysis_data[,j]
   n_pts <- length(which(curr_col >= 1)) #n of pts have at least one code in this group
   perc_pts[j] <- n_pts/nrow(analysis_data)
}

round(perc_pts,3)
ccol_indexes1 <- which(perc_pts > 0.05)

perc_pts <- NA
analysis_data <- nonrecurrent_pts_data
for (j in 4:length(analysis_data)){
  curr_col <- analysis_data[,j]
  n_pts <- length(which(curr_col >= 1)) #n of pts have at least one code in this group
  perc_pts[j] <- n_pts/nrow(analysis_data)
}

ccol_indexes2 <- which(perc_pts > 0.05)
comb_indexes <- unique(ccol_indexes1,ccol_indexes2)

comb_filtered_data <- month_data[,c(1,2,3,comb_indexes)]


##########START HERE##########
######################################################################################################## 
###############              Data preprocessing
###############     Note: Make sure label range: [0,num_class-1]
######################################################################################################## 
data_input <- comb_filtered_data
#split train and test
set.seed(123)   
library(caTools)
sample <- sample.split(data_input,SplitRatio = 0.8) # 0.8 for training
train_data <- subset(data_input,sample ==TRUE) 
external_validation_data <- subset(data_input, sample==FALSE)
table(train_data$outcome)
table(external_validation_data$outcome)

######################################################################################################## 
############               Final run                                          
######################################################################################################## 
##User input Pamameters
label_col_name <- "outcome"
features_to_select <- colnames(data_input)[3:6]
n_class <- 2
important_weight_threshold <- 0.2
top_feature_flag <- 1 
upsample_flag <- 1
num_rounds <- 10
num_sampling <- 5

#For multi-class classification #for multi-class, num_class needs to be specified:
#xgb_params <- list(booster = "gbtree","objective" = "multi:softmax",num_class = n_class) 
#For binary classification:
xgb_params <- list(booster = "gbtree","objective" = "reg:logistic")


##LOOCV
LOOCV_res<-main_func(train_data,features_to_select,label_col_name, top_feature_flag,important_weight_threshold,upsample_flag,num_sampling,xgb_params,num_rounds)
importantce_Matrix<-LOOCV_res[[1]]
LOOCV_AVG_performance<-round(LOOCV_res[[2]],2)
critical_features <- LOOCV_res[[3]]
#Print LOOCV performance
print(LOOCV_AVG_performance)

#Plot top feature importance
final_top_p <- Plot_FeatureImportance_func(importantce_Matrix,1,critical_features)
print(final_top_p)
#Plot all feature importance
final_p <- Plot_FeatureImportance_func(importantce_Matrix,0,critical_features)
print(final_p)

#External Validation
external_res<-main_external_func(train_data,external_validation_data,features_to_select,label_col_name, top_feature_flag,critical_features,upsample_flag,num_sampling,xgb_params,num_rounds)
external_AVG_performance<-round(external_res[[1]],2)
external_predicted_list <- external_res[[2]] #this variable contains list of tables of predicted values in each sampling 
#Print external performance
print(external_AVG_performance)
#Print external predicted values
example_prediction <- external_predicted_list[[1]]
print(example_prediction)

