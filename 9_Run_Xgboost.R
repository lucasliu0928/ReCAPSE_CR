source("XGBUtilities.R")

proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/"
data_dir <- paste0(proj_dir,"/ReCAPSE_Intermediate_Data/0318_21/For_Both_Data/")
month_data <- read.csv(paste0(data_dir, "diag_monthly_df.csv"))
month_data <- month_data[,-5] #remove Na code columns
length(unique(month_data$ID)) #27607
nrow(month_data) #2095430
table(month_data$outcome) #0:1985309, 1:110121

#patient-wise
nonrecurrent_pts_data <- month_data[which(month_data$outcome == 0),]
length(unique(nonrecurrent_pts_data$ID)) #26935
recurrent_pts_data <- month_data[which(month_data$outcome == 1),]
length(unique(recurrent_pts_data$ID)) #2749

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

mean(perc_pts,na.rm = T)

ccol_indexes1 <- which(perc_pts > 0.05)

perc_pts <- NA
analysis_data <- nonrecurrent_pts_data
for (j in 4:length(analysis_data)){
  curr_col <- analysis_data[,j]
  n_pts <- length(which(curr_col >= 1)) #n of pts have at least one code in this group
  perc_pts[j] <- n_pts/nrow(analysis_data)
}
mean(perc_pts,na.rm = T)

ccol_indexes2 <- which(perc_pts > 0.05)
comb_indexes <- unique(ccol_indexes1,ccol_indexes2)

comb_filtered_data <- month_data[,c(1,2,3,comb_indexes)]






##########START HERE##########
######################################################################################################## 
#### Data reprocessing 0521
######################################################################################################## 
#1. Training
#1.1 Random select 4500 monthdata from no SBCE
#1.2 Random select 500 from SBCE
#1.3 Record all orignal IDs in training 

#2.Validtion
#2.1 Exclude original IDs in Trainng from the pool
#2.2 random select 560 for no SBCE
#2.3 random select 60 for no SBCE

#3.Validtion
#3.1 Exclude original IDs in Training and Validation from the pool
#3.2 random select 560 for no SBCE
#3.3 random select 60 for no SBCE

######################################################################################################## 
###############              Data preprocessing
###############     Note: Make sure label range: [0,num_class-1]
######################################################################################################## 
data_input1 <- recurrent_pts_data[sample(nrow(recurrent_pts_data), nrow(recurrent_pts_data), replace = FALSE),]
data_input2 <- nonrecurrent_pts_data[sample(nrow(nonrecurrent_pts_data), nrow(recurrent_pts_data), replace = FALSE),]
comb_data_input <- rbind(data_input1,data_input2)
table(comb_data_input$outcome)

#remove all Na columns
comb_data_input <- comb_data_input[,colSums(is.na(comb_data_input))<nrow(comb_data_input)]


#split train and test
set.seed(123)   
library(caTools)
sample <- sample.split(comb_data_input,SplitRatio = 0.8) # 0.8 for training
train_data <- subset(comb_data_input,sample ==TRUE) 
external_validation_data <- subset(comb_data_input, sample==FALSE)
table(train_data$outcome)
table(external_validation_data$outcome)

# ######################################################################################################## 
#           Teresa's code
# ######################################################################################################## 
library(rBayesianOptimization) 
library(xgboost) 
library(Matrix)

train_label <- train_data[,"outcome"]
train_label <- as.numeric(train_label)
train_data_part<-train_data[,!(names(train_data) %in% c("ID","Month_Start","outcome"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

test_label <- external_validation_data[,"outcome"]
test_label <- as.numeric(test_label)
test_data_part<-external_validation_data[,!(names(external_validation_data) %in% c("ID","Month_Start","outcome"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)



xgb_cv_bayes <- function(eta, max_depth, min_child_weight, subsample, colsample_by_tree){
   print(paste("eta:", eta))
   print(paste("max_depth:", max_depth))
   print(paste("min_child_weight:", min_child_weight)) 
   print(paste("subsample:", subsample))
   print(paste("colsample_by_tree:", colsample_by_tree))
   cv <- xgb.cv(params=list(booster="gbtree", eta=eta, max_depth=max_depth,
                            min_child_weight=min_child_weight,
                            subsample=subsample,
                            olsample_by_tree=colsample_by_tree,
                            lambda=1, alpha=0,
                            #nthread=ncores, n_jobs=ncores,
                            objective="binary:logistic", eval_metric="auc"),
                            data=dtrain, nround=5,nfold = 10,
                            prediction=TRUE, showsd=TRUE, early_stopping_rounds=100,
                            stratified=FALSE, maximize=TRUE)
   print(paste("cv:", cv))
   list(Score=cv$evaluation_log[, max(test_auc_mean)], Pred=cv$pred)
}

optimal_results <- BayesianOptimization(xgb_cv_bayes, 
                                        bounds=list(eta=c(0.001, 0.3),
                                                    max_depth=c(3L, 10L),
                                                    min_child_weight=c(0L, 20L),
                                                    subsample=c(0.3, 0.9), colsample_by_tree=c(0.2, 0.8)),
                                        init_points=10,
                                        n_iter=10)


current_best <- list(etc = as.numeric(optimal_results$Best_Par['eta']), 
                     max_depth = as.numeric(optimal_results$Best_Par['max_depth']),
                     min_child_weight = as.numeric(optimal_results$Best_Par['min_child_weight']),
                     subsample = as.numeric(optimal_results$Best_Par['subsample']),
                     colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']))
watchlist <- list(train = dtrain, eval = dtest)

mod_optimal <- xgb.train(objective="binary:logistic", 
                         params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                         watchlist=watchlist, verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")

pred <- predict(mod_optimal, dtest)
actual <-test_label
compute_binaryclass_perf_func(pred,actual)
