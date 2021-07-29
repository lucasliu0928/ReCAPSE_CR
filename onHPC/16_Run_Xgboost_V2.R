source("Recapse_Ultility.R")
#This script:
#for each patient for selected features(By freq) combines:
#1.CodeCount feature
#2.CodeTrans feature
#3.BinaryChar feature

################################################################################
#Set up parallel computing envir
################################################################################
numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores


#onHPC
project_dir            <- "/recapse/intermediate_data/"
modelReady_dir         <- paste0(project_dir,"12_ModelReadyData/")
outdir                 <- paste0(project_dir,"16_Performance/")

#local
project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
modelReady_dir         <- paste0(project_dir,"12_ModelReadyData/")
outdir                 <- paste0(project_dir,"16_Performance/")


######################################################################################################## 
#1. Load and combine all patient data
######################################################################################################## 
pt_files <-list.files(modelReady_dir,full.names = T)
model_data <- do.call(rbind, lapply(pt_files,read.xlsx))
Final_ID <- unique(model_data$study_id)
print("Original pre vs post : ")
table(model_data$y_PRE_OR_POST_2ndEvent)

################################################################################ 
#2. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(project_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% Final_ID),] #only keep char for final ID
print("Original non-SBCE vs SBCE : ")
table(pts_level_char_df$SBCE)

################################################################################ 
#3. Get SBCE and non-SBCE IDs
################################################################################ 
sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
original_noSBCE_toSBCEratio <- round(length(nosbce_pt_Ids)/length(sbce_pt_Ids))

##########START HERE##########
######################################################################################################## 
#### Data reprocessing 0521
### make sure no overlapping in original Ids in train,validation and test
######################################################################################################## 
nonrecurrent_pts_data <- model_data[which(model_data$study_id %in% nosbce_pt_Ids),]
n_no <- nrow(nonrecurrent_pts_data) #619991
recurrent_pts_data <- model_data[which(model_data$study_id %in% sbce_pt_Ids),]
n_yes<- nrow(recurrent_pts_data) # 72670

#1.Testing : Randomly choose 100 SBCE original pts data, and 100*8 noSBCE original pt Data
set.seed(123)
test_ID_SBCE <- sample(sbce_pt_Ids,100)
test_ID_noSBCE <- sample(nosbce_pt_Ids,100*original_noSBCE_toSBCEratio)
test_IDs <- c(test_ID_SBCE,test_ID_noSBCE)
#remove test ID from 
remaining_ID <- final_ID[which(!final_ID %in% test_IDs)]

#1. Training 80% of the remaining_ID
training_ID <- sample(remaining_ID,length(remaining_ID)*0.8)
#2. validation 20% of the remaining_ID
validation_ID <- remaining_ID[which(!remaining_ID %in% training_ID)]


####Get data
train_data <- model_data[which(model_data$study_id %in% training_ID),]
print("Training: : ")
table(train_data$y_PRE_OR_POST_2ndEvent) 
train_label <- as.numeric(train_data[,"y_PRE_OR_POST_2ndEvent"])
train_data_part<-train_data[,!(names(train_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)


validation_data <- model_data[which(model_data$study_id %in% validation_ID),]
print("Validation: : ")
table(validation_data$y_PRE_OR_POST_2ndEvent) 
validation_label <- as.numeric(validation_data[,"y_PRE_OR_POST_2ndEvent"])
validation_data_part<-validation_data[,!(names(validation_data) %in% c("study_id","Month_Start","y_PRE_OR_POST_2ndEvent"))]
dvalidation <- xgb.DMatrix(data = as.matrix(validation_data_part), label = validation_label)


test_data <- model_data[which(model_data$study_id %in% test_IDs),]
print("Test: : ")
table(test_data$y_PRE_OR_POST_2ndEvent) 1 
test_label <- as.numeric(test_data[,"y_PRE_OR_POST_2ndEvent"])
test_data_part<-test_data[,!(names(test_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)


# ######################################################################################################## 
#           Teresa's code
# ######################################################################################################## 
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
                     colsample_by_tree = as.numeric(optimal_results$Best_Par['colsample_by_tree']),
                     scale_pos_weight = 9)
watchlist <- list(train = dtrain, eval = dvalidation)

mod_optimal <- xgb.train(objective="binary:logistic",
                         params=current_best, data=dtrain, nrounds=10, early_stopping_rounds=100, maximize=TRUE,
                         watchlist=watchlist, verbose=TRUE, print_every_n=10, eval_metric="error", eval_metric="error@0.2", eval_metric="auc")
#Prediction table
pred   <- predict(mod_optimal, dtest)
actual <- test_label
prediction_tb <- as.data.frame(cbind(pred,actual))
write.csv(prediction_tb,paste0(outdir,"16_prediction_tb.csv"),row.names = F)
#Performance table
perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)
write.csv(perf,paste0(outdir,"16_perf.csv"),row.names = F)

# #check 
# bst <- xgboost(data = dtrain,nrounds = 10, params = list(scale_pos_weight = 9),objective = "binary:logistic")
# pred <- predict(bst, dtest)
# perf <- compute_binaryclass_perf_func(pred,actual)
# print(perf)
# 
# importance_matrix = xgb.importance(model = mod_optimal)
# 
# importance_matrix <- xgb.importance(model = mod_optimal)
# importance_matrix$Group <- as.factor(1)
# gp = xgb.ggplot.importance(importance_matrix,top_n = 10,n_clusters = 1)
# final_p <- gp +geom_bar(stat="identity",position = position_dodge(width=10))+aes(fill = "brown1") +
#   theme_bw()+
#   theme(legend.position = "none",
#         #panel.grid.major = element_blank(), #remove grid
#         plot.title = element_text(size= 22),
#         axis.text.x = element_text(size = 20),
#         axis.text.y = element_text(size = 20),
#         axis.title = element_text(size=20)) +
#   ggtitle("")
# final_p
# 


# #statitiscs
# pre_recurrence_indxes <- which(model_data$y_PRE_OR_POST_2ndEvent==0)
# post_recurrence_indxes <- which(model_data$y_PRE_OR_POST_2ndEvent==1)
# features_tocompute <- c("months_since_dx","reg_age_at_dx","reg_nodes_exam","Age","reg_nodes_pos","CCS_D_42","CCS_D_24","C504","surg_prim_site_23","Laterality_1")
# stat_tb <- as.data.frame(matrix(NA, nrow = 10, ncol = 3))
# colnames(stat_tb) <- c("Features","Stats_Pre","Stats_Post")
# for (i in 1:length(features_tocompute)){
#   curr_f <- features_tocompute[i]
#   stat_tb[i,"Features"] <- curr_f
#   if (i <= 5){
#     avg0 <- round(mean(model_data[pre_recurrence_indxes,curr_f],na.rm = T),1)
#     sd0 <- round(sd(model_data[pre_recurrence_indxes,curr_f],na.rm = T),1)
#     
#     avg1 <- round(mean(model_data[post_recurrence_indxes,curr_f],na.rm = T),1)
#     sd1  <- round(sd(model_data[post_recurrence_indxes,curr_f],na.rm = T),1)
#     
#     stat_tb[i,"Stats_Pre"] <-  paste0(avg0," (",sd0,")")
#     stat_tb[i,"Stats_Post"] <- paste0(avg1," (",sd1,")")
#   }else{
#     n0 <- length(which(model_data[pre_recurrence_indxes,curr_f] == 1))
#     p0 <- round(n0/length(pre_recurrence_indxes)*100,1)
#     
#     n1 <- length(which(model_data[post_recurrence_indxes,curr_f] == 1))
#     p1 <- round(n1/length(post_recurrence_indxes)*100,1)
#     
#     stat_tb[i,"Stats_Pre"] <-  paste0(n0," (",p0,"%)")
#     stat_tb[i,"Stats_Post"] <- paste0(n1," (",p1,"%)")
#   }
# }
# 
# write.csv(stat_tb,"/Users/lucasliu/Desktop/stats.csv")