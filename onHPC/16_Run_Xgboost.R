source("Recapse_Ultility.R")
library(shapr)

#onHPC
data_dir <- "/recapse/intermediate_data/"
outdir <- "/recapse/intermediate_data/"

# #local
data_dir <- "/Users/lucasliu/Desktop/intermediate_data/"
outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

######################################################################################################## 
#load data
######################################################################################################## 
model_data <- read.csv(paste0(outdir,"15_ModelReadyData.csv"),stringsAsFactors = F)

################################################################################ 
#2. Load patient level char to get SBCE or not to make sure original ID not in both train and validation
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(data_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
final_ID <- unique(model_data$study_id)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% final_ID),]
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

##statisc
mean(nonrecurrent_pts_data$Age,na.rm = T) #69.34246
mean(recurrent_pts_data$Age,na.rm = T) #66

mean(nonrecurrent_pts_data$months_since_dx,na.rm = T) #84.5
mean(recurrent_pts_data$months_since_dx,na.rm = T) #77.9

mean(nonrecurrent_pts_data$reg_nodes_exam,na.rm = T) #10.17263
mean(recurrent_pts_data$reg_nodes_exam,na.rm = T) #11.09704

mean(nonrecurrent_pts_data$reg_nodes_pos,na.rm = T) #10.17263
mean(recurrent_pts_data$reg_nodes_pos,na.rm = T) #11.09704

length(which(nonrecurrent_pts_data$CCS_D_42==1))/n_no*100
length(which(recurrent_pts_data$CCS_D_42==1))/n_yes*100

length(which(nonrecurrent_pts_data$CCS_D_24==1))/n_no*100
length(which(recurrent_pts_data$CCS_D_24==1))/n_yes*100

length(which(nonrecurrent_pts_data$C504==1))/n_no*100
length(which(recurrent_pts_data$C504==1))/n_yes*100


length(which(nonrecurrent_pts_data$surg_prim_site_23==1))/n_no*100
length(which(recurrent_pts_data$surg_prim_site_23==1))/n_yes*100

length(which(nonrecurrent_pts_data$Laterality_1==1))/n_no*100
length(which(recurrent_pts_data$Laterality_1==1))/n_yes*100

####Sample IDs
Total_n_test <- round(6259*0.1)
Total_n_validation <- round(6259*0.1)
Total_n_Train <- round(6259*0.5)

#1.Testing : Randomly choose 100 SBCE original pts data, and 100*8 noSBCE original pt Data
set.seed(123)
test_ID_SBCE <- sample(sbce_pt_Ids,100)
test_ID_noSBCE <- sample(nosbce_pt_Ids,100*8)
test_IDs <- c(test_ID_SBCE,test_ID_noSBCE)

#remove test ID from 
remaining_ID <- final_ID[which(!final_ID %in% test_IDs)]

#1. Training 80% (50%)of the remaining_ID
training_ID <- sample(remaining_ID,length(remaining_ID)*0.8)

remaining_ID <- remaining_ID[which(!remaining_ID %in% training_ID)]

#2. validation 20% of the remaining_ID
validation_ID <- sample(remaining_ID,length(remaining_ID))


####Get data
train_data <- model_data[which(model_data$study_id %in% training_ID),]
table(train_data$y_PRE_OR_POST_2ndEvent) #114186   4707 
#downsample
pos_idxes<- which(train_data$y_PRE_OR_POST_2ndEvent==1)
neg_idxes<- which(train_data$y_PRE_OR_POST_2ndEvent==0)
sampled_neg_indxes <- sample(neg_idxes,length(pos_idxes))
train_data <- train_data[c(sampled_neg_indxes,pos_idxes),]

validation_data <- model_data[which(model_data$study_id %in% validation_ID),]
table(validation_data$y_PRE_OR_POST_2ndEvent) #89164  4572 

test_data <- model_data[which(model_data$study_id %in% test_IDs),]
table(test_data$y_PRE_OR_POST_2ndEvent) #96080  4381 




# ######################################################################################################## 
#           Teresa's code
# ######################################################################################################## 
train_label <- train_data[,"y_PRE_OR_POST_2ndEvent"]
train_label <- as.numeric(train_label)
train_data_part<-train_data[,!(names(train_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dtrain <- xgb.DMatrix(data = as.matrix(train_data_part), label = train_label)

validation_label <- validation_data[,"y_PRE_OR_POST_2ndEvent"]
validation_label <- as.numeric(validation_label)
validation_data_part<-validation_data[,!(names(validation_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dvalidation <- xgb.DMatrix(data = as.matrix(validation_data_part), label = validation_label)


test_label <- test_data[,"y_PRE_OR_POST_2ndEvent"]
test_label <- as.numeric(test_label)
test_data_part<-test_data[,!(names(test_data) %in% c("study_id","y_PRE_OR_POST_2ndEvent"))]
dtest <- xgb.DMatrix(data = as.matrix(test_data_part), label = test_label)



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


mod_easy <- xgb.train(objective="binary:logistic",
                      data=dtrain, nrounds=10)

pred <- predict(mod_optimal, dtest)
actual <-test_label


perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)
write.csv(perf,paste0(outdir,"16_perf.csv"),row.names = F)

#check 
bst <- xgboost(data = dtrain,nrounds = 10, params = list(scale_pos_weight = 9),objective = "binary:logistic")
pred <- predict(bst, dtest)
perf <- compute_binaryclass_perf_func(pred,actual)
print(perf)

importance_matrix = xgb.importance(model = mod_optimal)

importance_matrix <- xgb.importance(model = mod_optimal)
importance_matrix$Group <- as.factor(1)
gp = xgb.ggplot.importance(importance_matrix,top_n = 10,n_clusters = 1)
final_p <- gp +geom_bar(stat="identity",position = position_dodge(width=10))+aes(fill = "brown1") +
  theme_bw()+
  theme(legend.position = "none",
        #panel.grid.major = element_blank(), #remove grid
        plot.title = element_text(size= 22),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 20),
        axis.title = element_text(size=20)) +
  ggtitle("")
final_p

explainer <- shapr(as.matrix(train_data_part), mod_easy,n_combinations = 10000)
explanation <- explain(
  as.matrix(test_data_part),
  approach = "empirical",
  explainer = explainer,
  prediction_zero = p
)
