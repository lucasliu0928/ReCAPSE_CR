source("Recapse_Ultility.R")

compute_perf <- function(prediction_df){
  #prediction_df <- test_pred3
  
  predicted_class <- prediction_df[,"pred"]
  actual_label    <- prediction_df[,"y_PRE_OR_POST_2ndEvent"]
  
  #Match label factor levels
  matched_res   <- match_label_levels_func(predicted_class,actual_label)
  final_pred    <- matched_res[[1]]
  final_actual  <- matched_res[[2]]
  
  cm<-confusionMatrix(final_pred, final_actual, positive = "1", dnn = c("Prediction", "TrueLabels"),mode = "everything")
  #Manually get TN, FP, TP, FN
  # cm_tb <- cm$table
  # TN <- cm_tb[1,1]
  # FP <- cm_tb[2,1]
  # TP <- cm_tb[2,2]
  # FN <- cm_tb[1,2]
  
  TN <- length(which(final_pred==0 & final_actual==0))
  FP <- length(which(final_pred==1 & final_actual==0))
  TP <- length(which(final_pred==1 & final_actual==1))
  FN <- length(which(final_pred==0 & final_actual==1))
  
  #class 1
  performance_table <- cm$byClass[c("Sensitivity","Specificity",
                                    "Pos Pred Value","Neg Pred Value",
                                    "Precision", "Recall","F1")]
  performance_table["Accuracy"] <- cm$overall[1]
  #performance_table["AUC"]      <- auc_score
  performance_table["TNR_Specificity"] <- TN/(TN + FP) #True negative rate = specificity
  performance_table["FPR"] <- FP/(TN + FP) #False postive rate
  performance_table["TPR_Sensitivity_Recall"] <- TP/(TP + FN) #True postive rate = sensitivity = recall
  performance_table["FNR"] <- FN/(TP + FN) #False negative rate 
  
  performance_table <- round(performance_table,2)

  #Reognized the table
  perf_df <- data.frame(matrix(NA,nrow = 1, ncol = 8))
  colnames(perf_df) <- c("Accuracy","Recall/Sensitivity/TPR",
                         "Specificity/TNR",
                         "Precision/PPV","F1",
                         "NPV","FPR","FNR")
  perf_df[1,"Accuracy"]               <- performance_table["Accuracy"]
  perf_df[1,"Recall/Sensitivity/TPR"] <- performance_table["Recall"]
  perf_df[1,"Specificity/TNR"]        <- performance_table["Specificity"]
  perf_df[1,"Precision/PPV"]          <- performance_table["Precision"]
  perf_df[1,"F1"]                     <- performance_table["F1"]
  perf_df[1,"NPV"]                    <- performance_table["Neg Pred Value"]
  perf_df[1,"FPR"]                    <- performance_table["FPR"]
  perf_df[1,"FNR"]                    <- performance_table["FNR"]
  return(perf_df)
}


################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1       <- paste0(proj_dir, "15_XGB_Input/")
outdir           <- paste0(proj_dir, "21_BaselineResults/")

################################################################################ 
#5.Load data test data
################################################################################ 
#B. Entire Testing
load(file = paste0(data_dir1, "test_data.rda"))
table(test_data$y_PRE_OR_POST_2ndEvent) #0:239885, 1: 8847 

################################################################################ 
#3.Baseline 1: Random Gusses
################################################################################ 
test_pred1 <- data.frame(matrix(NA, nrow = nrow(test_data), ncol = 3))
colnames(test_pred1) <- c("sample_id","pred","y_PRE_OR_POST_2ndEvent")
test_pred1[,"sample_id"] <- test_data[,"sample_id"]
test_pred1[,"y_PRE_OR_POST_2ndEvent"] <- test_data[,"y_PRE_OR_POST_2ndEvent"]
for (i in 1:nrow(test_pred1)){
  if (i %% 1000 == 0){print(i)}
  set.seed(i)
  test_pred1[i,"pred"] <- sample(c(0,1), 1) #random generate 0 or 1
}

perf1_df <- compute_perf(test_pred1)
rownames(perf1_df) <- "RandomGuess"
################################################################################ 
#4.Baseline 2: All1s
################################################################################ 
test_pred2 <- data.frame(matrix(NA, nrow = nrow(test_data), ncol = 3))
colnames(test_pred2) <- c("sample_id","pred","y_PRE_OR_POST_2ndEvent")
test_pred2[,"sample_id"] <- test_data[,"sample_id"]
test_pred2[,"y_PRE_OR_POST_2ndEvent"] <- test_data[,"y_PRE_OR_POST_2ndEvent"]
test_pred2[,"pred"] <- 1
perf2_df <- compute_perf(test_pred2)
rownames(perf2_df) <- "All1s"
################################################################################ 
#4.Baseline 3: All0s
################################################################################ 
test_pred3 <- data.frame(matrix(NA, nrow = nrow(test_data), ncol = 3))
colnames(test_pred3) <- c("sample_id","pred","y_PRE_OR_POST_2ndEvent")
test_pred3[,"sample_id"] <- test_data[,"sample_id"]
test_pred3[,"y_PRE_OR_POST_2ndEvent"] <- test_data[,"y_PRE_OR_POST_2ndEvent"]
test_pred3[,"pred"] <- 0
perf3_df <- compute_perf(test_pred3)
rownames(perf3_df) <- "All0s"
################################################################################ 
#All performance
################################################################################ 
all_perf_df <- rbind(perf1_df,perf2_df,perf3_df)
write.csv(all_perf_df,paste0(outdir,"perf_baseline.csv"))


