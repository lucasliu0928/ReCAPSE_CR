source("Recapse_Ultility.R")
compute_sp_label_ratio <- function(in_data){
  label_tb <- table(in_data[,"Label"]) 
  ct_total <- nrow(in_data)
  ct_pre   <- label_tb["Pre"]
  ct_post  <- label_tb["Post"]
  neg_post_ratio <- round(ct_pre/ct_post,10)
  ct_tb <- data.frame("Total" = ct_total,"Pre"=ct_pre,"Post"=ct_post,"Ratio"=neg_post_ratio)
  rownames(ct_tb) <- NULL
  return(ct_tb)
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
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir  <- paste0(proj_dir, "12B_TopPCAFeature_ModelReady_TrainData/WithPossibleMonthsHasNoCodes/")

outdir   <- paste0(proj_dir, "12D_ExclusionSamples/WithPossibleMonthsHasNoCodes/")


######################################################################################################## 
#1. Load all pts model data with four top features
######################################################################################################## 
#1A. Load data
load(file = paste0(data_dir, "4F_ModelReady_TrainData.rda"))

#2B. Orginal NEG POS ratio
compute_sp_label_ratio(model_data_4f)

######################################################################################################## 
#2.Find samples < threshold OR > threshold by Most contributed feature by examing boxplot
#  Compute neg:pos ratio for each feature
######################################################################################################## 
#2A. cumul_ratio_CCS_PROC_202
sample1_idxes <- which(model_data_4f[,"cumul_ratio_CCS_PROC_202"] < 0)
sample1_data  <- model_data_4f[sample1_idxes,]
compute_sp_label_ratio(sample1_data)

afterRemoval_data <- model_data_4f[-sample1_idxes,]
compute_sp_label_ratio(afterRemoval_data)

#2B. cumul_ratio_CCS_PROC_227
threshold_list <- seq(2,7.5,0.5)
table_list <- list()
for (i in 1:length(threshold_list)){
  curr_th <- threshold_list[i]
  sample1_idxes <- which(model_data_4f[,"cumul_ratio_CCS_PROC_227"] > curr_th)
  sample1_data  <- model_data_4f[sample1_idxes,]
  sp_res <- compute_sp_label_ratio(sample1_data)
  
  afterRemoval_data <- model_data_4f[-sample1_idxes,]
  after_res <- compute_sp_label_ratio(afterRemoval_data)
  
  final_res <- rbind(sp_res,after_res)
  rownames(final_res) <-c(paste0("Sample"," > Threshold ",curr_th),"After Removal")
  table_list[[i]] <- final_res
}
all_table <- do.call(rbind,table_list)

#2C. months_since_dx
threshold_list <- seq(24,60,1)
table_list <- list()
for (i in 1:length(threshold_list)){
  curr_th <- threshold_list[i]
  sample1_idxes <- which(model_data_4f[,"months_since_dx"] < curr_th)
  sample1_data  <- model_data_4f[sample1_idxes,]
  sp_res <- compute_sp_label_ratio(sample1_data)
  
  afterRemoval_data <- model_data_4f[-sample1_idxes,]
  after_res <- compute_sp_label_ratio(afterRemoval_data)
  
  final_res <- rbind(sp_res,after_res)
  rownames(final_res) <-c(paste0("Sample"," < Threshold ",curr_th),"After Removal")
  table_list[[i]] <- final_res
}
all_table <- do.call(rbind,table_list)

#2C. Enrolled_year
threshold_list <- seq(2004,2020,1)
table_list <- list()
for (i in 1:length(threshold_list)){
  curr_th <- threshold_list[i]
  sample1_idxes <- which(model_data_4f[,"Enrolled_year"] > curr_th)
  sample1_data  <- model_data_4f[sample1_idxes,]
  sp_res <- compute_sp_label_ratio(sample1_data)
  
  afterRemoval_data <- model_data_4f[-sample1_idxes,]
  after_res <- compute_sp_label_ratio(afterRemoval_data)
  
  final_res <- rbind(sp_res,after_res)
  rownames(final_res) <-c(paste0("Sample"," > Threshold ",curr_th),"After Removal")
  table_list[[i]] <- final_res
}
all_table <- do.call(rbind,table_list)

######################################################################################################## 
#4. OBV NEGTIVES
######################################################################################################## 
#1.Find best combination of thresholds for featrues for NEGs
#  Best: good precision of selected negtive samples, neg:pos ratio of after exclusion is OK (Not too many neg or pos)

thres1_list <- seq(0,0.2,0.1) #3.5 to 5.5 by 0.5
thres2_list <- seq(24,48,4) 
n_thres1 <- length(thres1_list)
n_thres2 <- length(thres2_list)
n_comb   <- n_thres1*n_thres2
print(n_comb)
prec_ratio_tb <- as.data.frame(matrix(NA, nrow = n_comb,ncol = 6))
colnames(prec_ratio_tb) <- c("Threshold_PROC202","Threshold_PROC227","Threshold_months_since_dx",
                             "NEG_Percentage_SelectedSamples", #this is also the precision of negtives in the sample
                             "NEGtoPOS_Ratio_SelectedSamples",
                             "NEGtoPOS_Ratio_AfterExclusion")
ct <- 1
for (i in 1:length(thres1_list)){
  if(i %% 2 == 0){print(i)}
  thres1 <- thres1_list[i]
  for (j in 1:length(thres2_list)){
      thres2 <- thres2_list[j]
      
      prec_ratio_tb[ct,"Threshold_PROC202"] <- -1
      prec_ratio_tb[ct,"Threshold_PROC227"] <- thres1
      prec_ratio_tb[ct,"Threshold_months_since_dx"] <- thres2
        
      #Data to be treated as negatives
      sample1_idxes <- which(model_data_4f[,"cumul_ratio_CCS_PROC_202"] == -1 |
                               model_data_4f[,"cumul_ratio_CCS_PROC_227"] < thres1 | 
                               model_data_4f[,"months_since_dx"] < thres2 )
      sample1_data  <- model_data_4f[sample1_idxes,]
      sp_res        <- compute_sp_label_ratio(sample1_data)
      
      #Presicion of negatives of selected samples (i.e,For this samples, we predict them all as "pre")
      prec_ratio_tb[ct,"NEG_Percentage_SelectedSamples"] <- sp_res["Pre"]/(sp_res["Pre"] + sp_res["Post"]) 
      #Pre to post ratio
      prec_ratio_tb[ct,"NEGtoPOS_Ratio_SelectedSamples"] <- sp_res["Ratio"] 
      
      #after
      after_data <- model_data_4f[-sample1_idxes,]
      after_res <- compute_sp_label_ratio(after_data)
      prec_ratio_tb[ct,"NEGtoPOS_Ratio_AfterExclusion"] <- after_res["Ratio"] 
      ct <- ct + 1
  }
}


p <- ggplot(prec_ratio_tb, aes(x=NEG_Percentage_SelectedSamples, y=NEGtoPOS_Ratio_AfterExclusion)) + 
  geom_point(aes(size=2),color = "darkblue") +
  ylab("Negative to Positive Ratio After Exclusion \n (non-obvious cases)") +
  xlab("Precision of Negatives (Percentage of Negatives) \n in Selected Samples") +
  theme(legend.position = "none",
        axis.text =  element_text(size = 16),
        axis.title = element_text(size = 16)) +
  ylim(19,round(max(prec_ratio_tb$NEGtoPOS_Ratio_AfterExclusion))) + 
  geom_smooth(method='loess', formula= y~x)

print(p)

png(paste0(outdir,"Precision_Ratio_Plot_NEG.png"),res = 150,width = 1000,height = 800)
print(p)
dev.off()

#2. Find the best thresholds by the figure
tb_idxes <- which(round(prec_ratio_tb[,"NEG_Percentage_SelectedSamples"],7) == 0.9814992 & #0.9813743
                  round(prec_ratio_tb[,"NEGtoPOS_Ratio_AfterExclusion"],5) == 19.80003) #20 
#Best threshold hold
best_th1 <- prec_ratio_tb[tb_idxes,"Threshold_PROC227"]
best_th2 <- prec_ratio_tb[tb_idxes,"Threshold_months_since_dx"]

#3.Get exclusion samples IDs to be treated as negatives
#Data to be treated as negatives
final_sample_idxes <- which(model_data_4f[,"cumul_ratio_CCS_PROC_202"] == -1 |
                       model_data_4f[,"cumul_ratio_CCS_PROC_227"] < best_th1 | 
                      model_data_4f[,"months_since_dx"] < best_th2 )
final_sample_data  <- model_data_4f[final_sample_idxes,]
sp_res        <- compute_sp_label_ratio(final_sample_data)
sp_res
sp_res["Pre"]/sp_res["Total"]

sample_ID_df_NEG <- final_sample_data[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df_NEG,paste0(outdir,"ObviousNeg_Samples.csv"))



######################################################################################################## 
#4. OBV Postives
######################################################################################################## 
thres1_lower <- seq(1,2,0.5)
thres1_upper <- seq(3,4,0.5)
thres2_lower <- seq(48,60,4)
thres2_upper <- seq(78,90,4)
thres3_lower <- seq(0.1,0.2,0.05)
thres3_upper <- seq(0.3,0.4,0.05)


n_thres1 <- length(thres1_lower)
n_thres2 <- length(thres2_lower)
n_thres3 <- length(thres3_lower)
n_comb   <- n_thres1*n_thres2
print(n_comb)
prec_ratio_tb <- as.data.frame(matrix(NA, nrow = n_comb,ncol = 6))
colnames(prec_ratio_tb) <- c("Threshold_PROC202","Threshold_PROC227","Threshold_months_since_dx",
                             "POS_Percentage_SelectedSamples", #this is also the precision of pos in the sample
                             "NEGtoPOS_Ratio_SelectedSamples",
                             "NEGtoPOS_Ratio_AfterExclusion")
ct <- 1
for (i in 1:n_thres1){
  if(i %% 1 == 0){print(i)}
  thres1_l <- thres1_lower[i]
  thres1_u <- thres1_upper[i]
  for (j in 1:n_thres2){
    thres2_l <- thres2_lower[j]
    thres2_u <- thres2_upper[j]    
    
    for (k in 1:n_thres3){
        thres3_l <- thres3_lower[k]
        thres3_u <- thres3_upper[k]    
      
        prec_ratio_tb[ct,"Threshold_PROC202"] <- paste0(thres3_l ," < V < ",thres3_u)
        prec_ratio_tb[ct,"Threshold_PROC227"] <- paste0(thres1_l ," < V < ",thres1_u)
        prec_ratio_tb[ct,"Threshold_months_since_dx"] <-  paste0(thres2_l ," < V < ",thres2_u)
        
        #Data to be treated as negatives
        sample1_idxes <- which((model_data_4f[,"cumul_ratio_CCS_PROC_202"] >  thres3_l  & model_data_4f[,"cumul_ratio_CCS_PROC_202"] <  thres3_u )|
                                 (model_data_4f[,"cumul_ratio_CCS_PROC_227"] > thres1_l & model_data_4f[,"cumul_ratio_CCS_PROC_227"] < thres1_u) | 
                                 (model_data_4f[,"months_since_dx"] > thres2_l & model_data_4f[,"months_since_dx"] < thres2_u))
        sample1_data  <- model_data_4f[sample1_idxes,]
        sp_res        <- compute_sp_label_ratio(sample1_data)
        
        #Presicion of postives of selected samples (i.e,For this samples, we predict them all as "post")
        prec_ratio_tb[ct,"POS_Percentage_SelectedSamples"] <- sp_res["Post"]/(sp_res["Pre"] + sp_res["Post"]) 
        #Pre to post ratio
        prec_ratio_tb[ct,"NEGtoPOS_Ratio_SelectedSamples"] <- sp_res["Ratio"] 
        
        #after
        after_data <- model_data_4f[-sample1_idxes,]
        after_res <- compute_sp_label_ratio(after_data)
        prec_ratio_tb[ct,"NEGtoPOS_Ratio_AfterExclusion"] <- after_res["Ratio"] 
        ct <- ct + 1
        }
  }
}


p <- ggplot(prec_ratio_tb, aes(x=POS_Percentage_SelectedSamples, y=NEGtoPOS_Ratio_AfterExclusion)) + 
  geom_point(aes(size=2),color = "darkblue") +
  ylab("Negative to Positive Ratio After Exclusion \n (non-obvious cases)") +
  xlab("Precision of Postives (Percentage of Postives) \n in Selected Samples") +
  theme(legend.position = "none",
        axis.text =  element_text(size = 16),
        axis.title = element_text(size = 16)) +
  #ylim(19,round(max(prec_ratio_tb$NEGtoPOS_Ratio_AfterExclusion))) + 
  geom_smooth(method='loess', formula= y~x)

print(p)

png(paste0(outdir,"Precision_Ratio_Plot_POS.png"),res = 150,width = 1000,height = 800)
print(p)
dev.off()

#2. Find the best thresholds by the figure
tb_idxes <- which(round(prec_ratio_tb[,"POS_Percentage_SelectedSamples"],8) == 0.04179199	 &
                  round(prec_ratio_tb[,"NEGtoPOS_Ratio_AfterExclusion"],5) == 34.18392) #20 

#Best threshold hold
best_th1 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_PROC227"],split = " < "))
best_th2 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_months_since_dx"],split = " < "))
best_th3 <- unlist(strsplit(prec_ratio_tb[tb_idxes,"Threshold_PROC202"],split = " < "))

best_th1_l <- best_th1[1]
best_th1_u <- best_th1[3]
best_th2_l <- best_th2[1]
best_th2_u <- best_th2[3]
best_th3_l <- best_th3[1]
best_th3_u <- best_th3[3]

#3.Get exclusion samples IDs to be treated as postives
model_data_4f_updated <- model_data_4f[-which(model_data_4f$sample_id %in% sample_ID_df_NEG$sample_id),] #eclued obv neg IDs so that no overlap, cuz we use | in conditions
final_sample_idxes <-  which((model_data_4f_updated[,"cumul_ratio_CCS_PROC_202"] >  best_th3_l  & model_data_4f_updated[,"cumul_ratio_CCS_PROC_202"] <  best_th3_u )|
                            (model_data_4f_updated[,"cumul_ratio_CCS_PROC_227"] > best_th1_l & model_data_4f_updated[,"cumul_ratio_CCS_PROC_227"] < best_th1_u) | 
                            (model_data_4f_updated[,"months_since_dx"] > best_th2_l & model_data_4f_updated[,"months_since_dx"] < best_th2_u))
final_sample_data  <- model_data_4f_updated[final_sample_idxes,]

sp_res        <- compute_sp_label_ratio(final_sample_data)
sp_res
sp_res["Post"]/sp_res["Total"]

sample_ID_df_POS <- final_sample_data[,c("study_id","sample_id","Label")]
write.csv(sample_ID_df_POS,paste0(outdir,"ObviousPos_Samples.csv"))


######################################################################################################## 
#5. Get sample IDs after exclusion
######################################################################################################## 
obv_neg_IDs <- sample_ID_df_NEG$sample_id
obv_pos_IDs <- sample_ID_df_POS$sample_id

after_data <- model_data_4f[-which(model_data_4f$sample_id %in% c(obv_neg_IDs,obv_pos_IDs)),]
after_res <- compute_sp_label_ratio(after_data)
after_res

non_obvious_data <- after_data[,c("study_id","sample_id","Label")]
write.csv(non_obvious_data,paste0(outdir,"NON_Obvious_Samples.csv"))
