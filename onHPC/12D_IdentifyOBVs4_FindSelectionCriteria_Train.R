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

plot_ratio_vs_perc_func <- function(perc_ratio_table,x_col, y_col,x_lab,y_lab,ylim_min){
  p <- ggplot(perc_ratio_table, aes_string(x=x_col, y=y_col)) + 
    geom_point(aes(size=2),color = "darkblue") +
    ylab(y_lab) +
    xlab(x_lab) +
    theme(legend.position = "none",
          axis.text =  element_text(size = 16),
          axis.title = element_text(size = 16)) +
    ylim(ylim_min,round(max(perc_ratio_table[,y_col]))) + 
    geom_smooth(method='loess', formula= y~x)
  
  return(p)
}

find_final_thres_func <- function(perc_ratio_table,x_col,y_col, turning_pt_x, turning_pt_y, var_name1, var_name2,var_name3){
  tb_idxes <- which(round(perc_ratio_table[,x_col],7) == turning_pt_x & 
                      round(perc_ratio_table[,y_col],5) ==  turning_pt_y) 
  
  #Best threshold hold
  best_th1 <- perc_ratio_table[tb_idxes,var_name1]
  best_th2 <- perc_ratio_table[tb_idxes,var_name2]
  best_th3 <- perc_ratio_table[tb_idxes,var_name3]
  
  #Store them in a dataframe
  thresholds <- c(best_th1,best_th2,best_th3)
  vars <- c(var_name1,var_name2,var_name3)
  best_th_df <- cbind.data.frame(vars,thresholds)
  return(best_th_df)
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


SBCE_col    <- "SBCE_Excluded_DeathLabel" #choose SBCE or SBCE_Excluded_DeathLabel
feature_set_name <- "CCSandVAL2nd"

#data dir
data_dir  <- paste0(proj_dir,"12B_TopPCAFeatureData_Train/",feature_set_name,"/",SBCE_col,"/")

newout <- paste0("12D_OBVsSample_Thresholds/",feature_set_name,"/",SBCE_col,"/")
outdir   <- paste0(proj_dir, newout)
dir.create(file.path(proj_dir, newout), recursive = TRUE)

######################################################################################################## 
#1. Load all pts model data with four top features
######################################################################################################## 
#1A. Load data
load(file = paste0(data_dir, "Top4PCAFeature_ModelReadyData_Train.rda"))

#2B. Original NEG POS ratio
compute_sp_label_ratio(model_data_4f)

######################################################################################################## 
#2. OBV NEGTIVES
#Find best combination of thresholds for featrues for NEGs
# Best: 
#1. good precision of selected negtive samples (Proportion of neg is high)
#2  neg:pos ratio of after exclusion is OK (more balanced)
######################################################################################################## 
#2A. Get threshold list by checking the distribution plot
#Anyone with values < thres_list is considered as obv negtives
thres1_list <- seq(0,0.2,0.1) #CCS 227
thres2_list <- seq(24,48,4)   #Month since dx

#2B.Get precisions and neg:pos ratio for each combination of threshold
n_thres1 <- length(thres1_list) 
n_thres2 <- length(thres2_list) 
n_comb   <- n_thres1*n_thres2
print(n_comb)
prec_ratio_tb <- as.data.frame(matrix(NA, nrow = n_comb,ncol = 6))
colnames(prec_ratio_tb) <- c("Threshold_PROC202",
                             "Threshold_PROC227",
                             "Threshold_months_since_dx",
                             "NEG_Percentage_SelectedSamples", #this is also the precision of negtives in the sample
                             "NEGtoPOS_Ratio_SelectedSamples",
                             "NEGtoPOS_Ratio_AfterExclusion")
ct <- 1
for (i in 1:length(thres1_list)){
  if(i %% 2 == 0){print(i)}
  thres1 <- thres1_list[i]
  for (j in 1:length(thres2_list)){
      thres2 <- thres2_list[j]
      
      prec_ratio_tb[ct,"Threshold_PROC202"] <- paste0("=","-1")
      prec_ratio_tb[ct,"Threshold_PROC227"] <- paste0("<",thres1) 
      prec_ratio_tb[ct,"Threshold_months_since_dx"] <- paste0("<",thres2) 
        
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

##2C. Plot neg:pos ratio vs. percentage of negative after exclusion
x_col = "NEG_Percentage_SelectedSamples"
y_col = "NEGtoPOS_Ratio_AfterExclusion"
x_lab = "Precision of Negatives (Percentage of Negatives) \n in Selected Samples"
y_lab = "Negative to Positive Ratio After Exclusion \n (non-obvious cases)"
ylim_min = 19
p <- plot_ratio_vs_perc_func(prec_ratio_tb, x_col,y_col,x_lab,y_lab,ylim_min)
png(paste0(outdir,"Precision_Ratio_Plot_NEG.png"),res = 150,width = 1000,height = 800)
print(p)
dev.off()


#2D. Find the best thresholds by checking the figure 
if (SBCE_col == "SBCE_Excluded_DeathLabel" & feature_set_name == "CCSandVAL2nd"){
  turning_pt_x <-  0.9812913
  turning_pt_y <-  19.94299
}else if (SBCE_col == "SBCE" & feature_set_name == "CCSandVAL2nd"){
  turning_pt_x <- 0.9814992 
  turning_pt_y <- 19.80003
}

var_name1 <- "Threshold_PROC202"
var_name2 <- "Threshold_PROC227"
var_name3 <- "Threshold_months_since_dx" 
thres_neg <- find_final_thres_func(prec_ratio_tb, x_col,y_col,turning_pt_x, turning_pt_y,var_name1, var_name2,var_name3)
write.csv(thres_neg,paste0(outdir,"Threshold_NEG.csv"))


######################################################################################################## 
#3. OBV Postives
####################################################################################################### 
#3A. Get threshold list by checking the distribution plot
thres1_lower <- seq(1,2,0.5)      #CCS 227
thres1_upper <- seq(3,4,0.5)      #CCS 227
thres2_lower <- seq(48,60,4)      #month since dx
thres2_upper <- seq(78,90,4)      #month since dx
thres3_lower <- seq(0.1,0.2,0.05) #CCS 202
thres3_upper <- seq(0.3,0.4,0.05) #CCS 202

#3B.Get precisions and neg:pos ratio for each combination of threshold
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

##3C. Plot neg:pos ratio vs. percentage of postive after exclusion
x_col = "POS_Percentage_SelectedSamples"
y_col = "NEGtoPOS_Ratio_AfterExclusion"
x_lab = "Precision of Postives (Percentage of Postives) \n in Selected Samples"
y_lab = "Negative to Positive Ratio After Exclusion \n (non-obvious cases)"
ylim_min = 33
p <- plot_ratio_vs_perc_func(prec_ratio_tb, x_col,y_col,x_lab,y_lab,ylim_min)
png(paste0(outdir,"Precision_Ratio_Plot_POS.png"),res = 150,width = 1000,height = 800)
print(p)
dev.off()


#3D. Find the best thresholds by checking the figure 
if (SBCE_col == "SBCE_Excluded_DeathLabel" & feature_set_name == "CCSandVAL2nd"){
  turning_pt_x <- 0.0417078
  turning_pt_y <- 35.81769
}else if (SBCE_col == "SBCE" & feature_set_name == "CCSandVAL2nd"){
  turning_pt_x <- 0.0417920
  turning_pt_y <- 34.18392
}

var_name1 <- "Threshold_PROC202"
var_name2 <- "Threshold_PROC227"
var_name3 <- "Threshold_months_since_dx" 
thres_pos <- find_final_thres_func(prec_ratio_tb, x_col,y_col,turning_pt_x, turning_pt_y,var_name1, var_name2,var_name3)
write.csv(thres_pos,paste0(outdir,"Threshold_POS.csv"))


