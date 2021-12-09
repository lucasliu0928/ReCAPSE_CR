library(lubridate)
library(openxlsx)

create_data_everymonth_func <- function(pred_df){
  #pred_df <- curr_df
  
  m_start <- ymd(min(pred_df$month_start))
  m_end   <- ymd(max(pred_df$month_start))
  m_seq   <- seq(m_start,m_end,by = "1 month")
  
  time_df <- data.frame(matrix(NA, nrow = length(m_seq), ncol = 3))
  colnames(time_df) <- c("month_start","pred","actual")
  time_df$month_start <- m_seq
  for (i in 1:nrow(time_df)){
    curr_m <- time_df[i,"month_start"]
    curr_m_idx <- which(pred_df[,"month_start"] == curr_m)
    
    
    if (length(curr_m_idx) > 0){ # if has predicted prob in current month
      time_df[i,"pred"] <- pred_df[curr_m_idx,"pred"]
      time_df[i,"actual"] <- pred_df[curr_m_idx,"actual"]
      
    }else{
      #time_df[i,"pred"] <- NA
      time_df[i,"pred"]  <- time_df[i-1,"pred"] #copy the previous time-step
      time_df[i,"actual"] <- time_df[i-1,"actual"]
      
    }
  }
  return(time_df)
}

################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/All_DS_Performance/")
data_dir2        <- paste0(proj_dir, "12_TrainTestIDs/")

outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/All_DS_Performance/")

#User Input
ds_index <- 1

######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed","/16_Prediction_Table_DS", ds_index, "_posweight0.5.csv"))

#Add study_id and month start
original_IDs <- strsplit(as.character(test_prediction_df$sample_id),split = "@")
test_prediction_df$study_id    <- gsub("ID","",sapply(original_IDs, "[[", 1))
test_prediction_df$month_start <- sapply(original_IDs, "[[", 2)
test_ID <- unique(test_prediction_df$study_id)

################################################################################ 
#2. Load label df to get SBCE or not 
################################################################################ 
SBCE_label_df <- read.xlsx(paste0(data_dir2,"/test_ID_withLabel.xlsx"),sheet = 1)


################################################################################ 
#Curve fitting
################################################################################ 
curve_fit_prediction_list <- list()
for (i in 1:length(test_ID)){
  if(i %% 500 == 0){print(i)}
  curr_id <- test_ID[i]

  #get prediction df
  curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
  curr_df$month_start <- ymd(curr_df$month_start)
  
  #Create time series data provide pred every month, if NA, copy the last one
  #curr_df <- create_data_everymonth_func(curr_df)
  #Add month index
  curr_df$month_index <- 1:nrow(curr_df)
  
  #Logstic fitting
  lm <- glm(pred~month_index,curr_df,family = "binomial")
  smoothed_prob <- lm$fitted.values
  curr_df$smoothed_prob <- smoothed_prob
  
  curve_fit_prediction_list[[i]] <- curr_df
}

curve_fit_prediction_df <- do.call(rbind, curve_fit_prediction_list)
write.csv(curve_fit_prediction_df,paste0(outdir,"train_DS",ds_index,"/AfterSmoothed","/16_Smoothed_Prediction_Table_DS1_posweight0.5.csv"),row.names = T)

#'@TOTRY
# #Exponential
# library(aTSA)
# x <- time_df$pred
# es <- expsmooth(time_df$pred,trend = 1, alpha = 0.3, lead = 0) # trend = 1: a constant model
# plot(x,type = "l")
# lines(es$estimate,col = 2)
# expsmooth(x,trend = 2) # trend = 2: a linear model
# expsmooth(x,trend = 3) # trend = 3: a quadratic model


