source("Recapse_Ultility.R")
library(reshape2)

plot_individual_prediction <- function(pt_prediction_df){
  #pt_prediction_df <- curr_df
  
  plot_data           <- curr_df[,c("month_start","actual","pred")]
  colnames(plot_data) <- c("month_start","Acutal","Predicted")
  
  reshaped_plot_data <- melt(plot_data,id.vars="month_start") #reshape
  
  p <- ggplot(reshaped_plot_data, aes(x= month_start, y= value,color = variable, linetype = variable,shape = variable)) +
    geom_point(size=5) + 
    geom_line(size=1)+
    scale_linetype_manual("variable",values=c(0,1)) +
    scale_color_manual("variable", 
                       values = c("Acutal" = "darkblue",
                                  "Predicted" = "darkred"))+
    scale_shape_manual("variable",values=c(16,NA)) + 
    ylim(c(0,1)) +
    xlab("Year") +
    ylab("Predicted Probability")+
    theme_bw() +  theme(legend.position="top", legend.title = element_blank()) +
    theme(axis.text=element_text(size=14),
          axis.title=element_text(size=14,face="bold"),
          legend.text=element_text(size=14))
  
  return(p)
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

#User input
ds_index <- 1

######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/AfterSmoothed","/16_Smoothed_Prediction_Table_DS", ds_index, "_posweight0.5.csv"))

#Remove old prediction
test_prediction_df <- test_prediction_df[-which(colnames(test_prediction_df)=="pred")]
#rename smoothed prediction as pred
colnames(test_prediction_df)[which(colnames(test_prediction_df)=="smoothed_prob")] <- "pred"


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
#Plot
#'@TODO: Maybe add event date lalter
################################################################################ 
for (i in 1:length(test_ID)){
  curr_id <- test_ID[i]
  #get prediction df
  curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
  curr_df$month_start <- ymd(curr_df$month_start)
  
  #get label
  curr_label_df <- SBCE_label_df[which(SBCE_label_df[,"study_id"] == curr_id),]
  curr_label <- curr_label_df[,"SBCE"]
  p <- plot_individual_prediction(curr_df)
  
  png(paste0(outdir,"train_DS",ds_index,"/AfterSmoothed/Individual_Plot/","SBCE",curr_label,"_ID",curr_id,".png"))
  print(p)
  dev.off()
}
