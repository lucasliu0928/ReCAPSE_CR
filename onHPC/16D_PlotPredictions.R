source("Recapse_Ultility.R")
library(reshape2)

plot_individual_prediction <- function(pt_prediction_df, changepoint = NA, plot_cp = FALSE){
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
  
  if (plot_cp == T){
    p <- p + geom_vline(xintercept = changepoint, linetype="dotted", color = "darkorange", size=1.5) +
              geom_text(aes(x=changepoint, label="CP", y=1), colour="darkorange", angle=0) 
      
  }
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
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/All_DS_Performance/")
data_dir2        <- paste0(proj_dir, "12C_TrainTestIDs/")

outdir           <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1201updated/All_DS_Performance/")

#User input
ds_index <- 1

######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed","/16_Prediction_Table_DS",ds_index,".csv"))

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
#3. Load change point analysis results
################################################################################ 
changepoint_df <- read.csv(paste0(data_dir1,"train_DS",ds_index,"/BeforeSmoothed","/16_ChangePoint",".csv"),stringsAsFactors = F)

################################################################################ 
#Plot
#'@TODO: Maybe add event date Later
################################################################################ 
for (i in 1:length(test_ID)){
  if(i %% 100 == 0){print(i)}
  curr_id <- test_ID[i]
  #get prediction df
  curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
  curr_df$month_start <- ymd(curr_df$month_start)
  

  #get label
  curr_label_df <- SBCE_label_df[which(SBCE_label_df[,"study_id"] == curr_id),]
  curr_label <- curr_label_df[,"SBCE"]
  if (curr_label == 1){
    #get change point
    curr_cp <- ymd(changepoint_df[which(changepoint_df[,"study_id"] == curr_id),"changepoint_month"])
    p <- plot_individual_prediction(curr_df,curr_cp,TRUE)
  }else{
    p <- plot_individual_prediction(curr_df,NA,FALSE)
  }

  png(paste0(outdir,"train_DS",ds_index,"/BeforeSmoothed/Individual_Plot/","SBCE",curr_label,"_ID",curr_id,".png"),res=150,width = 700,height = 700)
  print(p)
  dev.off()
}
