source("Recapse_Ultility.R")
library(reshape2)

plot_individual_prediction <- function(pt_prediction_df, acutal_pre_post_label_col, pred_prob_col,pred_month, plot_pm = FALSE){
  plot_data           <- curr_df[,c("month_start",acutal_pre_post_label_col,pred_prob_col)]
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
  
  if (plot_pm == T){
    p <- p + geom_vline(xintercept = pred_month, linetype="dotted", 
                        color = "darkorange", size=1.5) +
              geom_text(aes(x=pred_month, label="Predicted Month", y=1), 
                        colour="darkorange", angle=0) 
  }
  return(p)
}



################################################################################ 
#User input
#'@NOTE: 
#'For CCSandVAL2nd:
#'ds_index = 3 for SBCE
#'ds_index = 1 for SBCE_Excluded_DeathLabel
#'ds_index = 5 for SBCE_Excluded_DeathPts

#'For CCSandDM3SPE:
#'ds_index = 4 for SBCE
#'ds_index =   for SBCE_Excluded_DeathPts
################################################################################ 
feature_set_name  <- "CCSandDM3SPE"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"
ds_index          <- 4 

model <- "AI"                            #c("Hybrid","AI","HybridCurveFit","AICurveFit")
method <- "Persis3Month_GT_Threshold"    #c("BinSeg","OneMonth_GT_Threshold","Persis3Month_GT_Threshold")
ths <- seq(1,9,1)
if ((SBCE_ID_Folder == "SBCE") | (SBCE_ID_Folder == "SBCE_Excluded_DeathPts")){
  label_col   <- "y_PRE_OR_POST_2ndEvent" 
  SBCE_col <- "SBCE"
}else{
  label_col   <- "y_PRE_OR_POST_2ndEvent_ExcludedDeath" 
  SBCE_col <- "SBCE_Excluded_DeathLabel"
}


################################################################################
#Data dir
################################################################################
#onHPC
proj_dir  <- "/recapse/intermediate_data/"

#local
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"

#data dir
data_dir1 <- paste0(proj_dir, "16C_Predictions/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder,"/Test/")
outdir    <- paste0(proj_dir,"17_Performance/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder, "/")


################################################################################ 
#2.Get data for plot
################################################################################ 
#Create out dir for each ds 
ds_out <- paste0("DS",ds_index,"/Z_Predicted_Trajectory/",model,"_",method,"/")
dir.create(file.path(outdir, ds_out), recursive = TRUE)


#1. Load all sample prediction table
sample_pred_dir  <- paste0(data_dir1,"DS",ds_index,"/Sample_Prediction_Table/")
sample_pred_file <- paste0("pred_tb_",model,".csv")
sp_pred_df <- read.csv(paste0(sample_pred_dir,sample_pred_file),stringsAsFactors = F)
sp_pred_df[,"month_start"] <- ymd(sp_pred_df[,"month_start"])
sp_predprob_col <- paste0("pred_Method_", model)

#2. Load all patient prediction table
pt_pred_dir  <- paste0(data_dir1,"DS",ds_index,"/Patient_Prediction_Table/")
pt_pred_file <- paste0(model, "_", method,"_patientlevel_pred_tb.csv")
pt_pred_df <- read.csv(paste0(pt_pred_dir,pt_pred_file),stringsAsFactors = F)
pt_predmonth_col <- "Pred_SBCEMon_Thres_05"
pt_pred_df[,"Acutal_SBCEMonth"] <- ymd(pt_pred_df[,"Acutal_SBCEMonth"])
pt_pred_df[,pt_predmonth_col] <- ymd(pt_pred_df[,pt_predmonth_col])

################################################################################ 
#Plot
#'@TODO: Maybe add event date Later
################################################################################ 
test_ID <- unique(pt_pred_df$study_id)

for (i in 1:length(test_ID)){
  if(i %% 100 == 0){print(i)}
  curr_id <- test_ID[i]
  #get prediction df
  curr_df <- sp_pred_df[which(sp_pred_df[,"study_id"] == curr_id),]
  

  #get label and predicted month
  curr_indxes <- which(pt_pred_df[,"study_id"] == curr_id)
  curr_label <- pt_pred_df[curr_indxes,SBCE_col]
  curr_pred_month <- pt_pred_df[curr_indxes,pt_predmonth_col]
  
  if (curr_label == 1){
    p <- plot_individual_prediction(curr_df, label_col, 
                                    sp_predprob_col,curr_pred_month, plot_pm = TRUE)
      
    png(paste0(outdir, ds_out,SBCE_col,curr_label,"_",curr_id,".png"),res=150,width = 700,height = 700)
    print(p)
    dev.off()
  }
}
