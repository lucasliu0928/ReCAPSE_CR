source("Recapse_Ultility.R")
library(reshape2)

plot_individual_prediction <- function(pt_prediction_df){
  #pt_prediction_df <- curr_df
  
  plot_data <- curr_df[,c("Month_Start","actual","pred")]
  colnames(plot_data) <- c("Month_Start","Acutal","Predicted")
  
  reshaped_plot_data <- melt(plot_data,id.vars="Month_Start") #reshape
  
  p <- ggplot(reshaped_plot_data, aes(x= Month_Start, y= value,color = variable, linetype = variable,shape = variable)) +
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


#onHPC
project_dir            <- "/recapse/intermediate_data/"
perfdir                 <- paste0(project_dir,"16_Performance/")
outdir                  <- paste0(project_dir,"16_Performance/Individual_Plot/")

#local
project_dir            <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"
perfdir                 <- paste0(project_dir,"16_Performance/")
outdir                  <- paste0(project_dir,"16_Performance/Individual_Plot/")
######################################################################################################## 
#1.Load predictions 
######################################################################################################## 
#Load prediction data
test_prediction_df <- read.csv(paste0(perfdir,"16_prediction_tb_withDS.csv"))
test_ID <- unique(test_prediction_df$study_id)

################################################################################ 
#Load event date data 
################################################################################ 
all_event_df <- read.xlsx(paste0(project_dir,"/4_updated_All_event_df.xlsx"),sheet = 1)
all_event_df <- all_event_df[which(all_event_df$study_id %in% test_ID),] #only keep char for test ID
nodeath_pt <- unique(all_event_df$study_id[which(all_event_df$First_Primary_BC_related_Death == 0)])

################################################################################ 
#2. Load patient level char to get SBCE or not 
################################################################################ 
pts_level_char_df <- read.xlsx(paste0(project_dir,"/8_PatientLevel_charecteristics.xlsx"),sheet = 1)
pts_level_char_df <- pts_level_char_df[which(pts_level_char_df$study_id %in% test_ID),] #only keep char for test ID

sbce_pt_Ids <-   unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 1)])
nosbce_pt_Ids <- unique(pts_level_char_df$study_id[which(pts_level_char_df$SBCE == 0)])
sbceNoDeath_pt_Ids <- sbce_pt_Ids[which(sbce_pt_Ids %in% nodeath_pt)]

################################################################################ 
#Plot
################################################################################ 
#SBCE but no death pts
set.seed(123)
sampled_sbceNoDeath_pt_Ids <- sample(sbceNoDeath_pt_Ids,20)
for (i in 1:length(sampled_sbceNoDeath_pt_Ids)){
    curr_id <- sampled_sbceNoDeath_pt_Ids[i]
    
    #Get event date
    event_date1 <- all_event_df[which(all_event_df$study_id == curr_id),"Date_1st_Event"]
    event_date2 <- all_event_df[which(all_event_df$study_id == curr_id),"Date_2nd_Event"]
    event_date3 <- all_event_df[which(all_event_df$study_id == curr_id),"Date_3rd_Event"]
    
    #get prediction df
    curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
    curr_df$Month_Start <- ymd(curr_df$Month_Start)
    
    #Check if any claims later than 2nd event date
    claim_month_index_Later <- which(curr_df$Month_Start >= mdy(event_date2))
    if (length(claim_month_index_Later) == 0){ #if no claims date later or equal to 2nd event, add 2nd event date
       event2_df <- data.frame(study_id = curr_id, 
                               Month_Start = mdy(event_date2), 
                               pred = NA,
                               actual = 1)
       curr_df <- rbind(curr_df,event2_df)
    }
    p <- plot_individual_prediction(curr_df)
    png(paste0(outdir,"SBCE1_","ID",curr_id,".png"))
    print(p)
    dev.off()
}

#no SBCE pts
set.seed(123)
sampled_nosbce_pt_Ids <- sample(nosbce_pt_Ids,20)
for (i in 1:length(sampled_nosbce_pt_Ids)){
  curr_id <- sampled_nosbce_pt_Ids[i]
  curr_df <- test_prediction_df[which(test_prediction_df[,"study_id"] == curr_id),]
  curr_df$Month_Start <- ymd(curr_df$Month_Start)
  p <- plot_individual_prediction(curr_df)
  png(paste0(outdir,"SBCE0_","ID",curr_id,".png"))
  print(p)
  dev.off()
}

