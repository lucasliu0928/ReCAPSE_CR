source("Recapse_Ultility.R")

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
data_dir1        <- paste0(proj_dir, "16_Performance_WithSurgPrimSite_V1_1111updated/")
data_dir2        <- paste0(proj_dir, "0_Codes/Grouped_CleanUniqueCodes/")

#Downsampled index
ds_index <- 1

################################################################################
#Load important features for each DS
################################################################################
important_f_df <- read.csv(paste0(data_dir1,"All_DS_Performance/train_DS",ds_index,"/BeforeSmoothed/16_importance_matrix_DS", ds_index ,"_posweight0.5.csv"), stringsAsFactors = F)
important_f_df <- important_f_df[order(important_f_df[,"Gain"],decreasing = T),]

#Add CCS type and code
res <- extract_ccs_typeAndcode(important_f_df[,"Feature"])
important_f_df$CCS_Type <- res[[1]]
important_f_df$CCS_Code <- res[[2]]

################################################################################
#Load CCS cateogry names and add discrption to imporatance matrix
################################################################################
Diag_grp <- read.xlsx(paste0(data_dir2,"Unique_Diag_And_Groups_inALLClaims.xlsx"), sheet = 1)
Proc_grp <- read.xlsx(paste0(data_dir2,"Unique_Proc_And_Groups_inALLClaims.xlsx"), sheet = 1)

#Add CCS description
important_f_df$CCS_descrption <- NA
for(i in 1:nrow(important_f_df)){ 
    curr_ccs_code <- important_f_df[i,"CCS_Code"]
    curr_ccs_type <- important_f_df[i,"CCS_Type"]
    if (is.na(curr_ccs_type) == F & curr_ccs_code != "NA"){
            if (curr_ccs_type == "DIAG"){
                curr_discrip <- find_ccs_discrption_func(Diag_grp,curr_ccs_code)
            }else if(curr_ccs_type == "PROC"){
                curr_discrip <- find_ccs_discrption_func(Proc_grp,curr_ccs_code)
            }else{
                curr_discrip <- NA
            }
    }else{
            curr_discrip <- NA
    }
    
    
    important_f_df$CCS_descrption[i] <- curr_discrip[1] #if multiple has the same nchar, use the first one
}


################################################################################
#Plot importance
################################################################################
plot_df <- important_f_df[1:15,]
p <-ggplot(plot_df,aes(x = factor(Feature, levels = rev(Feature)), y = Gain, width = 0.5)) +
        geom_bar(fill = "steelblue3",color = "steelblue3",stat = "identity", position = "identity") +
        coord_flip() +
        xlab("Features") +
        ggtitle("Feature importance") +
        theme(plot.title = element_text(lineheight = .9, face = "bold"),
              panel.grid.major.y = element_blank(),
              axis.text  = element_text(size = 20),
              axis.title  = element_text(size = 22))
p

outfile <- paste0(data_dir1, "All_DS_Performance/train_DS",ds_index,"/BeforeSmoothed/","important_features_top15.png")
png(outfile, width = 1000,height = 1000, res = 100)
print(p)
dev.off()
