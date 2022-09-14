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
#proj_dir  <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Intermediate_Data/0610_21/"


feature_set_name  <- "CCSandVAL2nd"     #choose from CCSandDM3SPE , CCSandVAL2nd
SBCE_ID_Folder    <- "SBCE_Excluded_DeathPts" #Choose SBCE or SBCE_Excluded_DeathLabel or SBCE_Excluded_DeathPts
sample_name       <- "All_Samples"  #choose from "All_Samples" , "Samples_HasAtLeastOneCodeGrpFeature"


#data dir
data_dir1 <- paste0(proj_dir,"17_Performance/",feature_set_name,"/",sample_name,"/",SBCE_ID_Folder, "/")

outdir <- paste0(proj_dir,"17_Performance/",feature_set_name,"/",sample_name,"/", SBCE_ID_Folder, "/")


################################################################# 
#Sample Level Combine all performance table
#'@NOTE: Only combine performance for entire test samples 
#'(other ones e.g., obvneg,obvpos,hascode,nocode will be found in the performance folder)
################################################################# 
model_list <- c("Hybrid","AI","HybridCurveFit","AICurveFit")
perf_tb_list <- list()
ct <- 1
for (ds_index in 0:10){
  for (model in model_list){
      #Classification perf table
      cur_perf_f <- paste0(data_dir1,"DS",ds_index,"/Sample_Level/", model,"_perf_tb_alltest",".csv")
      cur_perf_tb <- read.csv(cur_perf_f,stringsAsFactors = F)
      cur_perf_tb[,"MODEL"] <- model
      cur_perf_tb[,"DS_INDEX"] <- paste0("DS",ds_index)
      perf_tb_list[[ct]] <- cur_perf_tb
      ct <- ct + 1
  }
}

classification_perf_table <- do.call(rbind,perf_tb_list)
write.csv(classification_perf_table, paste0(outdir, "Sample_Level_AllModel_Perf_tb_alltest.csv"), row.names = F)



################################################################# 
#Patient Level Combine all performance table
#'@NOTE: Only combine performance for entire test samples 
#'(other ones e.g., hasnocode,hascode will be found in the performance folder)
################################################################# 
model_list <- c("Hybrid","AI","HybridCurveFit","AICurveFit")
method_list <- c("BinSeg","OneMonth_GT_Threshold","Persis3Month_GT_Threshold")

perf_tb_list1 <- list()
perf_tb_list2 <- list()
ct <- 1
for (ds_index in 0:10){
  for (model in model_list){
    for (method in method_list){
      #Classification perf table
      cur_perf_f <- paste0(data_dir1,"DS",ds_index,"/Patient_Level/", model,method,"_AllSAMPLE_perf_tb_alltest",".csv")
      cur_perf_tb <- read.csv(cur_perf_f,stringsAsFactors = F)
      cur_perf_tb[,"MODEL"] <- model
      cur_perf_tb[,"METHOD"] <- method
      cur_perf_tb[,"DS_INDEX"] <- paste0("DS",ds_index)
      perf_tb_list1[[ct]] <- cur_perf_tb
      #Month diff perf table
      cur_perf_f2 <- paste0(data_dir1,"DS",ds_index,"/Patient_Level/", model,method,"_AllSAMPLE_MonthDiff_Perf_SBCE",".csv")
      cur_perf_tb2 <- read.csv(cur_perf_f2,stringsAsFactors = F)
      cur_perf_tb2[,"MODEL"] <- model
      cur_perf_tb2[,"METHOD"] <- method
      cur_perf_tb2[,"DS_INDEX"] <- paste0("DS",ds_index)
      perf_tb_list2[[ct]] <- cur_perf_tb2
      ct <- ct + 1
    }
  }
}

classification_perf_table <- do.call(rbind,perf_tb_list1)
momthdiff_perf_table <- do.call(rbind,perf_tb_list2)

write.csv(classification_perf_table, paste0(outdir, "Patient_Level_AllModelMethods_Perf_tb_alltest.csv"),row.names = F)
write.csv(momthdiff_perf_table, paste0(outdir, "Patient_Level_AllModelMethods_MonthDiff_Perf_tb_SBCE.csv"),row.names = F)
