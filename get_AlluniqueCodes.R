library(parallel)
numCores <- detectCores() # get the number of cores available
print(numCores)
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
#"kcr_medicaid_healthclaims_fb0015.csv","kcr_medicare_claims_fb0015.csv
# start_time <- Sys.time()
# data_dir_local <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"
# df <- read.csv(paste0(data_dir,"kcr_medicaid_healthclaims_fb0015.csv"),stringsAsFactors = F)
# print(nrow(df))
# end_time <- Sys.time()
# print(end_time-start_time)

library(data.table)
start_time <- Sys.time()
dt <- fread(paste0(data_dir,"kcr_medicare_claims_fb0015.csv"))
nrow(dt)
end_time <- Sys.time()
print(end_time-start_time)