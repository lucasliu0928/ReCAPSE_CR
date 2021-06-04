library(parallel)
library(data.table)
library(foreach)
library(doParallel)

numCores <- detectCores() # get the number of cores available
print(numCores)
data_dir <- "/recapse/data/Testing data for UH3 - Dec 16 2020/"
local_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/ReCAPSE_Data/Testing data for UH3 - Dec 16 2020/"

#####
proj_dir <- "/Users/lucasliu/Desktop/DrChen_Projects/ReCAPSE_Project/" 
data_dir <- paste0(proj_dir,"ReCAPSE_Intermediate_Data/0318_21/Medicare_Intermediate_Data/Medicare_Claims_seperated/")

infiles <- dir(data_dir, full.names = TRUE)[1:5]

read_files_func <- function(file_name){
  df <- read.csv(file_name,stringsAsFactors = F)
  n_rows <- nrow(df)
  return(n_rows)
}

#data_df <- fread(paste0(local_dir,"kcr_medicaid_healthclaims_fb0015.csv"))
#unique_IDs <- unique(data_df[,"study_id"])
system.time(
for (i in 1:length(infiles)){
  res <- read_files_func(infiles[i])
}
)


system.time(
  results <- lapply(infiles, read_files_func)
)

system.time(
  results <- mclapply(infiles, read_files_func, mc.cores = numCores)
)

registerDoParallel(numCores)  # use multicore, set to the number of our cores
system.time(
res = foreach (i=1:length(infiles)) %dopar% {
     read_files_func(infiles[i])
  }
)

data=vector(length=1000)
data=foreach(i=1:1000) %dopar% {
  sqrt(1/(sin(i))^2)-sum(rnorm(10^6))
}
data=unlist(data)