library(openxlsx)library(openxlsx)
library(data.table)
library(lubridate)
library(parallel)
library(doParallel)

numCores <- detectCores() # get the number of cores available
print(numCores)
registerDoParallel(numCores)  # use multicore, set to the number of our cores

data_dir1 <- "/recapse/intermediate_data/Freq_SBCE/diag"
data_dir2 <- "/recapse/intermediate_data/Freq_noSBCE/diag"
outdir <- "/recapse/intermediate_data/"

# #local
# data_dir1 <- "/Users/lucasliu/Desktop/intermediate_data/Freq_SBCE/diag"
# data_dir2 <- "/Users/lucasliu/Desktop/intermediate_data/Freq_noSBCE/diag"
# outdir <- "/Users/lucasliu/Desktop/intermediate_data/"

sbce_files <- list.files(data_dir1,full.names = T)
sbce_freq_tb <- do.call(rbind,mclapply(sbce_files, read.xlsx,mc.cores = numCores))
write.xlsx(sbce_freq_tb,paste0(outdir,"sbce_freq_tb.xlsx"))


nosbce_files <- list.files(data_dir2,full.names = T)
nosbce_freq_tb <- do.call(rbind,mclapply(nosbce_files, read.xlsx,mc.cores = numCores))
write.xlsx(nosbce_freq_tb,paste0(outdir,"nosbce_freq_tb.xlsx"))
