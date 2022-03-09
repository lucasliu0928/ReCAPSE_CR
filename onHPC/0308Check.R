load(file = paste0(data_dir, "All_PTS_ModelReadyData.rda"))

transf_features_indxes <- which(grepl("time_since|time_until|cumul_ratio",colnames(model_data)))


check_df <- model_data[,transf_features_indxes]
idxes <- NA
ct <- 1
for (j in 1:ncol(check_df)){
  cur_col <- check_df[,j]
  if ( sum(cur_col) == -nrow(check_df)){
    idxes[ct] <- j
    ct <- ct + 1
  }
}

problem_codes <- as.data.frame(colnames(check_df)[idxes])

check_df2 <- check_df[,c(idxes[1],idxes[1])]
check_df3 <- model_data[,c(1,253)]
check_df5 <- curr_grp_f_df[,c(1,2,195)]

which(grepl("VAL_2ND_Ace.Inhibitors",colnames(check_df)))
