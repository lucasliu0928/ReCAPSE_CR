remove_NA_func<- function(curr_day_codes){
  na_idxes <- which(curr_day_codes == "" | is.na(curr_day_codes) == TRUE)
  if (length(na_idxes) > 0){
    curr_day_codes <- curr_day_codes[-na_idxes]
  }
  return(curr_day_codes)
}



clean_code_func <-function(list_of_codes){
  #list_of_codes <- curr_day_diag
  
  #1.omitting any codes with non-alphanumeric characters,
  updated_list_of_codes<- gsub("[^[:alnum:]]", " ", list_of_codes)
  
  #2. space
  updated_list_of_codes <- trimws(updated_list_of_codes, which = c("both"), whitespace = "[ \t\r\n]")
  
  #3.decimal
  updated_list_of_codes <- gsub("\\.","",updated_list_of_codes)
  updated_list_of_codes <- gsub("[[:space:]]", "", updated_list_of_codes) #after\\. might resulting in sapce
  
  #Check the number of charter for each code
  n_char <- NA
  for (c in 1:length(updated_list_of_codes)){
    cur_code <- updated_list_of_codes[c]
    n_char[c] <- nchar(cur_code)
  }
  
  #for codes less than 3 characters long
  #.if it is non-numeric, then exclude codes 
  # if it is numeric , then prepending '0'
  l3_idxes <- which(n_char< 3)
  l3_codes <- updated_list_of_codes[l3_idxes]
  if(length(l3_codes) > 0){
        updated_code <- NA
        for (c in 1:length(l3_codes)){
          cur_code <- l3_codes[c]
          if(is.na(as.numeric(cur_code)==T)){ #if NA, then it is non-numeric
            updated_code[c] <- NA #remove
          }else{ #it is numeric , then prepending '0'
            updated_code[c] <- paste0("0",cur_code)
          }
        }
        
    updated_list_of_codes[l3_idxes] <- updated_code
    
    #Then Remove the NAs(from converting to numeric when it is char) from orignal 
    na_idxes <- which(is.na(updated_list_of_codes==T))
    if (length(na_idxes) > 0){
      updated_list_of_codes <- updated_list_of_codes[-na_idxes]
    }
  }
  
  return(updated_list_of_codes)
}
