library(tidyverse)
library(ineq)





to_binary <- function(df,document="doc_id",text="acc_text",preserve_spaces=F) {
  
  vowels <-  c("а", "е","ё", "у", "о", "ю", "я", "и", "ы", "э")
  #cons <- c("б", "в", "г", "д", "ж", "з", "й", "к", "л", "м", "н", "п", "р", "с", "т", "ф", "х", "ц", "ч", "ш", "щ")
  
  syl_pattern = paste(vowels, collapse="|")
  #cons_pattern <- paste(cons, collapse="|")
  
  ## send to lower case, split by lines, add row ids
  unnested <- df %>% mutate(acc_text = str_to_lower(!! sym(text)),
                           acc_text = str_replace_all(acc_text,"ё", "ё'"),
                           acc_text = str_split(acc_text, "\n")) %>%
    unnest(acc_text) %>% 
    mutate(row_id=row_number())
  
  
  if(preserve_spaces) {
  binarized <- unnested %>%
    # replace all vowels with 0 and extract only 0 and 0' patterns
    mutate(stress_pattern = str_replace_all(acc_text, syl_pattern,"0"),
           stress_pattern = str_extract_all(stress_pattern, "0|'| "))
  } else {
    binarized <- unnested %>%
    # replace all vowels with 0 and extract only 0 and 0' patterns
    mutate(stress_pattern = str_replace_all(acc_text, syl_pattern,"0"),
           stress_pattern = str_extract_all(stress_pattern, "0|'"))
  }
  binarized <- binarized %>%  
    # unnest extracted lists
    unnest(stress_pattern) %>% 
    group_by(row_id, !! sym(document)) %>%
    summarise(stress_pattern=paste(stress_pattern, collapse="")) %>%
    mutate(stress_pattern = str_replace_all(stress_pattern, "0'", "1"))
    
  
  return(binarized)
}


extract_intervals <- function(binary_df,no_anacrusis=F,no_clausula=T) {
  
  # drop preceding unstressed syllables
  if(no_anacrusis) {
    binary_df <- binary_df %>% mutate(intervals = str_remove(stress_pattern, "^0*"))
  }
  
  # drop postceding unstressed syllables
  if(no_clausula) {
    binary_df <- binary_df %>% mutate(intervals = str_remove(stress_pattern, "0*$"))
  }
  
  
  intervals_unnested <- binary_df %>% mutate(no_stress = str_extract_all(intervals, "0{1,15}|(11)")) %>%
    group_by(row_id) %>%
    unnest(no_stress)
  
  return(intervals_unnested)
  
}

rhythm_inequality <- function(intervals, raw_values=F,drop_rare=T,document="doc_id") {
  
  if(drop_rare) {
    intervals <- intervals %>% mutate(no_stress = ifelse(nchar(no_stress) > 7, "0000000+", no_stress))
  }
  
  table_sum <- intervals %>%
    group_by(!! sym(document)) %>%
    count(no_stress) %>%
    mutate(n=n/sum(n)) #%>%
  

  
  if(raw_values) {
    return(table_sum)
  } else {
    matrix <- table_sum %>%
      pivot_wider(names_from=no_stress,values_from=n,values_fill=0)
    
    names = matrix[,1]
    intervals_matrix = matrix[,-1] %>% as.matrix()
    rownames(intervals_matrix) = names[,1]
    

    
    gini <- apply(intervals_matrix, 1, ineq)
    tb <- tibble(poem_id = names(gini), gini = gini)
    
    return(tb)
  }
}


hill <- function(p, q=c(0.5,1,1.5,2)) {
  
  if(1 %in% q) {
    q[which(q == 1)] <- 0.99999
  }
  
  hills <- sapply(q, function(q) {
    hill_v <- sum(p^q)^(1/(1-q))
  })
  return(list(hills))
}
