rdBL <- readLines(conBL, warn=FALSE, encoding="UTF-8", skipNul = TRUE)
rdNW <- readLines(conNW, warn=FALSE, encoding="UTF-8", skipNul = TRUE)
rdTW <- readLines(conTW, warn=FALSE, encoding="UTF-8", skipNul = TRUE) 

#rdBL <- data_frame(text = rdBL)
rdBL <- tibble(text = rdBL)
rdNW <- tibble(text = rdNW)
rdTW <- tibble(text = rdTW)

set.seed(260421)

dtBL_sample <- rdBL %>% 
  sample_n(., nrow(rdBL)*0.25) 
dtNW_sample <- rdNW %>% 
  sample_n(., nrow(rdNW)*0.25) 
dtTW_sample <- rdTW %>% 
  sample_n(., nrow(rdTW)*0.25) 


combined_sample1 <- bind_rows(mutate(dtBL_sample, source = "blogs"),
                             mutate(dtNW_sample,  source = "news"),
                             mutate(dtTW_sample, source = "twitter")) 
combined_sample1$source <- as.factor(combined_sample1$source)

length(combined_sample1)

#' ## Clean the data 
#' Create filters: stopwords, profanity, non-alphanumeric's, url's, repeated letters(+3x) 
#+ DataCleaning 
#data("stop_words") 
#swear_words <- read_delim("./data/final/en_US/swearWords.csv", delim = "\n", col_names = FALSE) 
#swear_words <- unnest_tokens(swear_words, word, X1) 
#replace_reg <- "[^[:alpha:][:space:]]*" 
#replace_url <- "http[^[:space:]]*" 
#replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"   

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words, 
#' and ngrams. 
clean_sample1 <-  combined_sample1 %>% 
  mutate(text = str_replace_all(text, replace_reg, "")) %>% 
  mutate(text = str_replace_all(text, replace_url, "")) %>% 
  mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
  mutate(text = iconv(text, "UTF-8")) 

## Create all n-grams   
#' Quadgrams   
quadgram_repo <- clean_sample1  %>% 
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) 

