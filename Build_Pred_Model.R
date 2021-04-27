library(dplyr)
library(R.utils)
library(stringi)
library(tm)
library(RWeka)
library(ggplot2)
library(wordcloud2)

library(tidytext) 
library(tidyverse) 
library(stringr) 
library(knitr) 
library(ngram) 


#Set Path file names
dataPath <- "Data/final/en_US"
infoBL <- paste(dataPath, "en_US.blogs.txt", sep = "/")
infoNW <- paste(dataPath, "en_US.news.txt", sep = "/")
infoTW <- paste(dataPath, "en_US.twitter.txt", sep = "/")


#Open connection and Read Data
conBL <- file(file.path(paste(getwd(), infoBL, sep = "/")), "r") 
conNW <- file(file.path(paste(getwd(), infoNW, sep = "/")), "r") 
conTW <- file(file.path(paste(getwd(), infoTW, sep = "/")), "r") 

rdBL <- readLines(conBL, warn=FALSE, encoding="UTF-8", skipNul = TRUE)
rdNW <- readLines(conNW, warn=FALSE, encoding="UTF-8", skipNul = TRUE)
rdTW <- readLines(conTW, warn=FALSE, encoding="UTF-8", skipNul = TRUE) 

#rdBL <- data_frame(text = rdBL)
rdBL <- tibble(text = rdBL)
rdNW <- tibble(text = rdNW)
rdTW <- tibble(text = rdTW)

set.seed(210421)

#dtBL_sample <- sample(rdBL1, length(rdBL1)*.05)
#dtNW_sample <- sample(rdNW, length(rdNW)*.05)
#dtTW_sample <- sample(rdTW, length(rdTW)*.05)

dtBL_sample <- rdBL %>% 
     sample_n(., nrow(rdBL)*0.5) 
dtNW_sample <- rdNW %>% 
  sample_n(., nrow(rdNW)*0.5) 
dtTW_sample <- rdTW %>% 
  sample_n(., nrow(rdTW)*0.5) 


#combined_sample <- c(dtBL_sample, dtNW_sample, dtTW_sample)
#combined_sample <- iconv(combined_sample, "UTF-8","ASCII", sub="")


combined_sample <- bind_rows(mutate(dtBL_sample, source = "blogs"),
                         mutate(dtNW_sample,  source = "news"),
                         mutate(dtTW_sample, source = "twitter")) 
combined_sample$source <- as.factor(combined_sample$source)

length(combined_sample)

#Clear memory by removing unwanted dataframes
#rm(dtBL_sample, dtNW_sample, dtTW_sample, rdBL, rdNW, rdTW)
rm(dtBL_sample, dtNW_sample, rdBL, rdNW, rdTW)

#' ## Clean the data 
#' Create filters: stopwords, profanity, non-alphanumeric's, url's, repeated letters(+3x) 
#+ DataCleaning 
data("stop_words") 
swear_words <- read_delim("./data/final/en_US/swearWords.csv", delim = "\n", col_names = FALSE) 
swear_words <- unnest_tokens(swear_words, word, X1) 
replace_reg <- "[^[:alpha:][:space:]]*" 
replace_url <- "http[^[:space:]]*" 
replace_aaa <- "\\b(?=\\w*(\\w)\\1)\\w+\\b"   

#' Clean the sample. Cleaning is separted from tidying so `unnest_tokens` function can be used for words, 
#' and ngrams. 
clean_sample <-  combined_sample %>% 
mutate(text = str_replace_all(text, replace_reg, "")) %>% 
mutate(text = str_replace_all(text, replace_url, "")) %>% 
mutate(text = str_replace_all(text, replace_aaa, "")) %>%  
mutate(text = iconv(text, "UTF-8")) 

## Create all n-grams   

#' Unigrams   
tidy_repo <- clean_sample %>% 
  unnest_tokens(word, text) %>% 
  anti_join(swear_words) %>% 
  anti_join(stop_words) 
 

#' Bigrams   
bigram_repo <- clean_sample  %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) 
 
#' Trigrams   
trigram_repo <- clean_sample  %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3) 
 

#' Quadgrams   
quadgram_repo <- clean_sample  %>% 
  unnest_tokens(quadgram, text, token = "ngrams", n = 4) 
 
#' ## Reduce n-grams to top 50% of CDF   
#' Unigram upper half 
unigram_50 <- tidy_repo %>% 
   count(word) %>%   
   mutate(proportion = n / sum(n)) %>% 
   arrange(desc(proportion)) %>%   
   mutate(coverage = cumsum(proportion)) %>% 
   filter(coverage <= 0.5) 

#' Bigram upper half 
bigram_50 <- bigram_repo %>% 
   count(bigram) %>%   
   mutate(proportion = n / sum(n)) %>% 
   arrange(desc(proportion)) %>%   
   mutate(coverage = cumsum(proportion)) %>% 
   filter(coverage <= 0.5) 
 

#' Trigram upper half 
trigram_50 <- trigram_repo %>% 
   count(trigram) %>%   
   mutate(proportion = n / sum(n)) %>% 
   arrange(desc(proportion)) %>%   
   mutate(coverage = cumsum(proportion)) %>% 
   filter(coverage <= 0.5) 

#' Quadgram upper half 
quadgram_50 <- quadgram_repo %>% 
   count(quadgram) %>%   
   mutate(proportion = n / sum(n)) %>% 
   arrange(desc(proportion)) %>%   
   mutate(coverage = cumsum(proportion)) %>% 
   filter(coverage <= 0.5) 

quadgram_cover <- quadgram_repo %>% 
 count(quadgram) %>%   
 filter(n > 10) %>% 
 arrange(desc(n))   

rm(list = c("quadgram_repo")) 

## Separate words 
bi_words <- bigram_50 %>% 
   separate(bigram, c("word1", "word2"), sep = " ") 
bi_words 
 

tri_words <- trigram_50 %>% 
   separate(trigram, c("word1", "word2", "word3"), sep = " ") 
tri_words 
 

quad_words <- quadgram_50 %>% 
 separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") 
quad_words 

quad_words1 <- quadgram_cover %>% 
  separate(quadgram, c("word1", "word2", "word3", "word4"), sep = " ") 
  quad_words 


#' Save separated words for prediction 
saveRDS(bi_words, "./Data/bi_words.rds") 
saveRDS(tri_words, "./Data/tri_words.rds") 
saveRDS(quad_words, "./Data/quad_words.rds") 
saveRDS(quad_words1, "./Data/quad_words_fast.rds") 

#' ## Clear workspace, time load 
# rm(list= ls()) 

