library(readr)
library(readxl)
library(dplyr)
library(tibble)
library(SnowballC)
library(stopwords)
library(stringr)
library(tidytext)
library(ggplot2)

questionnaire <- read_xlsx("data/Needs Survey 2.0_I_RIM against Covid-19_ITA (Risposte).xlsx", col_names = TRUE)

names_old <- names(questionnaire)

names(questionnaire) <- str_c("domanda", paste0("_", 1:ncol(questionnaire)))

questionnaire <- add_column(questionnaire, id = 1:nrow(questionnaire), .before = 1)

questionnaire$id <- 1:nrow(questionnaire)

dim(questionnaire)

subset_unique <- (unique(questionnaire[, seq(4, 29)]))

questionnaire[, seq(3, 15)]

library(tidytext)

stop_words <- stopwords(language = "it")

for (column in 4:ncol(questionnaire)) { #column <- 4
  
  print(column)
  
  unigram <- questionnaire %>%
    select(id, column) %>%
    rename(domanda = 2) %>%
    filter(!is.na(domanda)) %>%
    unnest_tokens(unigram, domanda, token = "ngrams", n = 1) %>%
    filter(!(unigram %in% stop_words)) %>%
    count(unigram, sort = TRUE)
  
  bigram <- questionnaire %>%
    select(id, column) %>%
    rename(domanda = 2) %>%
    filter(!is.na(domanda)) %>%
    unnest_tokens(bigram, domanda, token = "ngrams", n = 2) %>%
    filter(!is.na(bigram)) %>%
    count(bigram, sort = TRUE)
  
  trigram <- questionnaire %>%
    select(id, column) %>%
    rename(domanda = 2) %>%
    filter(!is.na(domanda)) %>%
    unnest_tokens(trigram, domanda, token = "ngrams", n = 3) %>%
    filter(!is.na(trigram)) %>%
    count(trigram, sort = TRUE)
  
  quadgram <- questionnaire %>%
    select(id, column) %>%
    rename(domanda = 2) %>%
    filter(!is.na(domanda)) %>%
    unnest_tokens(quadgram, domanda, token = "ngrams", n = 4) %>%
    filter(!is.na(quadgram)) %>%
    count(quadgram, sort = TRUE)
  
  file_name <- names_old[column - 1] %>%
    str_replace_all("\\?|/", "_")
  
  # get first 200 elements
  df <- unigram[1:200, ] %>%
    bind_cols(bigram[1:200, ]) %>%
    bind_cols(trigram[1:200, ]) %>%
    bind_cols(quadgram[1:200, ]) %>%
    write_csv2(path = paste0("./data_ngram/", column, "_", file_name, ".csv"))
    
}


library(udpipe)

udpipe_italian <- udpipe_download_model(language = "italian")

x <- udpipe(x = questionnaire$domanda_6, object = udpipe_italian)

x %>%
  filter(upos == "VERB")






