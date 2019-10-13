# Create a words by year data set for use in the Shiny app

# Boiler plate
rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest', 'xml2', 'lubridate', 'ggwordcloud',
              'tm', 'tau', 'viridis', 'tidytext', 'corpus', 
              'topicmodels', 'wordcloud2', 'SnowballC', 'ldatuning',
              'gganimate')
theme_set(theme_bw())

# Load in final data file
dat = readRDS(file = 'arxiv_scrape_20191011.rds')

words = dat %>%
  mutate(year = year(dates)) %>% 
  select(titles, year) %>%
  unnest_tokens(word, titles) %>%
  anti_join(stop_words) %>% # Remove stop words
  mutate(word = str_replace(word,
                            pattern = "[[:digit:]]", 
                            replacement = ' '),
         # removes any words with numeric digits
         word = str_replace(word,
                            pattern = "[[:punct:]]", 
                            replacement = ' '),
         # removes any remaining punctuations
         word = str_replace(word,
                            pattern = "(.)\\1{2,}", 
                            replacement = ' '),
         # removes any words with 3 or more repeated letters
         word = str_replace(word,
                            pattern = "\\b(.)\\b", 
                            replacement = ' ')
         # removes any remaining single letter words
  ) %>% 
  # mutate(word = corpus::text_tokens(word, 
  #                                   stemmer = "en") %>% 
  #          unlist()) %>% # add stemming process
  filter(word != 'learning') %>%
  count(year, word, sort = TRUE) %>% 
  group_by(year) %>% 
  mutate(norm_freq = round(n*1000/max(n))) %>% 
  ungroup()

# Save for plotting over time
words %>% select(-norm_freq) %>% 
  saveRDS(file = 'words_20191013.rds')

# Save lighter version for word clouds
words %>% filter(norm_freq > 100) %>% 
  saveRDS(file = 'words_by_year_20191013.rds')

# Save lighter version of data set
dat = readRDS(file = 'arxiv_scrape_20191011.rds') %>% 
  mutate(year = year(dates),
         titles = tolower(titles)) %>% 
  select(titles, year) %>%
  mutate(titles = str_replace(titles,
                            pattern = "[[:digit:]]", 
                            replacement = ' '),
         # removes any words with numeric digits
         titles = str_replace(titles,
                            pattern = "[[:punct:]]", 
                            replacement = ' '),
         # removes any remaining punctuations
         titles = str_replace(titles,
                            pattern = "(.)\\1{2,}", 
                            replacement = ' '),
         # removes any words with 3 or more repeated letters
         titles = str_replace(titles,
                            pattern = "\\b(.)\\b", 
                            replacement = ' ')
         # removes any remaining single letter words
  ) %>% 
  saveRDS(file = 'scrape_clean_20191013.rds')

