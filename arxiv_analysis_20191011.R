# Analyse the scrapes. Tasks are:
# 1. Number of submissions by day/month/year
# 2. Word cloud of titles
# 3. Changing trends of various terms
# 4. Topic models
# 5. Top authors and their locations
# 6. What's the most fashionable title
# 

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

# 1. Number of submissions by day/month/year ------------------------------

dat = dat %>% mutate(year = year(dates),
                     ym = floor_date(dates, 'month'))
year_range = with(dat, 2008:max(year))

# By year
dat %>% 
  ggplot(aes(x = year,
             fill=..count..)) + 
  geom_bar() + 
  scale_x_continuous(breaks = year_range, limits = c(2007.5,2019.5)) + 
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = 'None') + 
  labs(x = 'Year', 
       y = 'Number of papers')
ggsave(file = 'stat_ml_by_year.png', width = 8, height = 4)

# By month
lims = as.POSIXct(strptime(c("2007-01-01","2019-10-01"), 
                            format = "%Y-%m-%d"))    
dat %>% 
  ggplot(aes(x = ym,
             fill=..count..)) + 
  geom_bar() + 
  scale_x_datetime(date_labels = "%b%y", date_breaks = '6 months',
                   limits = lims, expand = c(0,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = 'None') + 
  labs(x = 'Date', 
       y = 'Number of papers',
       title = 'Papers submitted to Stat.ML by month')


# 2 Word cloud of titles --------------------------------------------------

# Use tidytext to get this all working 
# http://uc-r.github.io/creating-text-features

words = dat %>%
  select(titles) %>%
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
  count(word, sort = TRUE)

# Plot frequency
words %>% filter(n>1000) %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(x = word, 
             y = n, 
             fill=n)) + 
  geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + 
  labs(x = 'Word', 
       y = 'Count',
       title = 'Popular words appearning at least 1000 times in Stat.ML') +
  xlab("Terms") + ylab("Count") + coord_flip() + 
  theme(legend.position = 'None')

# Create a word cloud
set.seed(123)
stop()
#saveRDS(words, file = 'words_20191013.rds')
words_filt = words %>% 
  filter(n > 200) %>% 
  rename(freq = n)
#saveRDS(words_filt, file = 'all_statml_words.rds')
expr = wordcloud2(words_filt, 
           color=rep_len(viridis(10), nrow(words_filt)),
           fontFamily = 'Helvetica')
renderWordcloud2(expr)

# 2-grams -----------------------------------------------------------------

# Try doing this with 2-grams
# bigram_dat = dat %>%
#   unnest_tokens(bigram, titles, token = "ngrams", n = 2) %>%  
#   separate(bigram, c("word1", "word2"), sep = " ") %>%               
#   filter(
#     !word1 %in% stop_words$word, # remove stopwords from both words in bi-gram
#     !word2 %in% stop_words$word
#     ) %>% 
#   mutate(
#     word1 = str_replace(word1, pattern = "[[:digit:]]",
#                 replacement = ' '), # removes any words with numeric digits
#     word2 = str_replace(word2, pattern = "[[:digit:]]",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "[[:punct:]]",
#                 replacement = ' '), # removes any remaining punctuations
#     word2 = str_replace(word2, pattern = "[[:punct:]]",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "(.)\\1{2,}",
#                 replacement = ' '),  # removes any words with 3 or more repeated letters
#     word2 = str_replace(word2, pattern = "(.)\\1{2,}",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "\\b(.)\\b",
#                 replacement = ' '),   # removes any remaining single letter words
#     word2 = str_replace(word2, pattern = "\\b(.)\\b",
#                 replacement = ' ')
#   ) %>%
#   unite("bigram", c(word1, word2), sep = " ") %>%
#   count(bigram) %>% 
#   arrange(desc(n))
# saveRDS(bigram_dat, file = 'bigram_20191012.rds')
bigram_dat = readRDS(file = 'bigram_20191012.rds')

# Plot frequency
bigram_dat %>% filter(n>200) %>% 
  mutate(bigram = reorder(bigram, n)) %>% 
  ggplot(aes(x = bigram, 
             y = n, 
             fill=n)) + 
  geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + 
  labs(x = 'bigram', 
       y = 'Count',
       title = 'Popular bigrams appearning at least 200 times in Stat.ML') +
  xlab("Terms") + ylab("Count") + coord_flip() + 
  theme(legend.position = 'None')

# Create a word cloud
bigram_filt = bigram_dat %>% 
  filter(n > 10) %>% 
  rename(freq = n)
wordcloud2(bigram_filt, 
           color=rep_len(viridis(10), nrow(bigram_filt)),
           fontFamily = 'Helvetica')

# trigrams -----------------------------------------------------------------

# trigram_dat = dat %>%
#   unnest_tokens(trigram, titles, token = "ngrams", n = 3) %>%  
#   separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
#   filter(
#     !word1 %in% stop_words$word,                 # remove stopwords from both words in bi-gram
#     !word2 %in% stop_words$word,
#     !word3 %in% stop_words$word) %>%
#   na.omit() %>% 
#   mutate(
#     word1 = str_replace(word1, pattern = "[[:digit:]]",
#                 replacement = ' '), # removes any words with numeric digits
#     word2 = str_replace(word2, pattern = "[[:digit:]]",
#                 replacement = ' '),
#     word3 = str_replace(word3, pattern = "[[:digit:]]",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "[[:punct:]]",
#                 replacement = ' '), # removes any remaining punctuations
#     word2 = str_replace(word2, pattern = "[[:punct:]]",
#                 replacement = ' '),
#     word3 = str_replace(word3, pattern = "[[:punct:]]",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "(.)\\1{2,}",
#                 replacement = ' '),  # removes any words with 3 or more repeated letters
#     word2 = str_replace(word2, pattern = "(.)\\1{2,}",
#                 replacement = ' '),
#     word3 = str_replace(word3, pattern = "(.)\\1{2,}",
#                 replacement = ' '),
#     word1 = str_replace(word1, pattern = "\\b(.)\\b",
#                 replacement = ' '),   # removes any remaining single letter words
#     word2 = str_replace(word2, pattern = "\\b(.)\\b",
#                 replacement = ' '),
#     word3 = str_replace(word3, pattern = "\\b(.)\\b",
#                 replacement = ' ')) %>%
#   unite("trigram", c(word1, word2, word3), sep = " ") %>%
#   count(trigram) %>% 
#   arrange(desc(n))
# saveRDS(trigram_dat, file = 'trigram_20191012.rds')
trigram_dat = readRDS(file = 'trigram_20191012.rds')

# Plot frequency
trigram_dat %>% filter(n>50) %>% 
  mutate(trigram = reorder(trigram, n)) %>% 
  ggplot(aes(x = trigram, 
             y = n, 
             fill=n)) + 
  geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + 
  labs(x = 'trigram', 
       y = 'Count',
       title = 'Popular trigrams appearning at least 50 times in Stat.ML') +
  xlab("Terms") + ylab("Count") + coord_flip() + 
  theme(legend.position = 'None')

# Create a word cloud
set.seed(123)
trigram_filt = trigram_dat %>% 
  filter(n > 2) %>% 
  rename(freq = n)
wordcloud2(trigram_filt, 
           color=rep_len(viridis(10), nrow(trigram_filt)),
           fontFamily = 'Helvetica')

# Find proportion of certain word over time -------------------------------

term = 'bayesian|bayes'
term_titles = dat %>%
  mutate(titles = tolower(titles),
         titles = str_replace(titles,
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
                            replacement = ' ')) %>%  # removes any remaining single letter words
         filter(stringr::str_detect(titles, term))
  
# Plot the rise of the term 'deep'
lims = as.POSIXct(strptime(c("2007-01-01","2019-10-01"), 
                           format = "%Y-%m-%d"))    
term_titles %>% 
  ggplot(aes(x = ym,
             fill=..count..)) + 
  geom_bar() + 
  scale_x_datetime(date_labels = "%b%Y", date_breaks = '6 months',
                   limits = lims, expand = c(0,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = 'None') + 
  labs(x = 'Date', 
       y = 'Number of papers',
       title = paste0("Papers submitted with ",term," in the title to Stat.ML by month"))

dat %>% 
  ggplot(aes(x = ym,
             fill=..count..)) + 
  geom_bar() + 
  geom_bar(data = term_titles, aes(x = ym), fill = 'red') + 
  scale_x_datetime(date_labels = "%b%Y", date_breaks = '6 months',
                   limits = lims, expand = c(0,0)) +
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) + 
  theme(legend.position = 'None') + 
  labs(x = 'Date', 
       y = 'Number of papers',
       title = 'Papers submitted to Stat.ML by month')


# What about the papers that don't mention deep learning ------------------

term = 'deep|neural'
filt_words = dat %>%
  mutate(titles = tolower(titles)) %>% 
  filter(!stringr::str_detect(titles, term)) %>% 
  select(titles) %>%
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
  count(word, sort = TRUE)

# Create nodeep wordcloud    
nodeep_filt = nodeep_words %>% 
  filter(n > 300) %>% 
  rename(freq = n)
wordcloud2(nodeep_filt, 
           color=rep_len(viridis(10), nrow(nodeep_filt)),
           fontFamily = 'Helvetica')

# Let's try a topic model -------------------------------------------------

# Going to run a topic model on the abstracts
word_counts = dat %>%
  select(titles, summaries) %>%
  unnest_tokens(word, summaries) %>%
  anti_join(stop_words) %>%
  mutate(word = wordStem(word),
         word = str_replace(word, 'rm', NA_character_),
         word = str_replace(word, 'mathbf', NA_character_),
         word = str_replace(word, 'learn', NA_character_)) %>% 
  na.omit() %>%
  count(titles, word, sort = TRUE)
# word_counts = dat %>%
#   mutate(titles_orig = titles) %>% 
#   select(titles_orig, titles) %>% 
#   unnest_tokens(word, titles) %>% 
#   anti_join(stop_words) %>%
#   mutate(word = wordStem(word),
#          word = str_replace(word, 'rm', NA_character_),
#          word = str_replace(word, 'mathbf', NA_character_),
#          word = str_replace(word, 'learn', NA_character_)) %>% 
#   na.omit() %>% 
#   count(titles_orig,word, sort = TRUE)
bigram_dat = dat %>%
  mutate(titles_orig = titles) %>% 
  unnest_tokens(bigram, titles, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(
    !word1 %in% stop_words$word, # remove stopwords from both words in bi-gram
    !word2 %in% stop_words$word
    ) %>%
  mutate(
    word1 = str_replace(word1, pattern = "[[:digit:]]",
                replacement = ' '), # removes any words with numeric digits
    word2 = str_replace(word2, pattern = "[[:digit:]]",
                replacement = ' '),
    word1 = str_replace(word1, pattern = "[[:punct:]]",
                replacement = ' '), # removes any remaining punctuations
    word2 = str_replace(word2, pattern = "[[:punct:]]",
                replacement = ' '),
    word1 = str_replace(word1, pattern = "(.)\\1{2,}",
                replacement = ' '),  # removes any words with 3 or more repeated letters
    word2 = str_replace(word2, pattern = "(.)\\1{2,}",
                replacement = ' '),
    word1 = str_replace(word1, pattern = "\\b(.)\\b",
                replacement = ' '),   # removes any remaining single letter words
    word2 = str_replace(word2, pattern = "\\b(.)\\b",
                replacement = ' ')
  ) %>%
  unite("bigram", c(word1, word2), sep = " ") %>%
  count(titles_orig, bigram) %>%
  arrange(desc(n))

# Turn into a dtm
dat_dtm <- bigram_dat %>%
  cast_dtm(titles_orig, bigram, n)

# Find the right number of topics
# result <- FindTopicsNumber(
#   dat_dtm,
#   topics = seq(from = 2, to = 15, by = 1),
#   metrics = c("Griffiths2004", "CaoJuan2009", "Arun2010", "Deveaud2014"),
#   method = "Gibbs",
#   control = list(seed = 77),
#   mc.cores = 2L,
#   verbose = TRUE
# )
# FindTopicsNumber_plot(result)

titles_lda <- LDA(dat_dtm, k = 20, control = list(seed = 1234))
titles_topics <- tidy(titles_lda, matrix = "beta")

# Look at which terms are in the LDA
top_terms <- titles_topics %>%
  group_by(topic) %>%
  top_n(10, beta) %>%
  ungroup() %>%
  arrange(topic, -beta)
# print(top_terms, n= 40)

# Do this better in a plot
top_terms %>%
  mutate(term = reorder_within(term, beta, topic)) %>%
  ggplot(aes(term, beta, fill = factor(topic))) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip() +
  scale_x_reordered()

# Look at which titles are in the LDA
titles_gamma <- tidy(titles_lda, matrix = "gamma")
titles_gamma

# Create a plot of the probabilities of the highest in each group
# titles_gamma %>%
#   mutate(title = reorder(document, gamma * topic)) %>%
#   ggplot(aes(factor(topic), gamma)) +
#   geom_boxplot() +
#   facet_wrap(~ title)
  
# Look at most likely membership
titles_classifications <- titles_gamma %>%
  group_by(topic) %>% 
  top_n(5, gamma) %>%
  arrange(topic,desc(gamma)) %>% 
  ungroup()
titles_classifications %>% filter(topic == 8) %>% 
  select(document) %>% 
  pull

# Compare word clouds across years ----------------------------------------

words = dat %>%
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
  count(year, word, sort = TRUE)

# Create a word cloud
set.seed(123)
words_filt = words %>% 
  filter(year == 2019) %>% #n > 10, 
  rename(freq = n) %>% 
  select(-year)
wordcloud2(words_filt, 
           color=rep_len(viridis(10), nrow(words_filt)),
           fontFamily = 'Helvetica')

  