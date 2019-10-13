# Create a cool bar chart animation of words over time

rm(list = ls())
auf::packages('tidyverse', 'gganimate')
theme_set(theme_bw())

# Load in final data file
dat = readRDS(file = 'arxiv_scrape_20191011.rds')

# Create new date variables and filter out older years
dat = dat %>% mutate(year = year(dates),
                     ym = floor_date(dates, 'month')) %>% 
  filter(year <= max(year), year >= 2008)

# Get the words
words = dat %>%
  select(year, ym, titles) %>%
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
  count(year, word, sort = TRUE)

# Want to normalise the counts and then filter
set.seed(123)
words2 = words %>% 
  filter(word != 'learning') %>% 
  group_by(year) %>% 
  mutate(norm_freq = round(n/sum(n)*10000)) %>% 
  arrange(year,(desc(norm_freq))) %>% 
  slice(1:10) %>% 
  ungroup() %>% 
  mutate(ordering = rep(10:1, 12)) %>% 
  select(-n)

words2 %>%
  mutate(word = reorder(word, ordering)) %>% 
  ggplot(aes(x = word, 
             y = norm_freq, 
             fill=norm_freq)) + 
  geom_bar(stat = "identity") +
  scale_colour_gradientn(colors = terrain.colors(10)) + 
  labs(x = 'Word', 
       y = 'Count',
       title = 'Top 10 words by year in Stat.ML') +
  xlab("Terms") + ylab("Count") + 
  coord_flip() + 
  theme(legend.position = 'None') + 
  facet_wrap(year ~ ., scales = "free_y")
saveRDS(words2, file = 'words2.rds')

my_font = "Helvetica"
anim_bar = ggplot(words2, aes(ordering, group = word)) +
  geom_tile(aes(y = norm_freq / 2, 
                height = norm_freq,
                width = 0.9,
                fill =norm_freq), alpha = 0.9) +
  geom_text(aes(y = 0, label = word), 
            family= my_font, size = 5, #nudge_y = -5, 
            colour = 'white',hjust = 0) +
  geom_text(aes(x=1.1,y=200, label=paste0(year)),
            family=my_font, size=8, color = 'gray45') +
  #coord_cartesian(clip = "off", expand = FALSE) +
  coord_flip(clip = "off", expand = FALSE) +
  labs(#title = 'Top 10 stat.ML terms by year',
       #subtitle = 'frequency normalised',
       caption = 'based on plot by @emilykuehler',
       x = '',
       y = 'Normalised frequency by year') +
  theme(legend.position = 'None',
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  transition_time(year) +
  ease_aes('cubic-in-out', interval = 0.01)
