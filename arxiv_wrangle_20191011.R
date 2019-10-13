# Merge all the scrapes together

# Boiler plate
rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest', 'xml2')

# Load in all files
all_files = list.files('scrape')
n_files = length(all_files)
file_holder = vector('list', length = n_files)
for(i in 1:n_files) {
  file_holder[[i]] = readRDS(paste0('scrape/',all_files[i]))
}
final_dat_raw = do.call(rbind, file_holder)

# Remove dodgy dates
final_dat_raw = final_dat_raw %>% 
  filter(year(dates) > 2000)

# De-duplicate
final_dat = final_dat_raw %>% distinct()
glimpse(final_dat)

saveRDS(final_dat, file = 'arxiv_scrape_20191011.rds')
