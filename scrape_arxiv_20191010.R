# Scrape arxiv category Stat.ml 
# Web URL is https://arxiv.org/list/stat.ML/recent

# Boiler plate code
rm(list = ls())
auf::packages('tidyverse', 'stringr', 'RCurl', 'rjson', 'progress',
              'RJSONIO', 'rvest', 'xml2')

# Get today's date
today = format(Sys.time(), "%Y%m%d%H")

# Get functions
my_fun1 = function(x) x$published[[1]]
my_fun2 = function(x) x$title[[1]]
my_fun3 = function(x) x$summary[[1]]
my_fun4 = function(x) x$author$name[[1]]

# Set up waiting paramters
results_per_iteration = 1000 # results at a time
wait_time = 3

#url = "http://export.arxiv.org/api/query?search_query=all:electron"
#url = "http://export.arxiv.org/api/query?search_query=au:del_maestro+AND+ti:checkerboard"

all_good = TRUE
count = 32
while(all_good) {
  print(count)
  
  url = paste0("http://export.arxiv.org/api/query?search_query=cat:stat.ML&start=",results_per_iteration*count,"&max_results=",results_per_iteration,"&sortBy=submittedDate&sortOrder=descending")
  
  # Load the page and strip out the crazy namespaces
  page = read_xml(url) %>% xml_ns_strip() %>% as_list()
  
  # Extract out the entries
  which_entry = which(names(page$feed) == 'entry')
  if(length(which_entry)==0) {
    print('BAD!')
    Sys.sleep(10)
    page = read_xml(url) %>% xml_ns_strip() %>% as_list()
    which_entry = which(names(page$feed) == 'entry')
    if(is.null(which_entry)) next
  }
  entries = page$feed[which_entry]
  # Get date published
  dates = sapply(entries, my_fun1) %>% 
    parse_datetime(format = "%Y-%m-%dT%H:%M:%SZ",
                   locale = locale(tz = 'GB'))
  
  # Get title
  titles = sapply(entries, my_fun2)
  
  # Get summaries
  summaries = sapply(entries, my_fun3)
  
  # Get lead authors
  authors = sapply(entries, my_fun4)
  
  # Sort data frame
  all_data = data.frame(
    dates = dates,
    titles = titles,
    summaries = summaries,
    authors = authors,
    stringsAsFactors = FALSE
  )
  print(range(all_data$dates))
  
  saveRDS(all_data, file = paste0('arxiv_statML_new_',count,'.rds'))  
  
  # Increment count
  count = count + 1
  
  # See where we are
  print(dates[1])
  
  # Sleep for 3 seconds
  Sys.sleep(wait_time)
}


# Test code ---------------------------------------------------------------

# Test whether it worked
#news = readRDS(paste0('news',2019041716,'.rds'))
#tibble::glimpse(news)

# auf::packages('tidyverse')
# ggplot(news, aes(x = site, y = nchar(head))) +
#   geom_boxplot() +
#   coord_flip()

