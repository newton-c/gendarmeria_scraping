library(tidyverse)
library(rvest)
#library(ICplots)
#library(purrr)
library(furrr) # multicore maps
library(parallel) # for detectCores()

plan(multisession, workers = detectCores() - 1) # use all but one core

url <- "https://www.argentina.gob.ar/gendarmeria/noticias"
max_pages <- read_html(url) %>%
  html_element(".pager-last") %>%
  html_element("a") %>%
  html_attr("href") %>%
  sub(".*page=", "", .)

get_links <- function(i) {
  if (i == 1) {
    #url <- "https://www.argentina.gob.ar/gendarmeria/noticias"
    links <- read_html(url) %>%
      html_elements("a") %>%
      html_attr("href") %>%
      unlist %>%
      str_match("noticias/.*")
      
    links <- links[!is.na(links)]
  } else {
    page_num <- i - 1 # page 2 starts ?page=1 so each url is 1 less than page
    url <- paste0("https://www.argentina.gob.ar/node/34987/noticias?page=",
                  page_num)
    links <- read_html(url) %>%
      html_elements("a") %>%
      html_attr("href") %>%
      unlist %>%
      str_match("noticias/.*")
    
    links <- links[!is.na(links)]
  }
  return(links)
}
    
  

# scraping the news site to get the articles
#all_links <- map(seq_len(227), get_links) %>%
#  unlist()
all_links <- future_map(seq_len(as.numeric(max_pages)), get_links) %>%
  unlist()

# getting the press releases
get_article <- function(i) {
  base <- "https://www.argentina.gob.ar/"
  url <- paste0(base, all_links[i])
  
  page_html <- read_html(url)
  
  get_title <- page_html %>%
    html_elements("h1") %>%
    html_text2()
  
  get_title <- get_title[2] %>%
    unlist() %>%
    as_tibble() %>%
    rename(title = value)

  get_body <- page_html %>%
    html_elements(".pane-content") %>%
    html_text2()
  
  get_body <- get_body[2] %>%
    unlist() %>%
    as_tibble() %>%
    rename(text = value) %>%
    mutate(article_url = url)
  
  get_date <- page_html %>%
    html_element("time") %>%
    html_attr("datetime") %>%
    unlist() %>%
    as_tibble() %>%
    rename(date = value) %>%
    mutate(date = as.POSIXlt(date))

  combined_data <- as_tibble(cbind(get_date, get_title, get_body))
}

# extract date, url, and title for a dataset
all_articles <- future_map_dfr(seq_along(all_links), get_article) 
 
# saving with today's date in the file name
file_name <- paste0("data/gendarmeria_", Sys.Date(), ".csv")
write_excel_csv(all_articles, file_name)
