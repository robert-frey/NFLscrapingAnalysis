library(tidyverse)

nfl_combine_scrape <- function(year) {
  
  url <- paste0("https://pro-football-reference.com/draft/",year,"-combine.htm")
  
  url %>% xml2::read_html() %>%
    rvest::html_nodes("#combine") %>%
    rvest::html_table(trim=T) %>%
    as.data.frame(stringsAsFactors = F) %>%
    dplyr::filter(Pos != "Pos")
  
}

nfl_combine_scrape(2021)
