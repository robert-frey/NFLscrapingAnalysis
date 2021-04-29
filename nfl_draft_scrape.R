library(tidyverse)
library(rvest)
library(janitor)

draft_url <- data.frame(year = c(2000:2020), stringsAsFactors = F) %>%
  dplyr::mutate(url = paste0("https://www.pro-football-reference.com/years/",years,"/draft.htm"))

draft_scrape <- function(url) {
  data <- url %>% xml2::read_html() %>% rvest::html_nodes("#drafts") %>%
    rvest::html_table(trim=T) %>% .[[1]] %>%
    janitor::row_to_names(row_number = 1)
  
  return(data)
}


draft_data <- 1:nrow(draft_url) %>% purrr::map_df(function(x) draft_scrape(draft_url$url[[x]]))

draft_data <- draft_data %>% dplyr::filter(Tm != "Tm") %>%
  dplyr::mutate(Tm = case_when(Tm == "SDG" ~ "LAC",
                               Tm == "OAK" ~ "LVR",
                               Tm == "STL" ~ "LAR",
                               TRUE ~ Tm))

draft_data %>% filter(Rnd == 1) %>% group_by(Tm,Pos) %>% 
  summarise(num_picks = n()) %>%
  ungroup() %>% arrange(-num_picks)
