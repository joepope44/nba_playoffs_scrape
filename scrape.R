library(tidyverse)
library(rvest)

# Base url string for playoff data on basketball-reference.com 
url <- "https://www.basketball-reference.com/playoffs/NBA_"

# Create list of urls from 2010 to 2019
list_of_urls <- str_c(url, 2010:2019, ".html")  

# Initialise a list
repo = list()

for (j in seq_along(list_of_urls)) {
  
  # Initialize the url 
  url = list_of_urls[[j]]
  message(str_c("#", j, ": Pulling ", url, "..."))
  
  # Finals, Conference and Round info 
  game_titles <- url %>% 
    read_html() %>% 
    html_nodes("#all_playoffs strong") %>% 
    html_text()
  
  # Pull html table info 
  # Clean and create game and round number info 
  # Arrange df and store to repo
  repo[[j]] <- url%>% 
    read_html() %>% 
    html_nodes(".stats_table table") %>% 
    html_table() %>% 
    tibble() %>% 
    mutate(round = game_titles) %>% 
    unnest(cols = c(round, .)) %>% 
    rename(
      game = X1,
      game_date = X2, 
      away_team = X3, 
      away_score = X4, 
      home_team = X5, 
      home_score = X6
    ) %>% 
    mutate(
      url = url,
      round_num = 
        case_when(
          str_detect(round, "First") ~ 1, 
          str_detect(round, "Semifinals") ~ 2,
          str_detect(round, "Conference Finals") ~ 3,
          TRUE ~ 4
          ),
      game_num = str_extract(game, "[:digit:]"),
      home_team = str_remove(home_team, "@ ")
      ) %>% 
    select(round, round_num, game, game_num, everything()) %>% 
    arrange(round_num) 
}

# Convert list of dataframes into one dataframe
# Parse year of playoffs from url 
final_df <- repo %>% 
  bind_rows() %>% 
  mutate(year = str_extract(url, "\\d+")) %>% 
  select(year, everything()) 

write_csv(path = "nba_playoffs.csv", x = final_df)