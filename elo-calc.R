library(elo)
library(writexl)
library(tidyverse)
library(here)
options(encoding = 'UTF-8')
source(here("load_games.R"))
source(here("functions.R"))

# TODO once someone has played 20 games, he is ranked
# until he has, he loses / gains points as usual, but opponents don't
# k has three tiers depending on score

# setup players
players <- bind_rows(games_played %>% select(player = won),
                     games_played %>% select(player = def)) %>% 
  distinct(player) %>% 
  pull(player)

k <- 32

# compute elo scores
elo <- tibble(player = players,
              elo_points = 1000,
              games_played = 0)

log <- map2(games_played$won, games_played$def, ~update_elo(.x, .y))


scores <- elo %>% 
  group_by(player) %>% 
  filter(row_number(games_played) == n()) %>% 
  ungroup %>% 
  mutate(elo_points = round(elo_points)) 



# report ranking 
scores %>% 
  arrange(-elo_points) %>% 
  # select(-games_played) %>% 
  rowid_to_column("rank") %>% 
  select(-games_played) %>% 
  knitr::kable()



games_played %>% write_xlsx("games_played.xlsx")












