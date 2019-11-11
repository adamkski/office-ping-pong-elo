library(elo)
library(tidyverse)
library(here)
source(here("load_games.R"))
source(here("functions.R"))

# TODO once someone has played 20 games, he is ranked
# until he has, he loses / gains points as usual, but opponents don't
# k has three tiers depending on score

k <- 32
elo <- tibble(player = c("Sujoy","Darren","Brian","Gabe","Shibo","Sandy", "Adam", "Justin"),
              elo_points = 1000,
              games_played = 0)

log <- map2(games_played$won, games_played$def, ~update_elo(.x, .y))

elo %>% 
  group_by(player) %>% 
  filter(row_number(games_played) == n()) %>% 
  ungroup %>% 
  mutate(elo_points = round(elo_points)) %>% 
  arrange(-elo_points) %>% 
  # select(-games_played) %>% 
  rowid_to_column("rank") %>% 
  knitr::kable()
