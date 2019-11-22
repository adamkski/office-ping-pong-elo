# calculate other interesting stats

# calculate odds of winning
match_ups <- cross_df(list(players1 = players, players2 = players)) 

match_ups %>% 
  arrange(players1, players2) %>% 
  left_join(scores %>% select(-games_played), by = c("players1" = "player")) %>% 
  left_join(scores %>% select(-games_played), by = c("players2" = "player")) %>% 
  mutate(prob = if_else(players1 != players2, elo.prob(elo_points.x, elo_points.y), NA_real_),
         prob = round(prob, 3)) %>% 
  select(-contains("elo")) %>% 
  pivot_wider(names_from = "players2", values_from = "prob", names_prefix = "vs_") %>% 
  rename(probability_of_win = players1)  
write_xlsx(here("probability_of_win.xlsx"))


# check if order matters  
calc_scores <- function(){
  elo <<- tibble(player = players,
                 elo_points = 1000,
                 games_played = 0)
  
  games_played <- games_played %>% 
    sample_n(37)
  
  log <- map2(games_played$won, games_played$def, ~update_elo(.x, .y))
  
  scores <- elo %>% 
    group_by(player) %>% 
    filter(row_number(games_played) == n()) %>% 
    ungroup %>% 
    mutate(elo_points = round(elo_points)) 
  
  scores
}


calc_scores() %>% 
  arrange(-elo_points)


# compare to last day
games_played2 <- games_played
games_played <- games_played %>% filter(is.na((day)))
games_played <- games_played2

scores1 <- scores

scores1 %>% 
  left_join(scores, by = "player") %>% 
  arrange(-elo_points.y) %>% 
  select(-contains("games")) %>% 
  mutate(change = elo_points.y - elo_points.x) %>% 
  select(player, elo_points = elo_points.y, change) %>% 
  rowid_to_column("rank") %>%
  knitr::kable()
