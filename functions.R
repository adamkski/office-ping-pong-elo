transform_points <- function(elo_points) {
  10 ^ (elo_points / 400)
}

get_current_elo <- function(p1, p2) {
  elo %>% 
    filter(player %in% c(p1, p2)) %>% 
    group_by(player) %>% 
    filter(rank(desc(games_played)) == 1) %>% 
    ungroup()
}

add_result <- function(current_elo, won, def) {
  current_elo %>% 
    mutate(elo_transform = transform_points(elo_points),
           expected_score = elo_transform / sum(elo_transform),
           actual_score = if_else(player == won, 1, 0),
           elo_points = elo_points + k * (actual_score - expected_score),
           games_played = games_played + 1) %>% 
    select(player, elo_points, games_played)
}

.update_elo <- function(won, def){
  current_elo <- get_current_elo(won, def)
  elo <<- bind_rows(elo, add_result(current_elo, won, def))
}

update_elo <- quietly(.update_elo)