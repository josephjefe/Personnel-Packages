library(nflreadr)
library(dplyr)
library(tidyr)

# Get latest available game

latest_game <- load_ftn_charting() |>
  filter(ftn_game_id == max(ftn_game_id, na.rm = TRUE)) |>
  slice_head(n = 1) |>
  select(ftn_game_id, nflverse_game_id) |>
  separate(
    nflverse_game_id,
    into = c("year", "week", "away_team", "home_team"),
    sep = "_"
  ) |>
  mutate(
    year = as.integer(year),
    week = as.integer(week)
  )

saveRDS(
  latest_game,
  "Data/latest_game.rds"
)
