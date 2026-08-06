library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)

# Data-----

# Get datasets:
players <- load_players() |>
  mutate(
    short_name = coalesce(
      short_name,
      paste0(str_sub(first_name, 1, 1), ".", last_name)
    )
  )

ftn_pbp_raw <- right_join(
  load_ftn_charting(),
  load_pbp(),
  by = c(
    "nflverse_game_id" = "game_id",
    "nflverse_play_id" = "play_id",
    "season",
    "week"
  )
)

# all22_raw is loaded by get_data.R

# Calculate all 22 and personnel packages
all22_off <- all22_raw |>
  select(
    ftn_game_id = game_id,
    ftn_play_id = play_id,
    all_of(paste0("off", 1:11, "id"))
  ) |>
  pivot_longer(
    cols = starts_with("off"),
    names_to = "off_position",
    values_to = "offense_id"
  ) |>
  filter(!is.na(offense_id)) |>
  left_join(
    select(players, gsis_id, short_name, position_group),
    by = c("offense_id" = "gsis_id")
  )

off_personnel <- all22_off |>
  filter(position_group %in% c("RB", "TE", "WR")) |>
  summarize(
    rb = sum(position_group == "RB"),
    te = sum(position_group == "TE"),
    wr = sum(position_group == "WR"),
    .by = c(ftn_game_id, ftn_play_id)
  ) |>
  mutate(offense_personnel = paste0("p_", rb, te, wr))

# Add all22 players to pbp data
off_ftn_pbp <- ftn_pbp_raw |>
  filter(pass == 1 | rush == 1, !is.na(epa), !is.na(ftn_play_id)) |>
  select(
    nflverse_game_id,
    ftn_game_id,
    nflverse_play_id,
    ftn_play_id,
    season,
    week,
    home_team,
    away_team,
    home_score,
    away_score,
    posteam,
    defteam,
    epa,
    success,
    pass
  ) |>
  left_join(
    select(off_personnel, ftn_game_id, ftn_play_id, offense_personnel),
    by = c("ftn_game_id", "ftn_play_id")
  ) |>
  filter(!is.na(offense_personnel))

off_personnel_teams <- off_ftn_pbp |>
  summarize(
    snaps = n(),
    epa_total = sum(epa, na.rm = TRUE),
    success_total = sum(success, na.rm = TRUE),
    pass_total = sum(pass, na.rm = TRUE),
    .by = c(nflverse_game_id, week, posteam, defteam, offense_personnel)
  ) |>
  mutate(
    team_snaps = sum(snaps),
    .by = c(nflverse_game_id, week, posteam)
  )

off_personnel_players <- off_ftn_pbp |>
  left_join(
    select(
      all22_off,
      ftn_game_id,
      ftn_play_id,
      offense_id,
      short_name,
      position_group
    )
  ) |>
  filter(position_group %in% c("RB", "TE", "WR")) |>
  summarize(
    snaps = n(),
    .by = c(
      nflverse_game_id,
      week,
      posteam,
      defteam,
      offense_personnel,
      offense_id,
      short_name,
      position_group
    )
  )

off_teams_plays <- off_personnel_teams |>
  pivot_wider(
    id_cols = c(
      nflverse_game_id,
      week,
      posteam,
      defteam
    ),
    names_from = offense_personnel,
    values_from = snaps,
    values_fill = 0
  )

off_teams_epa <- off_personnel_teams |>
  pivot_wider(
    id_cols = c(
      nflverse_game_id,
      week,
      posteam,
      defteam
    ),
    names_from = offense_personnel,
    values_from = epa_total,
    values_fill = 0
  )

off_teams_success <- off_personnel_teams |>
  pivot_wider(
    id_cols = c(
      nflverse_game_id,
      week,
      posteam,
      defteam
    ),
    names_from = offense_personnel,
    values_from = success_total,
    values_fill = 0
  )

off_teams_pass <- off_personnel_teams |>
  pivot_wider(
    id_cols = c(
      nflverse_game_id,
      week,
      posteam,
      defteam
    ),
    names_from = offense_personnel,
    values_from = pass_total,
    values_fill = 0
  )

# Save summarized datasets -----

write_csv(
  off_personnel_players,
  file.path(data_dir, "off_personnel_players.csv")
)

write_csv(
  off_teams_plays,
  file.path(data_dir, "off_teams_plays.csv")
)

write_csv(
  off_teams_epa,
  file.path(data_dir, "off_teams_epa.csv")
)

write_csv(
  off_teams_success,
  file.path(data_dir, "off_teams_success.csv")
)

write_csv(
  off_teams_pass,
  file.path(data_dir, "off_teams_pass.csv")
)


