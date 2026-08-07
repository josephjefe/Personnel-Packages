library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(readr)

# Data-----

# Create year-specific data directory if necessary
data_dir <- file.path("Data", as.character(YEAR))

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

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
all22_def <- all22_raw |>
  select(
    ftn_game_id = game_id,
    ftn_play_id = play_id,
    all_of(paste0("def", 1:11, "id"))
  ) |>
  pivot_longer(
    cols = starts_with("def"),
    names_to = "def_position",
    values_to = "defense_id"
  ) |>
  filter(!is.na(defense_id)) |>
  left_join(
    select(players, gsis_id, short_name, position_group),
    by = c("defense_id" = "gsis_id")
  )

def_personnel <- all22_def |>
  filter(position_group %in% c("DB", "LB", "DL")) |>
  summarize(
    db = sum(position_group == "DB"),
    lb = sum(position_group == "LB"),
    dl = sum(position_group == "DL"),
    .by = c(ftn_game_id, ftn_play_id)
  ) |>
  mutate(defense_personnel = paste0("p_", dl, lb, db))

# Add all22 players to pbp data
def_ftn_pbp <- ftn_pbp_raw |>
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
    select(def_personnel, ftn_game_id, ftn_play_id, defense_personnel),
    by = c("ftn_game_id", "ftn_play_id")
  ) |>
  filter(!is.na(defense_personnel))

def_personnel_teams <- def_ftn_pbp |>
  summarize(
    snaps = n(),
    epa_total = sum(epa, na.rm = TRUE),
    success_total = sum(success, na.rm = TRUE),
    pass_total = sum(pass, na.rm = TRUE),
    .by = c(nflverse_game_id, week, defteam, posteam, defense_personnel)
  ) |>
  mutate(
    team_snaps = sum(snaps),
    .by = c(nflverse_game_id, week, defteam)
  )

def_personnel_players <- def_ftn_pbp |>
  left_join(
    select(
      all22_def,
      ftn_game_id,
      ftn_play_id,
      defense_id,
      short_name,
      position_group
    )
  ) |>
  filter(position_group %in% c("DL", "LB", "DB")) |>
  summarize(
    snaps = n(),
    .by = c(
      nflverse_game_id,
      week,
      posteam,
      defteam,
      defense_personnel,
      defense_id,
      short_name,
      position_group
    )
  )

# Save summarized datasets -----

saveRDS(
  def_personnel_players,
  file.path(data_dir, "def_personnel_players.rds")
)

saveRDS(
  def_personnel_teams,
  file.path(data_dir, "def_personnel_teams.rds")
)
