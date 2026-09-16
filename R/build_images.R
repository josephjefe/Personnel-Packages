#
# This script generates and saves the images that the shiny app will pull from GitHub
#
library(nflreadr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(gt)
library(gtExtras)
library(nflplotR)
library(jefeR)
library(digest)
library(readr)

# Settings -----------------------------------------------------------------

TARGET_YEAR <- 2026
IMAGE_VERSION <- 1

# Get current NFL season
latest_game <- nflreadr::rds_from_url(
  "https://github.com/josephjefe/Personnel-Packages/raw/refs/heads/main/Data/latest_game.rds"
)

CURRENT_YEAR <- latest_game$year[1]

# Create image root directory if it does not exist
dir.create(
  "Images",
  recursive = TRUE,
  showWarnings = FALSE
)

# Functions ----------------------------------------------------------------

personnel_image_filename <- function(
  WEEK,
  TEAM,
  TYPE
) {
  paste0(
    "Week_",
    sprintf("%02d", WEEK),
    "_",
    TEAM,
    "_",
    TYPE,
    ".png"
  )
}

normalize_for_hash <- function(df) {
  if (nrow(df) == 0 || ncol(df) == 0) {
    return(df)
  }

  df |>
    arrange(
      across(
        everything()
      )
    )
}

get_data_hash <- function(
  game,
  team_data,
  player_data
) {
  hash_data <- list(
    game = normalize_for_hash(game),
    team_data = normalize_for_hash(team_data),
    player_data = normalize_for_hash(player_data)
  )

  digest::digest(
    hash_data,
    algo = "sha256",
    serialize = TRUE
  )
}

# Load existing manifest ---------------------------------------------------

manifest_path <- file.path(
  "Images",
  "image_manifest.csv"
)

if (file.exists(manifest_path)) {
  image_manifest <- readr::read_csv(
    manifest_path,
    show_col_types = FALSE
  )
} else {
  image_manifest <- tibble(
    YEAR = integer(),
    TEAM = character(),
    WEEK = integer(),
    TYPE = character(),
    FILENAME = character(),
    DATA_HASH = character(),
    IMAGE_VERSION = integer()
  )
}

# Delete image cache for seasons more than one year old -------------------

image_years <- list.dirs(
  "Images",
  recursive = FALSE,
  full.names = FALSE
)

image_years <- image_years[
  grepl(
    "^[0-9]{4}$",
    image_years
  )
]

image_years <- as.integer(image_years)

years_to_delete <- image_years[
  image_years < TARGET_YEAR - 1
]

if (length(years_to_delete) > 0) {
  for (OLD_YEAR in years_to_delete) {
    message(
      "Deleting image cache for old season: ",
      OLD_YEAR
    )

    unlink(
      file.path(
        "Images",
        OLD_YEAR
      ),
      recursive = TRUE,
      force = TRUE
    )
  }

  image_manifest <- image_manifest |>
    filter(
      !YEAR %in% years_to_delete
    )
}

# Load schedule ------------------------------------------------------------

schedule <- load_schedules(
  seasons = TARGET_YEAR
)

teams_df <- load_teams()

# Load personnel data for target year -------------------------------------

base_url <- paste0(
  "https://raw.githubusercontent.com/",
  "josephjefe/Personnel-Packages/main/Data/",
  TARGET_YEAR,
  "/"
)

off_personnel_players <- nflreadr::rds_from_url(
  paste0(
    base_url,
    "off_personnel_players.rds"
  )
)

off_personnel_teams <- nflreadr::rds_from_url(
  paste0(
    base_url,
    "off_personnel_teams.rds"
  )
)

def_personnel_players <- nflreadr::rds_from_url(
  paste0(
    base_url,
    "def_personnel_players.rds"
  )
)

def_personnel_teams <- nflreadr::rds_from_url(
  paste0(
    base_url,
    "def_personnel_teams.rds"
  )
)

# Source generation functions ---------------------------------------------

source("R/generate_offense.R")
source("R/generate_defense.R")

# Get teams that played during target season -------------------------------

teams <- schedule |>
  filter(
    season == TARGET_YEAR
  ) |>
  summarise(
    teams = list(
      sort(
        unique(
          c(
            home_team,
            away_team
          )
        )
      )
    )
  ) |>
  pull(teams) |>
  unlist()

# Generate current-season images ------------------------------------------

for (TARGET_TEAM in teams) {
  team_games <- schedule |>
    filter(
      season == TARGET_YEAR,
      home_team == TARGET_TEAM |
        away_team == TARGET_TEAM,
      !is.na(result)
    ) |>
    distinct(
      week,
      .keep_all = TRUE
    ) |>
    arrange(week)

  for (TARGET_WEEK in team_games$week) {
    message(
      "Processing ",
      TARGET_YEAR,
      " ",
      TARGET_TEAM,
      " Week ",
      TARGET_WEEK
    )

    game <- team_games |>
      filter(
        week == TARGET_WEEK
      ) |>
      slice_head(
        n = 1
      )

    GAME_ID <- game$game_id[1]

    # Relevant FTN data for this game ------------------------------------

    off_team_data <- off_personnel_teams |>
      filter(
        nflverse_game_id == GAME_ID
      )

    off_player_data <- off_personnel_players |>
      filter(
        nflverse_game_id == GAME_ID
      )

    def_team_data <- def_personnel_teams |>
      filter(
        nflverse_game_id == GAME_ID
      )

    def_player_data <- def_personnel_players |>
      filter(
        nflverse_game_id == GAME_ID
      )

    # Build hashes --------------------------------------------------------

    offense_hash <- get_data_hash(
      game = game,
      team_data = off_team_data,
      player_data = off_player_data
    )

    defense_hash <- get_data_hash(
      game = game,
      team_data = def_team_data,
      player_data = def_player_data
    )

    # Offense -------------------------------------------------------------

    offense_filename <- personnel_image_filename(
      WEEK = TARGET_WEEK,
      TEAM = TARGET_TEAM,
      TYPE = "Offense"
    )

    offense_dir <- file.path(
      "Images",
      TARGET_YEAR,
      "offense"
    )

    offense_file <- file.path(
      offense_dir,
      offense_filename
    )

    offense_manifest <- image_manifest |>
      filter(
        YEAR == TARGET_YEAR,
        TEAM == TARGET_TEAM,
        WEEK == TARGET_WEEK,
        TYPE == "Offense"
      )

    offense_current <- nrow(offense_manifest) == 1 &&
      file.exists(offense_file) &&
      identical(
        offense_manifest$DATA_HASH[1],
        offense_hash
      ) &&
      identical(
        offense_manifest$IMAGE_VERSION[1],
        IMAGE_VERSION
      )

    if (!offense_current) {
      message(
        "Generating offense: ",
        offense_filename
      )

      dir.create(
        offense_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      off_tab <- generate_offense(
        YEAR = TARGET_YEAR,
        TEAM = TARGET_TEAM,
        WEEK_START = TARGET_WEEK,
        WEEK_END = TARGET_WEEK,
        CURRENT_YEAR = CURRENT_YEAR,
        schedule = schedule,
        teams_df = teams_df,
        off_personnel_players = off_personnel_players,
        off_personnel_teams = off_personnel_teams
      )

      gtsave_with_border(
        gt_object = off_tab,
        path = offense_dir,
        filename = offense_filename,
        vwidth = 1200
      )

      image_manifest <- image_manifest |>
        filter(
          !(YEAR == TARGET_YEAR &
            TEAM == TARGET_TEAM &
            WEEK == TARGET_WEEK &
            TYPE == "Offense")
        ) |>
        bind_rows(
          tibble(
            YEAR = TARGET_YEAR,
            TEAM = TARGET_TEAM,
            WEEK = TARGET_WEEK,
            TYPE = "Offense",
            FILENAME = offense_filename,
            DATA_HASH = offense_hash,
            IMAGE_VERSION = IMAGE_VERSION
          )
        )
    } else {
      message(
        "Skipping current offense: ",
        offense_filename
      )
    }

    # Defense -------------------------------------------------------------

    defense_filename <- personnel_image_filename(
      WEEK = TARGET_WEEK,
      TEAM = TARGET_TEAM,
      TYPE = "Defense"
    )

    defense_dir <- file.path(
      "Images",
      TARGET_YEAR,
      "defense"
    )

    defense_file <- file.path(
      defense_dir,
      defense_filename
    )

    defense_manifest <- image_manifest |>
      filter(
        YEAR == TARGET_YEAR,
        TEAM == TARGET_TEAM,
        WEEK == TARGET_WEEK,
        TYPE == "Defense"
      )

    defense_current <- nrow(defense_manifest) == 1 &&
      file.exists(defense_file) &&
      identical(
        defense_manifest$DATA_HASH[1],
        defense_hash
      ) &&
      identical(
        defense_manifest$IMAGE_VERSION[1],
        IMAGE_VERSION
      )

    if (!defense_current) {
      message(
        "Generating defense: ",
        defense_filename
      )

      dir.create(
        defense_dir,
        recursive = TRUE,
        showWarnings = FALSE
      )

      def_tab <- generate_defense(
        YEAR = TARGET_YEAR,
        TEAM = TARGET_TEAM,
        WEEK_START = TARGET_WEEK,
        WEEK_END = TARGET_WEEK,
        CURRENT_YEAR = CURRENT_YEAR,
        schedule = schedule,
        teams_df = teams_df,
        def_personnel_players = def_personnel_players,
        def_personnel_teams = def_personnel_teams
      )

      gtsave_with_border(
        gt_object = def_tab,
        path = defense_dir,
        filename = defense_filename,
        vwidth = 1200
      )

      image_manifest <- image_manifest |>
        filter(
          !(YEAR == TARGET_YEAR &
            TEAM == TARGET_TEAM &
            WEEK == TARGET_WEEK &
            TYPE == "Defense")
        ) |>
        bind_rows(
          tibble(
            YEAR = TARGET_YEAR,
            TEAM = TARGET_TEAM,
            WEEK = TARGET_WEEK,
            TYPE = "Defense",
            FILENAME = defense_filename,
            DATA_HASH = defense_hash,
            IMAGE_VERSION = IMAGE_VERSION
          )
        )
    } else {
      message(
        "Skipping current defense: ",
        defense_filename
      )
    }
  }
}

# Save manifest ------------------------------------------------------------

image_manifest <- image_manifest |>
  arrange(
    YEAR,
    TYPE,
    WEEK,
    TEAM
  )

readr::write_csv(
  image_manifest,
  manifest_path
)

message(
  "Image cache build complete for ",
  TARGET_YEAR,
  "."
)
