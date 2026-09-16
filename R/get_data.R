library(httr2)
library(readr)

# Variables -----

YEAR <- get_current_season()

# Create year-specific data directory if necessary
data_dir <- file.path("Data", YEAR)

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

# Get private FTN data -----

load_ftn_private <- function(type = "all22", year = YEAR) {
  url <- paste0(
    "https://api.github.com/repos/",
    "josephjefe/ftn_data/contents/",
    "Data/",
    year,
    "/",
    type,
    ".csv"
  )

  httr2::request(url) |>
    # httr2::req_auth_bearer_token(Sys.getenv("FTN_DATA_PAT")) |>
    # Use if in RStudio
    httr2::req_auth_bearer_token(Sys.getenv("FTN_PAT_2025")) |>
    httr2::req_headers(
      Accept = "application/vnd.github.raw+json"
    ) |>
    httr2::req_perform() |>
    httr2::resp_body_string() |>
    I() |>
    readr::read_csv(show_col_types = FALSE)
}

# Retrieve data -----

all22_raw <- load_ftn_private(
  type = "all22",
  year = YEAR
) |>
  janitor::clean_names()

ftn_mappings_combined <- load_ftn_private(
  type = "ftn_mappings_combined",
  year = YEAR
) |>
  janitor::clean_names()

latest_all22 <- max(all22_raw$game_id)

latest_game <- ftn_mappings_combined |>
  filter(ftn_game_id == latest_all22) |>
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
