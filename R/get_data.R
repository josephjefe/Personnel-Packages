library(httr2)
library(readr)

# Variables -----

YEAR <- 2025

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
    httr2::req_auth_bearer_token(Sys.getenv("FTN_DATA_PAT")) |>
    # Use if in RStudio
    # httr2::req_auth_bearer_token(Sys.getenv("FTN_PAT_2025")) |>
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
