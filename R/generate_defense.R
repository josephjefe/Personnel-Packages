# Function to generate the defense table:
#
generate_defense <- function(
  YEAR,
  TEAM,
  WEEK_START,
  WEEK_END,
  CURRENT_YEAR,
  schedule,
  teams_df,
  def_personnel_players,
  def_personnel_teams
) {
  # Team personnel-----

  def_p_team <- def_personnel_teams |>
    filter(
      defteam == TEAM,
      week %in% c(WEEK_START:WEEK_END)
    ) |>
    mutate(
      total_team_snaps = sum(snaps, na.rm = TRUE)
    ) |>
    # Summarize first to get the personnel grouping percentages
    summarize(
      snaps = sum(snaps, na.rm = TRUE),
      snap_pct = snaps / first(total_team_snaps),
      epa_total = sum(epa_total, na.rm = TRUE),
      success_total = sum(success_total, na.rm = TRUE),
      pass_total = sum(pass_total, na.rm = TRUE),
      .by = c(defteam, defense_personnel, total_team_snaps)
    ) |>
    mutate(
      defense_personnel = if_else(
        snap_pct >= 0.03,
        defense_personnel,
        "p_other"
      )
    ) |>
    # Summarize a second time to calculate the percentages for "p_other"
    summarize(
      snaps_personnel = sum(snaps),
      pct_snaps_personnel = snaps_personnel / first(total_team_snaps),
      epa_total = sum(epa_total, na.rm = TRUE),
      success_total = sum(success_total, na.rm = TRUE),
      pass_total = sum(pass_total, na.rm = TRUE),
      .by = c(defteam, defense_personnel, total_team_snaps)
    ) |>
    mutate(
      pct_snaps_personnel = round(
        pct_snaps_personnel * 100,
        0
      )
    ) |>
    mutate(
      epa_play_personnel = round(
        epa_total / snaps_personnel,
        2
      )
    ) |>
    mutate(
      success_rate_personnel = round(
        ((success_total / snaps_personnel) * 100),
        0
      )
    ) |>
    mutate(
      pass_rate_personnel = round(
        ((pass_total / snaps_personnel) * 100),
        0
      )
    ) |>
    mutate(
      team_epa_play = round(
        (sum(epa_total, na.rm = TRUE) / total_team_snaps),
        2
      )
    ) |>
    mutate(
      team_success_rate = round(
        ((sum(success_total, na.rm = TRUE) / total_team_snaps) * 100),
        0
      )
    ) |>
    mutate(
      team_pass_rate = round(
        ((sum(pass_total, na.rm = TRUE) / total_team_snaps) * 100),
        0
      )
    ) |>
    # The logical evaluates to true or false, and puts false first,
    # so 'p_other' will always be last.
    arrange(
      defense_personnel == "p_other",
      desc(pct_snaps_personnel)
    )

  # Team personnel wide-----

  def_p_team_wide <- def_p_team |>
    mutate(
      across(
        c(
          snaps_personnel,
          pct_snaps_personnel,
          epa_play_personnel,
          success_rate_personnel,
          pass_rate_personnel
        ),
        as.character
      )
    ) |>
    pivot_wider(
      id_cols = c(
        defteam,
        team_epa_play,
        team_success_rate,
        team_pass_rate,
        total_team_snaps
      ),
      names_from = defense_personnel,
      values_from = c(
        snaps_personnel,
        pct_snaps_personnel,
        epa_play_personnel,
        success_rate_personnel,
        pass_rate_personnel
      ),
      names_glue = "{defense_personnel}_{.value}"
    )

  # Team summary rows-----

  make_team_row <- function(
    data,
    stat_name,
    stat_value,
    extra_col
  ) {
    data |>
      mutate(
        position_group = "Team Defense",
        defense_id = "",
        short_name = stat_name
      ) |>
      select(
        position_group,
        defense_id,
        short_name,
        ends_with(stat_value),
        all_of(extra_col)
      ) |>
      rename_with(
        ~ gsub(
          paste0("_", stat_value, "$"),
          "",
          .x
        ),
        ends_with(stat_value)
      ) |>
      rename(
        total = all_of(extra_col)
      )
  }

  def_p_team_summary <- bind_rows(
    make_team_row(
      data = def_p_team_wide,
      stat_name = "Team Snaps",
      stat_value = "snaps_personnel",
      extra_col = "total_team_snaps"
    ),

    make_team_row(
      data = def_p_team_wide,
      stat_name = "EPA per Play",
      stat_value = "epa_play_personnel",
      extra_col = "team_epa_play"
    ),

    make_team_row(
      data = def_p_team_wide,
      stat_name = "Success Rate",
      stat_value = "success_rate_personnel",
      extra_col = "team_success_rate"
    ),

    make_team_row(
      data = def_p_team_wide,
      stat_name = "Pass Rate",
      stat_value = "pass_rate_personnel",
      extra_col = "team_pass_rate"
    )
  ) |>
    mutate(
      total = as.character(total)
    ) |>
    mutate(
      defense_id = TEAM,
      .after = position_group
    )

  # Player personnel-----

  personnel_keep <- def_p_team$defense_personnel

  all_team_snaps <- def_p_team$total_team_snaps[1]

  def_p_players <- def_personnel_players |>
    filter(
      defteam == TEAM,
      week %in% c(WEEK_START:WEEK_END)
    ) |>
    mutate(
      defense_personnel = if_else(
        defense_personnel %in% personnel_keep,
        defense_personnel,
        "p_other"
      )
    ) |>
    mutate(
      player_snaps = sum(snaps),
      .by = c(defense_id, short_name)
    ) |>
    summarize(
      snaps = sum(snaps),
      player_total = first(player_snaps),
      snap_pct = snaps / first(player_total),
      .by = c(
        defteam,
        defense_id,
        short_name,
        position_group,
        defense_personnel
      )
    ) |>
    mutate(
      player_total_pct = player_total / all_team_snaps
    )

  # Player personnel wide-----

  def_p_players_wide <- def_p_players |>
    mutate(
      snaps = as.character(snaps)
    ) |>
    mutate(
      snap_pct = as.character(
        round((snap_pct * 100), 0)
      )
    ) |>
    pivot_wider(
      id_cols = c(
        defteam,
        defense_id,
        short_name,
        position_group,
        player_total,
        player_total_pct
      ),
      names_from = defense_personnel,
      values_from = c(
        snaps,
        snap_pct
      ),
      names_glue = "{defense_personnel}_{.value}"
    ) |>
    select(
      position_group,
      defense_id,
      short_name,
      starts_with("p_"),
      player_total,
      player_total_pct
    ) |>
    rename_with(
      ~ gsub("_snaps$", "", .x),
      ends_with("_snaps")
    ) |>
    rename_with(
      ~ gsub("_snap_pct$", "_pct", .x),
      ends_with("_snap_pct")
    ) |>
    rename(
      total = player_total
    ) |>
    arrange(
      position_group,
      -total
    ) |>
    mutate(
      total = as.character(total)
    ) |>
    mutate(
      player_total_pct = as.character(
        round((player_total_pct * 100), 0)
      )
    )

  # Combine team and player data-----

  all_defense <- bind_rows(
    def_p_team_summary,
    def_p_players_wide
  ) |>
    mutate(
      total = case_when(
        is.na(total) ~ NA_character_,
        is.na(player_total_pct) ~ as.character(total),
        TRUE ~ paste0(
          total,
          " ",
          player_total_pct,
          "%"
        )
      )
    )

  all_defense_table_df1 <- all_defense

  pct_cols <- names(all_defense_table_df1)[
    startsWith(names(all_defense_table_df1), "p_") &
      !endsWith(names(all_defense_table_df1), "_pct")
  ]

  for (col in pct_cols) {
    pct_col <- paste0(col, "_pct")

    all_defense_table_df1[[col]] <- ifelse(
      is.na(all_defense_table_df1[[col]]),
      NA_character_,
      ifelse(
        is.na(all_defense_table_df1[[pct_col]]),
        as.character(all_defense_table_df1[[col]]),
        paste0(
          all_defense_table_df1[[col]],
          " ",
          all_defense_table_df1[[pct_col]],
          "%"
        )
      )
    )
  }

  all_defense_table_df <- all_defense_table_df1 |>
    select(-ends_with("_pct"))

  # Table-----

  my_team_df <- teams_df |>
    filter(
      team_abbr == TEAM
    )

  if (WEEK_START == WEEK_END) {
    # Single week:
    # Get away/home teams + scores

    single_game <- schedule |>
      filter(
        season == YEAR,
        week == WEEK_START,
        home_team == TEAM | away_team == TEAM
      )

    home_team_df <- teams_df |>
      filter(
        team_abbr == single_game$home_team[1]
      )

    away_team_df <- teams_df |>
      filter(
        team_abbr == single_game$away_team[1]
      )

    # Get the actual html for the title and subtitle

    def_title_html <- paste0(
      '<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
      away_team_df$team_logo_espn,
      '">',
      "&nbsp;",
      away_team_df$team_nick,
      "&nbsp;",
      single_game$away_score[1],
      " &nbsp;@&nbsp; ",
      single_game$home_score[1],
      "&nbsp;",
      home_team_df$team_nick,
      '&nbsp;',
      '<img height="70"src="',
      home_team_df$team_logo_espn,
      '">',
      '</div>'
    )

    def_subtitle <- paste0(
      TEAM,
      " Defense: Week ",
      WEEK_START,
      ", ",
      YEAR
    )
  } else {
    # Multiple weeks:

    def_title_html <- paste0(
      '<div style = "display:flex;justify-content:center;align-items:center;"><img height="70"src="',
      my_team_df$team_logo_espn[1],
      '">',
      "&nbsp;",
      my_team_df$team_nick[1],
      "&nbsp;",
      YEAR,
      ' Season',
      "&nbsp;",
      '<img height="70"src="',
      my_team_df$team_logo_espn[1],
      '">',
      '</div>'
    )

    def_subtitle <- paste0(
      TEAM,
      " Defense: Weeks ",
      WEEK_START,
      "-",
      WEEK_END,
      ", ",
      YEAR
    )
  }

  # Table formatting-----

  def_formation_cols <- ncol(all_defense_table_df) - 1

  def_team_html <- paste0(
    '<div style = "line-height:0.01"><img height="20" vertical-align="middle" src="',
    my_team_df$team_wordmark,
    '"></div>'
  )

  # Footnote-----

  other_footnote <- \(x, column = "p_other") {
    stopifnot(
      "x should be a gt" = inherits(x, "gt_tbl")
    )

    if (!column %in% names(x[["_data"]])) {
      return(x)
    }

    x |>
      tab_footnote(
        footnote = "Other combines all personnel groupings used on less than 3% of team snaps",
        locations = cells_column_labels(
          columns = all_of(column)
        )
      )
  }

  # Build gt table-----

  def_tab <-
    gt(
      all_defense_table_df,
      groupname_col = "position_group"
    ) |>
    tab_spanner(
      label = "Personnel = DL-LB-DB",
      columns = 4:def_formation_cols
    ) |>
    gt_theme_jefe(
      img_width = 4.5,
      caption = "Data: FTNData.com & nflverse.com"
    ) |>
    tab_header(
      title = html(def_title_html),
      subtitle = def_subtitle
    ) |>
    cols_label_with(
      columns = starts_with("p_"),
      fn = function(x) {
        sapply(x, function(col) {
          if (col == "p_other") {
            "other"
          } else {
            paste(
              strsplit(
                sub("^p_", "", col),
                ""
              )[[1]],
              collapse = "-"
            )
          }
        })
      }
    ) |>
    cols_label(
      defense_id = "",
      short_name = "",
      total = html("TOTAL&nbsp;&nbsp;")
    ) |>
    gt_nfl_logos(
      "defense_id",
      height = 25
    ) |>
    gt_nfl_headshots(
      "defense_id",
      height = 25
    ) |>
    tab_style(
      style = cell_text(
        weight = "bold",
        align = "center"
      ),
      locations = cells_title(
        groups = c("title", "subtitle")
      )
    ) |>
    tab_style(
      style = cell_text(
        size = px(18)
      ),
      locations = cells_title(
        groups = "subtitle"
      )
    ) |>
    text_transform(
      locations = cells_body(
        columns = 4:(def_formation_cols)
      ),
      fn = function(x) {
        ifelse(
          is.na(x),
          "",
          {
            text <- word(x, 1)
            sup <- coalesce(
              word(x, 2),
              ""
            )

            glue::glue(
              "<span style='font-size: 14px'>{text}</span><span style='font-size: 10px; color:#777; font-style:italic'>&nbsp;{sup}&nbsp;</span>"
            )
          }
        )
      }
    ) |>
    text_transform(
      locations = cells_body(
        columns = total
      ),
      fn = function(x) {
        ifelse(
          is.na(x),
          "",
          {
            text <- word(x, 1)
            sup <- coalesce(
              word(x, 2),
              ""
            )

            glue::glue(
              "<span style='font-size: 14px'>{text}</span><span style='font-size: 10px; color:#777; font-style:italic'>&nbsp;{sup}&nbsp;&nbsp;&nbsp;&nbsp;</span>"
            )
          }
        )
      }
    ) |>
    text_transform(
      locations = cells_body(
        columns = short_name
      ),
      fn = function(x) {
        ifelse(
          x == TEAM,
          html(def_team_html),
          x
        )
      }
    ) |>
    cols_align(
      align = "left",
      columns = everything()
    ) |>
    other_footnote() |>
    tab_options(
      data_row.padding = px(0)
    )

  # Return the gt table-----

  return(def_tab)
}
