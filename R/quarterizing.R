suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(lubridate)
})

# helpers 
q_start <- function(d) floor_date(d, unit = "quarter")
q_end   <- function(d) ceiling_date(d, unit = "quarter") - days(1)
q_id    <- function(d) paste0(year(q_start(d)), "Q", quarter(q_start(d)))

#quarterize_panel 
quarterize_panel <- function(df, rules_tbl) {
  stopifnot(all(c("series_id","date","value","freq") %in% names(df)))
  if (!"state" %in% names(df)) df$state <- "AUS"
  
  # coerce date to Date to avoid parse warnings downstream
  df <- df |> mutate(date = as.Date(date))
  
  dfq <- df |>
    mutate(
      freq_norm = case_when(
        toupper(freq) %in% c("Q","QUARTER","QUARTERLY") ~ "Q",
        toupper(freq) %in% c("M","MONTH","MONTHLY")     ~ "M",
        toupper(freq) %in% c("W","WEEK","WEEKLY")       ~ "W",
        toupper(freq) %in% c("D","DAY","DAILY")         ~ "D",
        TRUE ~ NA_character_
      ),
      qtr_end = q_end(date)
    ) |>
    left_join(rules_tbl |> transmute(series_id, rule = tolower(trimws(rule))), by = "series_id")
  
  # Roll W/D → M 
  df_wd <- dfq |> filter(freq_norm %in% c("W","D"))
  if (nrow(df_wd)) {
    df_wd <- df_wd |>
      mutate(month = floor_date(date, "month")) |>
      group_by(series_id, state, month, rule) |>
      summarise(
        value = if (first(rule) == "sum") sum(value, na.rm = TRUE) else mean(value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      transmute(series_id, state, date = month, value, freq_norm = "M", rule)
    dfq <- dfq |>
      filter(!freq_norm %in% c("W","D")) |>
      select(series_id, state, date, value, freq_norm, rule) |>
      bind_rows(df_wd) |>
      mutate(qtr_end = q_end(date))
  } else {
    dfq <- dfq |> select(series_id, state, date, value, freq_norm, rule, qtr_end)
  }
  
  # Aggregate to quarter
  q_panel <- bind_rows(
    # Quarterly series: identity (last within quarter if duplicates)
    dfq |> filter(freq_norm == "Q") |>
      group_by(series_id, state, qtr_end) |>
      summarise(value_q = dplyr::last(value[order(date)], default = NA_real_), .groups = "drop"),
    # Monthly series: rule-driven
    dfq |> filter(freq_norm == "M") |>
      group_by(series_id, state, qtr_end, rule) |>
      summarise(
        value_q = dplyr::case_when(
          first(rule) == "sum"      ~ sum(value,  na.rm = TRUE),
          first(rule) == "last"     ~ dplyr::last(value[order(date)], default = NA_real_),
          first(rule) == "compound" ~ prod(1 + value, na.rm = TRUE) - 1, # for decimal rates
          TRUE                      ~ mean(value, na.rm = TRUE)
        ),
        .groups = "drop"
      )
  ) |>
    arrange(series_id, state, qtr_end) |>
    mutate(q = q_id(qtr_end))
  
  q_panel
}

#ragged_edge_map
ragged_edge_map <- function(df, ref_date = Sys.Date()) {
  stopifnot(all(c("series_id","date") %in% names(df)))
  if (!"state" %in% names(df)) df$state <- "AUS"
  df |>
    mutate(date = as.Date(date)) |>
    group_by(series_id, state) |>
    summarise(
      last_obs_date = max(date, na.rm = TRUE),
      last_obs_q_end = q_end(last_obs_date),
      last_q = q_id(last_obs_q_end),
      days_since_last = as.integer(as.Date(ref_date) - last_obs_date),
      .groups = "drop"
    ) |>
    arrange(desc(last_obs_date))
}

# snapshot helpers
snapshot_cut_dates <- function(target_q_end) {
  tibble(
    label = c("T+15","T+45","T+75"),
    cut   = as.Date(target_q_end) + c(15,45,75)
  )
}

quarter_month_coverage <- function(df, cut_date) {
  if (!"freq" %in% names(df)) return(tibble())
  if (!"state" %in% names(df)) df$state <- "AUS"
  df <- df |>
    mutate(
      date = as.Date(date),
      freq_norm = case_when(
        toupper(freq) %in% c("Q","QUARTER","QUARTERLY") ~ "Q",
        toupper(freq) %in% c("M","MONTH","MONTHLY")     ~ "M",
        TRUE ~ NA_character_
      ),
      qtr_end = q_end(date)
    ) |>
    filter(date <= as.Date(cut_date))
  df |>
    filter(freq_norm == "M") |>
    group_by(series_id, state, qtr_end) |>
    summarise(months_seen = n_distinct(floor_date(date, "month")), .groups = "drop")
}

# build_nowcast_snapshots 
build_nowcast_snapshots <- function(df, rules_tbl, target_q_end = NULL) {
  stopifnot(all(c("series_id","date","value") %in% names(df)))
  df <- df |> mutate(date = as.Date(date))
  latest_q_end <- if (is.null(target_q_end)) q_end(max(df$date, na.rm = TRUE)) else q_end(as.Date(target_q_end))
  cuts <- snapshot_cut_dates(latest_q_end)
  
  # precompute: use same rules across cuts
  rules_tbl <- rules_tbl |> transmute(series_id, rule = tolower(trimws(rule)))
  
  panels <- vector("list", nrow(cuts))
  coverage <- vector("list", nrow(cuts))
  
  for (i in seq_len(nrow(cuts))) {
    cut_i <- cuts$cut[i]
    df_cut <- df |> filter(date <= cut_i)
    panels[[i]]   <- quarterize_panel(df_cut, rules_tbl = rules_tbl) |> mutate(snapshot = cuts$label[i])
    coverage[[i]] <- quarter_month_coverage(df, cut_i)               |> mutate(snapshot = cuts$label[i])
  }
  
  list(
    cuts = cuts |> mutate(target_q = q_id(latest_q_end)),
    panels = bind_rows(panels),
    coverage = bind_rows(coverage)
  )
}
