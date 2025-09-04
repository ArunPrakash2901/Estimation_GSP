library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(stringr)
library(purrr)

# dates to quarter end, keep monthly (can quarterly-aggregate when needed)
normalise_dates <- function(df, freq = c("Q","M","A")) {
  df |>
    mutate(
      # If date is yearmon or yearqtr-like, force to Date
      date = as.Date(date),
      quarter = yearquarter(date)  # from tsibble
    )
}

# Attach metadata from registry (state, group, transform, alias mapping)
attach_registry_meta <- function(abs_tidy, rba_tidy, reg) {
  abs_meta <- reg |> filter(source == "ABS") |> select(source, series_id, state, group, freq, sa_flag, unit, transform, alias)
  abs_join <- abs_tidy |>
    left_join(abs_meta, by = c("series_id")) |>
    mutate(source = "ABS")
  
  # For RBA, we join by alias (we treat RBA's `alias` column as unique id)
  rba_meta <- reg |> filter(source == "RBA") |> select(source, alias, group, freq, transform)
  rba_join <- rba_tidy |>
    left_join(rba_meta, by = "alias") |>
    mutate(source = "RBA", state = "NA", unit = NA_character_, sa_flag = NA_character_)
  
  bind_rows(abs_join, rba_join) |>
    filter(!is.na(group)) |>
    select(source, group, state, alias, series_id, date, value, freq, sa_flag, unit, transform) |>
    arrange(source, group, state, alias, date)
}

# Generic transform functions
tf_logdiff <- function(x, k = 1) { d <- log(x); c(rep(NA_real_, k), diff(d, lag = k)) }
tf_pctchg <- function(x, k = 1)  { c(rep(NA_real_, k), 100 * (x[(k+1):length(x)]/x[1:(length(x)-k)] - 1)) }
tf_index  <- function(x, base_val = 100) { (x / x[1]) * base_val }

apply_transforms <- function(df) {
  df |>
    group_by(source, group, state, alias) |>
    arrange(date, .by_group = TRUE) |>
    mutate(
      value_tr = dplyr::case_when(
        transform == "level"               ~ value,
        transform == "logdiff1"            ~ tf_logdiff(value, 1),
        transform == "logdiff4"            ~ tf_logdiff(value, 4),
        transform == "pctchg1"             ~ tf_pctchg(value, 1),
        transform == "pctchg4"             ~ tf_pctchg(value, 4),
        str_starts(transform, "index_")    ~ {
          base <- as.numeric(str_remove(transform, "index_"))
          tf_index(value, base_val = base)
        },
        TRUE                               ~ value
      )
    ) |>
    ungroup()
}

# Minimal quarterly aggregation for monthly RBA series if the registry says freq = "M"
monthly_to_quarterly <- function(df) {
  monthly <- df |> filter(freq == "M")
  if (nrow(monthly) == 0) return(tibble())
  monthly |>
    mutate(qtr = yearquarter(date)) |>
    group_by(source, group, state, alias, qtr) |>
    summarise(
      # Default aggregation: quarter average (Should change if in need of end-of-quarter)
      value_q = mean(value, na.rm = TRUE),
      .groups = "drop"
    ) |>
    transmute(
      source, group, state, alias,
      date = as.Date(qtr),
      value = value_q,
      freq = "Q"
    )
}

combine_and_coerce_quarterly <- function(df) {
  # Split monthly vs non-monthly, aggregate monthly to Q, then recombine
  monthly_q <- monthly_to_quarterly(df)
  non_m <- df |> filter(freq != "M") |> mutate(date = as.Date(yearquarter(date)))
  bind_rows(non_m, monthly_q) |>
    arrange(source, group, state, alias, date)
}
