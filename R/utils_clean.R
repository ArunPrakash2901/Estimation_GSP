suppressPackageStartupMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(stringr)
})

source("R/utils_state_code.R")

prep_step1 <- function(df) {
  stopifnot(is.data.frame(df))
  df <- janitor::clean_names(df)
  
  #  safely drop optional columns if present 
  df <- df |>
    dplyr::select(-dplyr::any_of(c("collection_month","table_no","sheet_no","pub_date")))
  
  #  ensure a date column exists BEFORE arrange() 
  if (!"date" %in% names(df)) {
    alt <- intersect(c("obs_date","period"), names(df))
    df$date <- if (length(alt)) df[[alt[1]]] else NA
  }
  
  #  gentle pre-normalisation if columns exist 
  if ("frequency" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        frequency = dplyr::recode(
          frequency,
          "Quarter" = "Quarterly",
          "Month"   = "Monthly",
          .default  = frequency
        )
      )
  }
  if ("sa_flag" %in% names(df)) {
    df <- df |>
      dplyr::mutate(
        sa_flag = stringr::str_replace(
          sa_flag,
          stringr::fixed("Seasonally adjusted"),
          "Seasonally Adjusted"
        )
      )
  }
  
  #  ensure freq exists, then populate from frequency if available 
  if (!"freq" %in% names(df)) df$freq <- NA_character_
  if ("frequency" %in% names(df)) df <- df |> dplyr::mutate(freq = dplyr::coalesce(freq, frequency))
  
  #  arrange is now safe 
  df <- df |> dplyr::arrange(date, series_id)
  
  if (!"state" %in% names(df)) df$state <- NA_character_
  
  #  transform pipeline 
  out <- df |>
    dplyr::mutate(
      date  = suppressWarnings(lubridate::as_date(.data$date)),
      value = suppressWarnings(as.numeric(.data$value))
    ) |>
    dplyr::mutate(
      dplyr::across(
        dplyr::any_of(c(
          "series_id","alias","series_label","state","freq","unit",
          "sa_flag","transform_rule","source","industry","table","seasonal_adj_source"
        )),
        ~ ifelse(is.na(.x), NA, stringr::str_squish(as.character(.x)))
      )
    ) |>
    # add canonical state code (prefers existing `state`, else infers from `series_label`)
    add_state_code(state_col = "state",
                   series_label_col = "series_label",
                   out_col = "state_code") |>
    # normalize frequency labels
    dplyr::mutate(
      freq = dplyr::case_when(
        stringr::str_to_upper(freq) %in% c("Q","QUARTER","QUARTERLY") ~ "Q",
        stringr::str_to_upper(freq) %in% c("M","MONTH","MONTHLY")     ~ "M",
        stringr::str_to_upper(freq) %in% c("W","WEEK","WEEKLY")       ~ "W",
        stringr::str_to_upper(freq) %in% c("D","DAY","DAILY")         ~ "D",
        TRUE ~ freq
      ),
      # expose a single canonical state column for downstream use
      state = dplyr::coalesce(.data$state_code, .data$state)
    )
  
  #  reorder canonical columns (NO braces needed) 
  ordered <- c(
    "series_id","alias","series_label","state","state_code",
    "date","value","freq","unit","sa_flag","transform_rule",
    "source","industry","table","seasonal_adj_source"
  )
  out <- out |> dplyr::select(dplyr::any_of(ordered), dplyr::everything())
  
  # assertions
  if (!"series_id" %in% names(out)) stop("`series_id` missing after prep.")
  if (!"date"      %in% names(out)) stop("`date` missing after prep.")
  if (!"value"     %in% names(out)) stop("`value` missing after prep.")
  
  out
}
