library(dplyr)
library(tidyr)
library(lubridate)
library(readabs)

get_col <- function(df, nm, default = NA_character_) {
  if (nm %in% names(df)) df[[nm]] else default
}

fetch_abs_by_registry <- function(reg) {
  abs_reg <- reg |> dplyr::filter(source == "ABS")
  if (nrow(abs_reg) == 0) return(tibble::tibble())
  
  ids <- abs_reg |>
    dplyr::filter(!is.na(series_id) & series_id != "") |>
    dplyr::pull(series_id) |> unique()
  
  out <- tibble::tibble()
  bad_ids <- character()
  
  if (length(ids)) {
    for (sid in ids) {
      tryCatch({
        # read_abs returns tidy columns like:
        # series, series_id, date, value, unit, frequency, data_type,
        # series_type, collection_month, table_title, table_no, sheet_no
        x <- readabs::read_abs(series = sid, show_progress_bars = FALSE)
        
        # Map to your schema; use existing names
        x <- x |>
          dplyr::transmute(
            # keep both the human label and the machine id
            series_label = series,          # e.g. "Employed total ; Persons ;"
            series_id    = series_id,       # e.g. "A84423349V"
            date         = as.Date(date),
            value        = value,
            unit         = unit,            # was series_unit (does not exist)
            sa_flag      = series_type,     # was seasonal_adjustment
            frequency    = frequency,
            collection_month = collection_month,
            table_no     = table_no,
            table_title  = table_title,
            sheet_no     = sheet_no
          ) |>
          dplyr::arrange(series_id, date)
        
        out <- dplyr::bind_rows(out, x)
      },
      error = function(e) {
        message(sprintf(" No data for series_id %s: %s", sid, e$message))
        bad_ids <- c(bad_ids, sid)
      })
    }
  }
  
  missing_ids <- abs_reg |> dplyr::filter(is.na(series_id) | series_id == "")
  if (nrow(missing_ids)) {
    warning("ABS rows without series_id were skipped (add series_id in schema).")
  }
  
  if (length(bad_ids)) {
    warning(sprintf("These series_id(s) could not be retrieved: %s",
                    paste(bad_ids, collapse = ", ")))
  }
  
  out
}

find_bad_abs_ids <- function(reg, abs_raw) {
  requested <- reg |> dplyr::filter(source == "ABS", !is.na(series_id), series_id != "") |> dplyr::pull(series_id) |> unique()
  got       <- abs_raw |> dplyr::pull(series_id) |> unique()
  setdiff(requested, got)
}
