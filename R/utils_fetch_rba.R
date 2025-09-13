library(dplyr)
library(tidyr)
library(lubridate)
library(readabs)

get_col <- function(df, nm, default = NA_character_) {
  if (nm %in% names(df)) df[[nm]] else default
}

fetch_rba_by_registry <- function(reg) {
  rba_reg <- reg |> dplyr::filter(source == "RBA")
  if (nrow(rba_reg) == 0) return(tibble::tibble())
  
  ids <- rba_reg |>
    dplyr::filter(!is.na(series_id) & series_id != "") |>
    dplyr::pull(series_id) |> unique()
  
  out <- tibble::tibble()
  bad_ids <- character()
  
  if (length(ids)) {
    for (sid in ids) {
      tryCatch({
        # read_rba returns tidy columns like:
        # "date", "description", "frequency", "pub_date", "series"
        # "series_id", "series_type", "source", "table_title", "units", "value"
        x <- readrba::read_rba(series = sid)
        
        # Map to your schema; use existing names
        x <- x |>
          dplyr::transmute(
            # keep both the human label and the machine id
            series_label = series,          # e.g. "Employed total ; Persons ;"
            series_id    = series_id,       # e.g. "A84423349V"
            date         = as.Date(date),
            value        = value,
            unit         = units,            # was series_unit (does not exist)
            sa_flag      = series_type,     # was seasonal_adjustment
            frequency    = frequency,
            table_title  = table_title,
            description  = description,
            pub_date     = pub_date,
            source       = source
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
  
  missing_ids <- rba_reg |> dplyr::filter(is.na(series_id) | series_id == "")
  if (nrow(missing_ids)) {
    warning("RBA rows without series_id were skipped (add series_id in schema).")
  }
  
  if (length(bad_ids)) {
    warning(sprintf("These series_id(s) could not be retrieved: %s",
                    paste(bad_ids, collapse = ", ")))
  }
  
  out
}

find_bad_rba_ids <- function(reg, rba_raw) {
  requested <- reg |> dplyr::filter(source == "RBA", !is.na(series_id), series_id != "") |> dplyr::pull(series_id) |> unique()
  got       <- rba_raw |> dplyr::pull(series_id) |> unique()
  setdiff(requested, got)
}
