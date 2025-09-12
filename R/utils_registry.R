library(readr)
library(dplyr)
library(stringr)
library(purrr)

# - No column renames enforced, just light defaults so downstream code is stable.
# - Keeps 'download_args' as raw JSON string; we parse it later where needed.
read_registry <- function(path = "data/schema.csv") {
  stopifnot(file.exists(path))
  reg <- readr::read_csv(path, show_col_types = FALSE) |> 
    mutate(
      source   = toupper(source),
      freq     = toupper(coalesce(freq, "")),
      sa_flag  = toupper(coalesce(sa_flag, "")),
      state    = ifelse(is.na(geo) | geo == "", "NA", geo),
      # safe alias if missing
      alias    = dplyr::coalesce(alias, series_id, rba_series, rba_series_id),
      transform = ifelse(is.na(transform) | transform == "", "level", transform)
    )
  # checks
  need <- c("source","group","state","freq","transform","alias")
  miss <- setdiff(need, names(reg))
  if (length(miss)) stop("Schema missing columns: ", paste(miss, collapse=", "))
  reg
}
