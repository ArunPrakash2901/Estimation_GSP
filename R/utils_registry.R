library(readxl)
library(dplyr)
library(stringr)

read_registry <- function(path = "data/metadata/schema.xlsx", sheet = "series_registry") {
  reg <- readxl::read_excel(path, sheet = sheet) |>
    mutate(
      source = toupper(source),
      state  = ifelse(is.na(state), "NA", state),
      alias  = ifelse(is.na(alias), series_id, alias),
      freq   = toupper(freq),
      sa_flag = toupper(sa_flag)
    )
  stopifnot(all(c("source","group","state","freq","transform","alias") %in% names(reg)))
  reg
}

