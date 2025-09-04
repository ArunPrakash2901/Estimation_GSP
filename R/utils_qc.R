library(dplyr)
library(cli)

qc_missingness <- function(df){
"
quality-control (QC) function, to scan through your transformed 
dataset and report how much data is missing after all the transformations
have been applied
"
  
  df |>
    group_by(source, group, state, alias) |>
    summarise(
      n = n(),
      n_na = sum(is.na(value_tr)),
      first = min(date),
      last  = max(date),
      .groups = "drop"
    )
}

qc_print <- function(qc_tbl) {
  cli::cli_h2("QC: Missingness after transforms")
  print(qc_tbl |> arrange(desc(n_na)) |> head(20))
}
