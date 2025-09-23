library(targets)
library(tarchetypes)

# Use a separate store so graphs/metadata don’t mix with main.
tar_config_set(store = "_targets_diag")

tar_option_set(packages = c(
  "dplyr","tidyr","lubridate","readr"
))

# Reuse your helpers
source("R/quarterizing.R")  # quarterize_panel(), ragged_edge_map(), build_nowcast_snapshots()
source("R/utils_registry.R")    # read_registry()

list(
  # Inputs produced/maintained by the main project
  tar_target(diag_schema_file, "data/schema.csv", format = "file"),
  tar_target(diag_schema_rules,
             readr::read_csv(diag_schema_file, show_col_types = FALSE) |>
               transmute(series_id, rule = tolower(trimws(agg_rule)))
  ),
  
  tar_target(diag_p21_raw_file, "data/p21_raw.rds", format = "file"),
  tar_target(diag_p21_raw,
             readRDS(diag_p21_raw_file)
  ),
  
  # Outputs (compute once, write, and return file paths)
  tar_file(
    diag_q_panel_csv,
    {
      q_panel <- quarterize_panel(diag_p21_raw, rules_tbl = diag_schema_rules)
      readr::write_csv(q_panel, "data/q_panel.csv")
      "data/q_panel.csv"
    }
  ),
  
  tar_file(
    diag_ragged_csv,
    {
      ragged <- ragged_edge_map(diag_p21_raw, ref_date = Sys.Date())
      readr::write_csv(ragged, "data/ragged_edge_profile.csv")
      "data/ragged_edge_profile.csv"
    }
  ),
  
  tar_target(
    diag_snaps_files,
    {
      snaps <- build_nowcast_snapshots(diag_p21_raw, rules_tbl = diag_schema_rules)
      paths <- c(
        cuts     = "data/snapshot_cuts.csv",
        panels   = "data/q_panel_snapshots.csv",
        coverage = "data/within_quarter_coverage.csv"
      )
      readr::write_csv(snaps$cuts,     paths[["cuts"]])
      readr::write_csv(snaps$panels,   paths[["panels"]])
      readr::write_csv(snaps$coverage, paths[["coverage"]])
      paths
    },
    format = "file"
  )
)
