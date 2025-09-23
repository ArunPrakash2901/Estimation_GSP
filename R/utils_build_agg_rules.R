suppressPackageStartupMessages({
  library(tidyverse)
  library(stringr)
})



SCHEMA_IN  <- "data/schema.csv"
SCHEMA_OUT <- "data/schema_with_rules.csv"

# helpers

normalize_rule <- function(x) {
  x <- tolower(trimws(as.character(x)))
  case_when(
    x %in% c("sum","s","add","total","tot") ~ "sum",
    x %in% c("mean","avg","average","m")    ~ "mean",
    x %in% c("last","eop","end","end-of-period","stock","closing","eom") ~ "last",
    x %in% c("compound","comp","prod")      ~ "compound",
    TRUE ~ NA_character_
  )
}

# Core mapping from UNIT -> default rule
rule_from_unit <- function(unit_chr) {
  u <- tolower(trimws(unit_chr))
  case_when(
    # indices / levels (average within quarter)
    str_detect(u, "index")                       ~ "mean",
    str_detect(u, "percent|per cent|per\\s*cent")~ "mean",
    str_detect(u, "per cent per annum")          ~ "mean",
    
    # monetary flows / amounts
    str_detect(u, "\\$|million|billion|'000|\\b000\\b") ~ "sum",
    
    # hours worked (flow)
    str_detect(u, "hours")                       ~ "sum",
    
    # counts can be either survey-level or flows; default to mean (safer) and override via series name below
    str_detect(u, "^number$|\\b000\\b")         ~ "mean",
    
    TRUE ~ NA_character_
  )
}

# Series-name overrides (take precedence over unit rules)
# Return one of {sum, mean, last, compound} or NA if no override
rule_from_series <- function(series_label) {
  s <- tolower(series_label)
  
  case_when(
    # STOCKS at end-of-month: use last
    str_detect(s, "^credit; .*; seasonally adjusted$") ~ "last",  # RBA credit aggregates
    str_detect(s, "^credit; (business|total|investor housing|owner-occupier housing);") ~ "last",
    
    # INTEREST RATES / YIELDS: use mean
    str_detect(s, "cash rate|bond") ~ "mean",
    
    # CPI index numbers: mean
    str_detect(s, "index numbers ; all groups cpi") ~ "mean",
    str_detect(s, "^consumer price index$") ~ "mean",
    
    # labour: employment (survey level) mean; hours (flow) sum; unemployment rate mean
    str_detect(s, "^employed total") ~ "mean",
    str_detect(s, "^unemployment rate") ~ "mean",
    str_detect(s, "^monthly hours worked") ~ "sum",
    str_detect(s, "^job vacancies") ~ "mean",
    
    # trade & turnover (flows): sum
    str_detect(s, "^turnover ;") ~ "sum",
    str_detect(s, "^credits, total goods") ~ "sum",
    str_detect(s, "^balance on goods") ~ "sum",  # note: can be negative; still a flow sum
    
    # building/construction (flows): sum
    str_detect(s, "^total value of building jobs") ~ "sum",
    str_detect(s, "^total number of dwelling units") ~ "sum",
    
    # capex / actual expenditure (flows): sum
    str_detect(s, "^actual expenditure") ~ "sum",
    
    # household spending (flow): sum
    str_detect(s, "^household spending") ~ "sum",
    
    # WPI quarterly index already quarterly -> identity not needed; treat as mean (no harm)
    str_detect(s, "^quarterly index ; total hourly rates of pay") ~ "mean",
    
    # SFD already quarterly -> identity not needed; treat as sum (neutral if no M agg is done)
    str_detect(s, "state final demand") ~ "sum",
    
    # Business Turnover Index (monthly index): mean
    str_detect(s, "^business turnover index") ~ "mean",
    
    # monthly percent changes -> compound (add if m/m % changes)
    str_detect(s, "(m/m|mom|pct change|percent change|monthly inflation)") ~ "compound",
    
    TRUE ~ NA_character_
  )
}

# Final aggregator decision: priority series override > explicit schema agg_rule > unit default
decide_agg_rule <- function(unit, series_label, agg_rule_existing = NA_character_) {
  s_over <- rule_from_series(series_label)
  if (!is.na(s_over)) return(s_over)
  
  # if schema already has a valid agg_rule, keep it
  norm <- normalize_rule(agg_rule_existing)
  if (!is.na(norm)) return(norm)
  
  u_rule <- rule_from_unit(unit)
  if (!is.na(u_rule)) return(u_rule)
  
  # conservative fallback: mean (avoids double-counting)
  "mean"
}

# main

schema <- readr::read_csv(SCHEMA_IN, show_col_types = FALSE)

# required columns
stopifnot("series_id" %in% names(schema))
if (!("unit" %in% names(schema))) {
  warning("No 'unit' column found in schema; unit-based defaults will be skipped.")
  schema$unit <- NA_character_
}
if (!("series_label" %in% names(schema))) {
  # try possible names
  guess_col <- intersect(names(schema), c("alias","label","series","name","title"))
  if (length(guess_col)) {
    schema$series_label <- schema[[guess_col[1]]]
  } else {
    stop("Schema must contain a 'series_label' (or an alias-like column) to use name-based overrides.")
  }
}
if (!("agg_rule" %in% names(schema))) schema$agg_rule <- NA_character_

schema2 <- schema |>
  mutate(
    agg_rule_new = pmap_chr(
      list(unit, series_label, agg_rule),
      ~ decide_agg_rule(..1, ..2, ..3)
    ),
    agg_rule_new = normalize_rule(agg_rule_new)
  )

# QA summary
message("\nAggregation rule summary:")
print(schema2 |> count(agg_rule_new, sort = TRUE))

# sanity: flag anything strange
bad_rules <- schema2 |> filter(is.na(agg_rule_new))
if (nrow(bad_rules)) {
  warning("Some rows have NA agg_rule_new. They will default to 'mean' if used without fix.")
}

# write out new schema
schema_out <- schema2 |>
  mutate(agg_rule = coalesce(agg_rule, agg_rule_new)) |>
  select(-agg_rule_new)

readr::write_csv(schema_out, SCHEMA_OUT)
message(glue::glue("\nWrote updated schema with agg_rule → {SCHEMA_OUT}"))

