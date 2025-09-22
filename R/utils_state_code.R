# Standardise & infer Australian state codes from (optional) `state` and `series_label`
# uses stringr::fixed() and simple dictionary scanning.

.STATE_LEVELS <- c("NSW","VIC","QLD","SA","WA","TAS","NT","ACT","AUS")

# recode map for an existing `state` column (we accept full names and codes)
.STATE_RECODE_EXISTING <- c(
  "new south wales" = "NSW", "nsw" = "NSW",
  "victoria" = "VIC", "vic" = "VIC",
  "queensland" = "QLD", "qld" = "QLD",
  "south australia" = "SA", "sa" = "SA",
  "western australia" = "WA", "wa" = "WA",
  "tasmania" = "TAS", "tas" = "TAS",
  "northern territory" = "NT", "nt" = "NT",
  "australian capital territory" = "ACT", "act" = "ACT",
  "australia" = "AUS", "national" = "AUS", "aus" = "AUS"
)

# for inferring from `series_label`, we deliberately AVOID short codes like "sa"/"vic"
# to prevent false hits (e.g., "Seasonally adjusted"). We match only full names & capitals.
.STATE_TOKENS_IN_LABEL <- c(
  # full state names
  "new south wales" = "NSW",
  "victoria" = "VIC",
  "queensland" = "QLD",
  "south australia" = "SA",
  "western australia" = "WA",
  "tasmania" = "TAS",
  "northern territory" = "NT",
  "australian capital territory" = "ACT",
  # state capitals -> state (for CPI etc.)
  "sydney"   = "NSW",
  "melbourne"= "VIC",
  "brisbane" = "QLD",
  "adelaide" = "SA",
  "perth"    = "WA",
  "hobart"   = "TAS",
  "darwin"   = "NT",
  "canberra" = "ACT",
  # country-level
  "australia" = "AUS",
  "national"  = "AUS"
)

# order tokens longest-first to prefer "south australia" over just "adelaide"
.tokens_ordered <- names(.STATE_TOKENS_IN_LABEL)[order(nchar(names(.STATE_TOKENS_IN_LABEL)), decreasing = TRUE)]

# normaliser with only literal replacements
.norm_label <- function(x) {
  x |>
    stringr::str_to_lower() |>
    stringr::str_replace_all(stringr::fixed(">"), " ") |>
    stringr::str_replace_all(stringr::fixed(";"), " ") |>
    stringr::str_squish()
}

# Recode an existing `state` column to canonical codes
recode_existing_state <- function(x) {
  if (is.null(x)) return(character(0))
  s <- stringr::str_squish(stringr::str_to_lower(as.character(x)))
  out <- dplyr::recode(s, !!!.STATE_RECODE_EXISTING, .default = NA_character_)
  ifelse(out %in% .STATE_LEVELS, out, NA_character_)
}

# Infer code from series_label using literal token scanning
infer_state_from_series_label <- function(series_label) {
  if (is.null(series_label)) return(character(0))
  lbl <- .norm_label(series_label)
  
  # For each label, scan the token list (longest first) and take first hit
  find_one <- function(s) {
    if (is.na(s) || s == "") return(NA_character_)
    for (tok in .tokens_ordered) {
      # literal presence check
      if (stringr::str_detect(s, stringr::fixed(tok))) {
        return(.STATE_TOKENS_IN_LABEL[[tok]])
      }
    }
    NA_character_
  }
  
  res <- vapply(lbl, find_one, FUN.VALUE = character(1))
  
  # explicitly treat ambiguous "total (state)" lines as NA
  ambig <- stringr::str_detect(lbl, stringr::fixed("total (state)"))
  res[ambig & is.na(res)] <- NA_character_
  res
}

# add a state code
add_state_code <- function(df,
                           state_col = "state",
                           series_label_col = "series_label",
                           out_col = "state_code",
                           keep_internals = FALSE) {
  stopifnot(is.data.frame(df))
  has_state <- state_col %in% names(df)
  has_label <- series_label_col %in% names(df)
  
  tmp <- df
  
  tmp$`.__state_rec__` <- if (has_state) recode_existing_state(tmp[[state_col]]) else NA_character_
  tmp$`.__state_inf__` <- if (has_label) infer_state_from_series_label(tmp[[series_label_col]]) else NA_character_
  
  # prefer explicit state col, then inferred from label
  state_fin <- dplyr::coalesce(tmp$`.__state_rec__`, tmp$`.__state_inf__`)
  state_fin <- ifelse(state_fin %in% .STATE_LEVELS, state_fin, NA_character_)
  
  tmp[[out_col]] <- state_fin
  
  if (!keep_internals) {
    tmp$`.__state_rec__` <- NULL
    tmp$`.__state_inf__` <- NULL
  }
  tmp
}
