library(yaml)
library(dplyr)
library(purrr)
library(stringr)
library(tibble)

SCHOOL_NAME <- "Central York High School"
BASE_RAW    <- "https://raw.githubusercontent.com/Duosmium/duosmium/main/data/"

`%||%` <- function(a, b) if (is.null(a)) b else a

extract_teams <- function(teams_list) {
  if (is.null(teams_list) || length(teams_list) == 0) {
    return(tibble(number = integer(), school = character()))
  }
  map_dfr(teams_list, function(t) tibble(
    number = as.integer(t$number),
    school = as.character(t$school %||% NA_character_)
  ))
}

extract_events <- function(events_list) {
  if (is.null(events_list) || length(events_list) == 0) {
    return(tibble(name = character(), trial = logical()))
  }
  map_dfr(events_list, function(e) tibble(
    name  = as.character(e$name %||% NA_character_),
    trial = isTRUE(e$trial)
  ))
}

extract_placings <- function(placings_list) {
  if (is.null(placings_list) || length(placings_list) == 0) {
    return(tibble(team = integer(), event = character(), place = numeric()))
  }
  map_dfr(placings_list, function(p) tibble(
    team  = as.integer(p$team %||% NA_integer_),
    event = as.character(p$event %||% NA_character_),
    place = suppressWarnings(as.numeric(p$place %||% NA))
  ))
}

index <- yaml::read_yaml(paste0(BASE_RAW, "recents.yaml")) |> unlist()
pa_files <- index[str_detect(index, "_PA_(states|.*regional)_")]

invitational_patterns <- c(
  "tiger_invitational",
  "barons_invitational",
  "dick_smith_memorial",
  "pitt_invitational",
  "birdso_satellite_invitational",
  "georgia_scrimmage",
  "berks_county_invitational",
  "umbc",
  "umd_invitational",
  "bavf"
)

extra_files <- index[str_detect(
  index,
  paste0("(", paste(invitational_patterns, collapse = "|"), ").*_c\\.yaml$")
)]


candidate_files <- unique(c(pa_files, extra_files))
message("Downloading ", length(candidate_files), " tournament files...")

fetch_yml <- function(fname) {
  tryCatch(
    yaml::read_yaml(paste0(BASE_RAW, "results/", fname)),
    error = function(e) NULL
  )
}
all_yml <- map(candidate_files, fetch_yml)
names(all_yml) <- candidate_files

tournament_key <- function(yml) {
  yml$Tournament$`short name` %||% yml$Tournament$name %||%
    paste(yml$Tournament$state, yml$Tournament$level)
}

mode_or_na <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  as.numeric(names(sort(table(x), decreasing = TRUE))[1])
}

cutoff_lookup <- tibble(
  key      = map_chr(compact(all_yml), tournament_key),
  medals   = map_dbl(compact(all_yml), \(y) as.numeric(y$Tournament$medals   %||% NA)),
  trophies = map_dbl(compact(all_yml), \(y) as.numeric(y$Tournament$trophies %||% NA))
) |>
  group_by(key) |>
  summarise(
    medals_mode   = mode_or_na(medals),
    trophies_mode = mode_or_na(trophies),
    .groups = "drop"
  )

get_team_result <- function(fname, yml) {
  if (is.null(yml) || is.null(yml$Teams)) return(NULL)

  teams <- extract_teams(yml$Teams)
  if (nrow(teams) == 0) return(NULL) 
  our_teams <- teams |> filter(school == SCHOOL_NAME)
  if (nrow(our_teams) == 0) return(NULL)  
  
  team_nums <- our_teams$number

  events         <- extract_events(yml$Events)
  trial_events   <- events$name[events$trial]
  scored_events  <- setdiff(events$name, trial_events)
  if (length(scored_events) == 0) return(NULL)

  placings_raw <- extract_placings(yml$Placings) |>
    filter(event %in% scored_events)
  if (nrow(placings_raw) == 0) return(NULL) 
  n_offset  <- as.numeric(yml$Tournament$`n offset`  %||% 0)
  ns_offset <- as.numeric(yml$Tournament$`ns offset` %||% 1)

  grid <- expand.grid(
    team  = teams$number,
    event = scored_events,
    stringsAsFactors = FALSE
  ) |> as_tibble()

  scored <- grid |>
    left_join(placings_raw, by = c("team", "event")) |>
    left_join(
      placings_raw |> count(event, name = "n_entries"),
      by = "event"
    ) |>
    mutate(
      n_entries    = coalesce(n_entries, 0L),
      scored_place = ifelse(is.na(place), n_entries + n_offset + ns_offset, place)
    )

  totals <- scored |>
    group_by(team) |>
    summarise(points = sum(scored_place), .groups = "drop") |>
    arrange(points) |>
    mutate(rank = row_number())

  our_ranks <- totals$rank[totals$team %in% team_nums]
  our_rank  <- if (length(our_ranks) > 0) min(our_ranks) else NA_integer_
  tournament <- tournament_key(yml)
  level <- yml$Tournament$level
  year  <- yml$Tournament$year

  own_medals   <- as.numeric(yml$Tournament$medals   %||% NA)
  own_trophies <- as.numeric(yml$Tournament$trophies %||% NA)
  lookup_row   <- cutoff_lookup |> filter(key == tournament)
  medals_imputed <- is.na(own_medals) && nrow(lookup_row) > 0 && !is.na(lookup_row$medals_mode[1])

  medal_cut  <- if (!is.na(own_medals)) {
    own_medals
  } else if (nrow(lookup_row) > 0 && !is.na(lookup_row$medals_mode[1])) {
    lookup_row$medals_mode[1]
  } else {
    0
  }
  trophy_cut <- if (!is.na(own_trophies)) {
    own_trophies
  } else if (nrow(lookup_row) > 0 && !is.na(lookup_row$trophies_mode[1])) {
    lookup_row$trophies_mode[1]
  } else {
    0
  }

  our_placings <- placings_raw |> filter(team %in% team_nums, !is.na(place))

  summary_row <- tibble(
    file           = fname,
    tournament     = tournament,
    level          = level,
    year           = year,
    division       = yml$Tournament$division,
    rank           = our_rank,
    n_teams        = nrow(totals),
    trophy_cut     = trophy_cut,
    trophy         = !is.na(our_rank) && our_rank <= trophy_cut,
    event_medals   = sum(our_placings$place >= 1 & our_placings$place <= medal_cut,
                          na.rm = TRUE),
    medals_imputed = medals_imputed
  )

  event_rows <- our_placings |>
    transmute(
      tournament = tournament,
      level      = level,
      year       = year,
      event      = event,
      place      = place,
      medal      = !is.na(place) & place >= 1 & place <= medal_cut,
      medal_cut_imputed = medals_imputed
    )

  list(summary = summary_row, events = event_rows)
}

raw_results <- map2(names(all_yml), all_yml, get_team_result) |> compact()

results <- map(raw_results, "summary") |> bind_rows() |> arrange(year)
event_results <- map(raw_results, "events") |> bind_rows() |> arrange(year)

corrections_path <- "data/manual-corrections.csv"
if (file.exists(corrections_path)) {
  corrections <- read.csv(corrections_path, stringsAsFactors = FALSE) |>
    rename(corrected_rank = rank)
  results <- results |>
    left_join(corrections, by = "file") |>
    mutate(
      corrected = !is.na(corrected_rank),
      rank      = ifelse(corrected, corrected_rank, rank),
      trophy    = ifelse(corrected, rank <= trophy_cut, trophy)
    )
  message(sum(results$corrected), " tournament(s) manually corrected")
} else {
  results <- results |> mutate(corrected = FALSE, note = NA_character_)
}
results <- results |> select(-trophy_cut)

dir.create("data", showWarnings = FALSE)
write.csv(results, "data/team-stats.csv", row.names = FALSE)
write.csv(event_results, "data/team-event-results.csv", row.names = FALSE)

message(nrow(results), " tournament results found for ", SCHOOL_NAME)
message(nrow(event_results), " individual event results found")