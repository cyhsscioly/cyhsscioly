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
  "umbc_neighbors_division_invitational",
  "umd_invitational",
  "bavf_invitational"
)

extra_files <- index[str_detect(
  index,
  paste0("(", paste(invitational_patterns, collapse = "|"), ").*_c\\.yaml$")
)]

candidate_files <- unique(c(pa_files, extra_files))
message("Checking ", length(candidate_files), " tournament files...")

get_team_result <- function(fname) {
  yml <- tryCatch(
    yaml::read_yaml(paste0(BASE_RAW, "results/", fname)),
    error = function(e) NULL
  )
  if (is.null(yml) || is.null(yml$Teams)) return(NULL)

  teams <- extract_teams(yml$Teams)
  if (nrow(teams) == 0) return(NULL)  
  our_team <- teams |> filter(school == SCHOOL_NAME)
  if (nrow(our_team) == 0) return(NULL)  
  team_num <- our_team$number[1]

  events         <- extract_events(yml$Events)
  trial_events   <- events$name[events$trial]
  scored_events  <- setdiff(events$name, trial_events)
  if (length(scored_events) == 0) return(NULL)

  placings_raw <- extract_placings(yml$Placings) |>
    filter(event %in% scored_events)
  if (nrow(placings_raw) == 0) return(NULL)

  grid <- expand.grid(
    team  = teams$number,
    event = scored_events,
    stringsAsFactors = FALSE
  ) |> as_tibble()

  scored <- grid |>
    left_join(placings_raw, by = c("team", "event")) |>
    left_join(
      placings_raw |> filter(!is.na(place)) |> count(event, name = "n_scored"),
      by = "event"
    ) |>
    mutate(
      n_scored     = coalesce(n_scored, 0L),
      scored_place = ifelse(is.na(place), n_scored + 1, place)
    )

  totals <- scored |>
    group_by(team) |>
    summarise(points = sum(scored_place), .groups = "drop") |>
    arrange(points) |>
    mutate(rank = row_number())

  our_rank    <- totals$rank[totals$team == team_num][1]
  medal_cut   <- yml$Tournament$medals   %||% 0
  trophy_cut  <- yml$Tournament$trophies %||% 0
  tournament  <- yml$Tournament$`short name` %||% yml$Tournament$name %||%
    paste(yml$Tournament$state, yml$Tournament$level)
  level <- yml$Tournament$level
  year  <- yml$Tournament$year

  our_placings <- placings_raw |> filter(team == team_num, !is.na(place))

  summary_row <- tibble(
    file         = fname,
    tournament   = tournament,
    level        = level,
    year         = year,
    division     = yml$Tournament$division,
    rank         = our_rank,
    n_teams      = nrow(totals),
    trophy_cut   = trophy_cut,
    trophy       = !is.na(our_rank) && our_rank <= trophy_cut,
    event_medals = sum(our_placings$place >= 1 & our_placings$place <= medal_cut,
                        na.rm = TRUE)
  )

  event_rows <- our_placings |>
    transmute(
      tournament = tournament,
      level      = level,
      year       = year,
      event      = event,
      place      = place,
      medal      = !is.na(place) & place >= 1 & place <= medal_cut
    )

  list(summary = summary_row, events = event_rows)
}

raw_results <- map(candidate_files, get_team_result) |> compact()

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