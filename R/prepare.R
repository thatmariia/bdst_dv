read_duo <- function(path) {
  readr::read_csv(
    path,
    col_types = readr::cols(.default = readr::col_character()),
    progress = FALSE, num_threads = 1
  ) |>
    dplyr::filter(TYPE_PO == "BO", PEILJAAR %in% c("2022", "2023")) |>
    dplyr::mutate(
      school_id = paste0(INSTELLINGSCODE, VESTIGINGSCODE),
      year = as.integer(PEILJAAR),
      # DUO's -1 denotes a suppressed count, never minus one pupil.
      pupils = dplyr::na_if(as.integer(AANTAL_LEERLINGEN), -1L)
    )
}

prepare_population <- function(path) {
  raw <- readr::read_csv(path, show_col_types = FALSE) |>
    dplyr::filter(
      Perioden == "2022JJ00", Geslacht == "T001038",
      grepl("^PC[1-9][0-9]{3}$", trimws(Postcode))
    ) |>
    dplyr::mutate(pc4 = sub("^PC", "", trimws(Postcode)))
  residents <- raw |>
    dplyr::filter(Migratieachtergrond == "T001040") |>
    dplyr::transmute(pc4, residents = Bevolking_1)
  background <- raw |>
    dplyr::filter(Migratieachtergrond == "2012657") |>
    dplyr::transmute(pc4, background = Bevolking_1)
  stopifnot(!anyDuplicated(residents$pc4), !anyDuplicated(background$pc4))
  dplyr::left_join(residents, background, by = "pc4") |>
    dplyr::mutate(share = dplyr::if_else(
      residents >= 50 & background >= 0 & background <= residents,
      background / residents, NA_real_
    ))
}

prepare_data <- function(paths) {
  population <- prepare_population(paths[["population"]])
  origins <- read_duo(paths[["origins"]]) |>
    dplyr::transmute(school_id, year, pc4 = POSTCODE4_DEELNEMER, pupils)
  totals <- read_duo(paths[["totals"]]) |>
    dplyr::transmute(school_id, year, enrollment = pupils)
  stopifnot(
    !anyDuplicated(origins[c("school_id", "year", "pc4")]),
    !anyDuplicated(totals[c("school_id", "year")])
  )

  # Only exact counts with a usable CBS match contribute to composition.
  observed <- origins |>
    dplyr::left_join(population, by = "pc4") |>
    dplyr::group_by(school_id, year) |>
    dplyr::summarise(
      known_pupils = sum(pupils[!is.na(share)], na.rm = TRUE),
      composition = sum(pupils * share, na.rm = TRUE) /
        dplyr::na_if(known_pupils, 0),
      .groups = "drop"
    )
  schools <- totals |>
    dplyr::left_join(observed, by = c("school_id", "year")) |>
    dplyr::mutate(
      known_pupils = dplyr::coalesce(known_pupils, 0),
      coverage = known_pupils / dplyr::na_if(enrollment, 0L),
      eligible = !is.na(coverage) & coverage >= 0.8 & known_pupils >= 50
    )
  stopifnot(all(schools$coverage <= 1, na.rm = TRUE))
  eligible <- dplyr::filter(schools, eligible)
  pairs <- dplyr::inner_join(
    dplyr::filter(eligible, year == 2022),
    dplyr::filter(eligible, year == 2023),
    by = "school_id", suffix = c("_2022", "_2023")
  ) |>
    dplyr::mutate(change = composition_2023 - composition_2022)
  list(schools = schools, pairs = pairs)
}
