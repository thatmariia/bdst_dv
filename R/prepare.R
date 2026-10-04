#' Summarise suppression in age-four counts, used as an intake proxy.
#' @param path DUO age-by-postcode CSV path.
#' @return One row with positive cells, suppressed cells, and their share.
check_intake <- function(path) {
    raw <- read_delim(
        path,
        delim = ";",
        col_types = cols(.default = col_character()),
        progress = FALSE, num_threads = 1
    ) |>
        filter(SOORT_PO == "Bo")
    counts <- raw$LEEFTIJD_4
    stopifnot(all(counts == "<5" | grepl("^[0-9]+$", counts)))
    positive <- counts != "0"
    data.frame(
        positive_cells = sum(positive),
        suppressed_cells = sum(counts == "<5"),
        suppressed_share = sum(counts == "<5") / sum(positive)
    )
}

#' Read regular primary-school counts for 2022 and 2023.
#' @param path DUO enrolment or school-total CSV path.
#' @return Raw columns plus school_id, year, and pupils; suppression is NA.
read_duo <- function(path) {
    read_csv(
        path,
        col_types = cols(.default = col_character()),
        progress = FALSE, num_threads = 1
    ) |>
        filter(TYPE_PO == "BO", PEILJAAR %in% c("2022", "2023")) |>
        mutate(
            school_id = paste0(INSTELLINGSCODE, VESTIGINGSCODE),
            year = as.integer(PEILJAAR),
            # DUO's -1 denotes a suppressed count, never minus one pupil.
            pupils = na_if(as.integer(AANTAL_LEERLINGEN), -1L)
        )
}

#' Calculate neighbourhood composition from CBS population counts.
#' @param path Cached CBS population CSV path.
#' @return One row per PC4, with counts and a share for usable postcodes.
prepare_population <- function(path) {
    raw <- read_csv(path, show_col_types = FALSE) |>
        filter(
            Perioden == "2022JJ00", Geslacht == "T001038",
            grepl("^PC[1-9][0-9]{3}$", trimws(Postcode))
        ) |>
        mutate(pc4 = sub("^PC", "", trimws(Postcode)))
    residents <- raw |>
        filter(Migratieachtergrond == "T001040") |>
        transmute(pc4, residents = Bevolking_1)
    background <- raw |>
        filter(Migratieachtergrond == "2012657") |>
        transmute(pc4, background = Bevolking_1)
    stopifnot(!anyDuplicated(residents$pc4), !anyDuplicated(background$pc4))
    left_join(residents, background, by = "pc4") |>
        mutate(share = if_else(
            residents >= 50 & background >= 0 & background <= residents,
            background / residents, NA_real_
        ))
}

#' Join pupil origins to neighbourhoods and apply the coverage rules.
#' @param paths Named source paths returned by download_data().
#' @return A list of school-year summaries and eligible two-year pairs.
prepare_data <- function(paths) {
    population <- prepare_population(paths[["population"]])
    origins <- read_duo(paths[["origins"]]) |>
        transmute(school_id, year, pc4 = POSTCODE4_DEELNEMER, pupils)
    totals <- read_duo(paths[["totals"]]) |>
        transmute(school_id, year, enrollment = pupils)
    stopifnot(
        !anyDuplicated(origins[c("school_id", "year", "pc4")]),
        !anyDuplicated(totals[c("school_id", "year")])
    )

    # Only exact counts with a usable CBS match contribute to composition.
    observed <- origins |>
        left_join(population, by = "pc4") |>
        group_by(school_id, year) |>
        summarise(
            known_pupils = sum(pupils[!is.na(share)], na.rm = TRUE),
            composition = sum(pupils * share, na.rm = TRUE) /
                na_if(known_pupils, 0),
            .groups = "drop"
        )
    schools <- totals |>
        left_join(observed, by = c("school_id", "year")) |>
        mutate(
            known_pupils = coalesce(known_pupils, 0),
            coverage = known_pupils / na_if(enrollment, 0L),
            eligible = !is.na(coverage) & coverage >= 0.8 & known_pupils >= 50
        )
    stopifnot(all(schools$coverage <= 1, na.rm = TRUE))
    eligible <- filter(schools, eligible)
    pairs <- inner_join(
        filter(eligible, year == 2022),
        filter(eligible, year == 2023),
        by = "school_id", suffix = c("_2022", "_2023")
    ) |>
        mutate(change = composition_2023 - composition_2022)
    list(schools = schools, pairs = pairs)
}
