#' Turn the tidy DUO tables into the analysis tables (conventions D1-D7), with
#' only the columns the study uses. Every table is keyed by school and test
#' year; counts are n (midpoint) with bounds n_lo and n_hi.
school_key <- c("institution_code", "location_code", "test_year")
counts <- c("n", "n_lo", "n_hi")

#' D1: regular schools only. D2: key by test year (reference_year + 1).
#' D3: the midpoint becomes the plain count (n_mid -> n, total_mid -> total).
finalise_duo <- function(data) {
    data |>
        filter(school_type == "regular") |>
        select(-school_type, -reference_year, -any_of("suppressed")) |>
        rename_with(~ sub("_mid$", "", .x), ends_with("_mid"))
}

#' D4: revision counts; school-years with N suppressed or zero are dropped,
#' so total is exact.
filter_reconsiderations <- function(reconsiderations) {
    finalise_duo(reconsiderations) |>
        filter(total_lo == total_hi, total > 0) |>
        select(
            all_of(school_key), total, higher, higher_lo, higher_hi,
            lower, lower_lo, lower_hi, raised, raised_lo, raised_hi
        )
}

#' D5: tests used and pupils tested per test, grade 8 only.
filter_test_scores <- function(test_scores) {
    finalise_duo(test_scores) |>
        filter(grade == 8L) |>
        select(all_of(school_key), provider, all_of(counts))
}

#' D5: main provider and its share of tested pupils.
filter_main_provider <- function(main_provider) {
    finalise_duo(main_provider) |>
        select(all_of(school_key), provider, main_share)
}

#' D6: the ten teacher categories; special secondary and "no advice
#' possible" are excluded.
filter_final_advice <- function(final_advice) {
    finalise_duo(final_advice) |>
        filter(advice %in% provisional_levels) |>
        mutate(advice = factor(advice, levels = provisional_levels)) |>
        select(all_of(school_key), advice, all_of(counts))
}

#' D7: institutions with a single location in that year, to be merged on
#' institution_code and test_year.
filter_school_weighting <- function(school_weighting) {
    school_weighting |>
        filter(n() == 1, .by = c(institution_code, test_year)) |>
        select(institution_code, test_year, school_weighting, spread, pupils)
}

#' D1: regular school locations only.
filter_school_locations <- function(school_locations) {
    school_locations |>
        filter(school_type == "regular") |>
        select(-school_type)
}
