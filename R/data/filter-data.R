#' Keep only the rows and columns the study uses (conventions D1-D8). After
#' D1 every school is regular, so school_type is dropped.
school_key <- c(
    "institution_code", "location_code", "reference_year", "test_year"
)
counts <- c("n_lo", "n_mid", "n_hi")

#' D1: regular primary schools only.
regular_only <- function(data) {
    data |>
        filter(school_type == "regular") |>
        select(-school_type)
}

#' D4-D5: drop school-years with N suppressed or zero, and add the main
#' provider. in_provider_analysis marks main share >= 0.9 and N >= 10.
#' @param reconsiderations Output of tidy_advice_reconsiderations().
#' @param providers Output of main_providers().
#' @return One row per school-year with exact total and bounded counts.
filter_reconsiderations <- function(reconsiderations, providers) {
    regular_only(reconsiderations) |>
        filter(total_lo == total_hi, total_mid > 0) |>
        mutate(total = total_mid, .keep = "unused", .after = test_year) |>
        select(-total_lo, -total_hi) |>
        left_join(
            select(regular_only(providers), -n_tested),
            by = school_key
        ) |>
        mutate(
            in_provider_analysis = coalesce(main_share >= 0.9, FALSE) &
                total >= 10
        )
}

#' D5: tests used, grade 8 only.
filter_test_scores <- function(test_scores) {
    regular_only(test_scores) |>
        filter(grade == 8L) |>
        select(all_of(school_key), provider, all_of(counts))
}

#' D6: the ten teacher categories; special secondary and "no advice
#' possible" are excluded.
filter_final_advice <- function(final_advice) {
    regular_only(final_advice) |>
        filter(advice %in% provisional_levels) |>
        mutate(advice = factor(advice, levels = provisional_levels)) |>
        select(all_of(school_key), advice, all_of(counts))
}

#' D8: reading levels only.
filter_reading_levels <- function(reference_levels) {
    regular_only(reference_levels) |>
        filter(subject == "reading") |>
        select(all_of(school_key), provider, level, all_of(counts))
}

#' D7: institutions with a single location in that year, merged later on
#' institution_code and year.
filter_school_weighting <- function(school_weighting) {
    school_weighting |>
        filter(n() == 1, .by = c(institution_code, reference_year)) |>
        select(
            institution_code, reference_year, test_year,
            school_weighting, spread, pupils
        )
}
