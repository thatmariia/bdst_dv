#' Keep only the rows and columns the study uses. School keys and the
#' n_lo/n_mid/n_hi suppression bounds are always kept.
counts <- c("n_lo", "n_mid", "n_hi")

#' @param test_scores Output of tidy_test_scores().
#' @return Grade-8 rows with provider and pupil counts.
select_test_scores <- function(test_scores) {
    test_scores |>
        filter(grade == 8L) |>
        select(all_of(school_year_key), provider, all_of(counts))
}

#' @param final_advice Output of tidy_final_advice().
#' @return Advice and pupil counts.
select_final_advice <- function(final_advice) {
    select(final_advice, all_of(school_year_key), advice, all_of(counts))
}

#' @param reference_levels Output of tidy_reference_levels().
#' @return Provider, subject, level, and pupil counts.
select_reference_levels <- function(reference_levels) {
    select(
        reference_levels,
        all_of(school_year_key), provider, subject, level, all_of(counts)
    )
}

#' @param school_weighting Output of tidy_school_weighting().
#' @return Mean weighting, spread, and pupil count per school and year.
select_school_weighting <- function(school_weighting) {
    select(
        school_weighting,
        institution_code, reference_year, test_year,
        school_weighting, spread, pupils
    )
}
