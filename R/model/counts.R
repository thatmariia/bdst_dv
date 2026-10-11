# Pseudo counts behind the fit: the published CvTE shares turned into
# numbers of pupils per provisional advice i and test result j.

#' Pseudo counts n_t(i, j) = N_t f_y J_t(i, j) (@eq-model-counts), with the
#' joint distribution J_t(i, j) = m_t(i) P_t(j | i) of each test's own
#' population (@eq-joint).
#' @param conditional cvte_conditional: P_t(j | i) in percent.
#' @param provisional cvte_provisional: m_t(i) in percent.
#' @param pupils cvte_pupils: N_t, pupils per test.
#' @param coverage cvte_coverage: f_y, share of pupils with a known
#'   provisional advice.
#' @return One row per test year, test, provisional advice, and test result,
#'   with the joint share and the pseudo count n.
pseudo_counts <- function(conditional, provisional, pupils, coverage) {
    test_mix <- provisional |>
        mutate(m = pct / sum(pct), .by = c(test_year, test)) |>
        select(test_year, test, provisional, m)

    conditional |>
        filter(test != "national") |>
        mutate(p = pct / sum(pct), .by = c(test_year, test, provisional)) |>
        select(test_year, test, provisional, test_advice, p) |>
        inner_join(test_mix, by = c("test_year", "test", "provisional")) |>
        inner_join(pupils, by = c("test_year", "test")) |>
        inner_join(coverage, by = "test_year") |>
        mutate(
            provisional = factor(provisional, levels = provisional_levels),
            test_advice = factor(test_advice, levels = test_advice_levels),
            joint = m * p,
            n = pupils * share * joint
        ) |>
        select(test_year, test, provisional, test_advice, joint, n) |>
        arrange(test_year, test, provisional, test_advice)
}

#' Pseudo counts of one year as 10 x 6 matrices (provisional advice by test
#' result), one per test, as the likelihood uses them.
#' @param counts Output of pseudo_counts().
#' @param year Test year.
#' @return Named list of count matrices.
count_matrices <- function(counts, year) {
    year_counts <- filter(counts, test_year == year)
    lapply(
        split(year_counts, year_counts$test),
        function(test_counts) {
            counts <- xtabs(n ~ provisional + test_advice, data = test_counts)
            structure(unclass(counts), call = NULL)
        }
    )
}
