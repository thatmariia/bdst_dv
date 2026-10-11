# National results prepared for the eligibility explorer.

#' National results of the explorer from the CvTE model, per year: the
#' decomposition of every test and of their pupil-weighted average, among
#' all pupils and per provisional advice; the observed test results per
#' provisional advice and the provisional advice itself (CvTE tables); and
#' the observed share of eligible pupils.
#' @param by_advice decomposition_by_advice (R/national-model/setup.R).
#' @param pupils cvte_pupils.
#' @param conditional cvte_conditional.
#' @param provisional cvte_provisional.
#' @param joint Output of joint_distribution().
national_units <- function(by_advice, pupils, conditional, provisional,
                           joint) {
    # P(L = i) does not depend on the test, so the average over the tests
    # weights every test by its pupils, also per provisional advice
    average <- by_advice |>
        inner_join(pupils, by = c("test_year", "test")) |>
        summarise(
            across(
                c(
                    category_design, teacher_error, test_error, test_menu,
                    eligible
                ),
                \(x) weighted.mean(x, pupils)
            ),
            .by = c(test_year, advice)
        ) |>
        mutate(test = "national average")
    rename_national <- function(test) {
        if_else(test == "national", "national average", test)
    }
    list(
        decomposition = bind_rows(average, by_advice),
        results = conditional |>
            mutate(
                test = rename_national(test),
                share = pct / sum(pct), .by = c(test_year, test, provisional)
            ) |>
            select(test_year, test, provisional, test_advice, share),
        provisional = provisional |>
            mutate(
                test = rename_national(test), share = pct / sum(pct),
                .by = c(test_year, test)
            ) |>
            select(test_year, test, provisional, share),
        observed = joint |>
            filter(test == "national") |>
            summarise(higher = sum(share[gap > 0]), .by = test_year),
        pupils = filter(pupils, test == "national")
    )
}
