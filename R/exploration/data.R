# Datasets behind the exploratory plots. Each function takes the tables it
# needs (from R/data/get-data.R) and returns one tidy dataset.

#' National revision shares per test year: higher, lower, and raised as
#' shares of pupils (with _lo/_hi bounds), and compliance = raised / higher.
#' @param reconsiderations advice_reconsiderations.
compliance_by_year <- function(reconsiderations) {
    reconsiderations |>
        filter(!is.na(raised)) |>
        summarise(
            pupils = sum(total),
            compliance = sum(raised) / sum(higher),
            higher_est = sum(higher) / pupils,
            lower_est = sum(lower) / pupils,
            raised_est = sum(raised) / pupils,
            across(matches("_(lo|hi)$"), \(x) sum(x) / pupils),
            .by = test_year
        ) |>
        arrange(test_year)
}

#' One row per test year and measure (est, lo, hi), with `segment` splitting
#' each measure at years without a test, so lines break there.
#' @param compliance Output of compliance_by_year().
compliance_measures <- function(compliance) {
    compliance |>
        pivot_longer(
            cols = matches("^(higher|lower|raised)_(est|lo|hi)$"),
            names_to = c("type", ".value"),
            names_pattern = "(higher|lower|raised)_(est|lo|hi)"
        ) |>
        mutate(
            measure = factor(
                recode(
                    type,
                    higher = "test advice higher", lower = "test advice lower",
                    raised = "advice raised"
                ),
                levels = names(measure_colours)
            )
        ) |>
        arrange(measure, test_year) |>
        mutate(
            segment = interaction(
                measure, cumsum(c(1, diff(test_year) > 1))
            ),
            .by = measure
        )
}

#' Tests and modes with their own CvTE tables, per test year.
cvte_tests <- function(conditional) {
    distinct(filter(conditional, test != "national"), test_year, test)
}

#' DUO gap matrix as one row per provisional and test advice.
gap_cells <- function(gap_matrix) {
    as_tibble(gap_matrix, rownames = "provisional") |>
        pivot_longer(-provisional, names_to = "test_advice", values_to = "gap")
}

#' National provisional advice distribution m(i) per test year: the per-test
#' distributions weighted by pupils per test.
#' @return Tibble with test_year, provisional, and m (sums to 1 per year).
national_provisional_mix <- function(provisional, pupils, conditional) {
    provisional |>
        semi_join(cvte_tests(conditional), by = c("test_year", "test")) |>
        inner_join(pupils, by = c("test_year", "test")) |>
        summarise(
            m = weighted.mean(pct, pupils) / 100,
            .by = c(test_year, provisional)
        ) |>
        mutate(m = m / sum(m), .by = test_year)
}

#' P(test advice j | provisional advice i) per test year and test, with the
#' national mix m(i) and the DUO gap for each cell.
conditional_cells <- function(conditional, mix, gap_matrix) {
    conditional |>
        mutate(p = pct / sum(pct), .by = c(test_year, test, provisional)) |>
        inner_join(mix, by = c("test_year", "provisional")) |>
        inner_join(gap_cells(gap_matrix), by = c("provisional", "test_advice"))
}

#' National joint distribution of provisional and test advice per test year:
#' share = m(i) P(j | i). Stops if advice labels do not match the shared
#' provisional_levels / test_advice_levels.
joint_distribution <- function(conditional, mix, gap_matrix) {
    joint <- conditional |>
        filter(test == "national") |>
        conditional_cells(mix, gap_matrix) |>
        mutate(share = m * p)
    unknown <- c(
        setdiff(joint$provisional, provisional_levels),
        setdiff(joint$test_advice, test_advice_levels)
    )
    if (length(unknown)) {
        stop(
            "Advice labels not in provisional_levels/test_advice_levels: ",
            paste(unknown, collapse = ", "),
            ". Restart R or re-source R/data/tidy-duo.R."
        )
    }
    joint
}

#' Revision shares per test and mode, standardised to the national
#' provisional mix: h = sum m(i) P(j | i) over cells where the test advice is
#' higher (gap > 0), l the same for lower, leniency = h - l, and
#' disagreement = h + l.
standardize_tests <- function(conditional, mix, gap_matrix) {
    conditional |>
        semi_join(cvte_tests(conditional), by = c("test_year", "test")) |>
        conditional_cells(mix, gap_matrix) |>
        summarise(
            higher = sum(m * p * (gap > 0)),
            lower = sum(m * p * (gap < 0)),
            .by = c(test_year, test)
        ) |>
        arrange(test_year, test) |>
        mutate(leniency = higher - lower, disagreement = higher + lower)
}
