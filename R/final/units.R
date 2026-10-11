# National results prepared for the eligibility explorer.

#' Share of eligible pupils among all pupils ("all") or among the pupils
#' with one provisional advice, in a table of cell shares.
#' @param advice "all" or a provisional advice.
eligibility_among <- function(cells, gap, advice) {
    if (advice == "all") {
        return(eligibility(cells, gap))
    }
    row <- cells[advice, ]
    sum(row[gap[advice, ] > 0]) / sum(row)
}

#' Decomposition (@eq-shapley) of one test, among all pupils and among the
#' pupils with each provisional advice, if the whole national population
#' took the test.
#' @param population Output of national_population().
#' @param structure Output of structural_parameters().
decompose_test_by_advice <- function(population, structure, test, gap) {
    key <- function(on) {
        if (length(on) == 0) "none" else paste(sort(on), collapse = "+")
    }
    subsets <- all_subsets(decomposition_sources)
    cells <- lapply(
        subsets, \(on) scenario_cells(population, structure, test, on)
    )
    names(cells) <- vapply(subsets, key, "")
    rows <- lapply(
        c("all", provisional_levels),
        function(advice) {
            value <- function(on) {
                eligibility_among(cells[[key(on)]], gap, advice)
            }
            tibble(
                test = test, advice = advice,
                category_design = value(character(0)),
                !!!as.list(shapley_values(decomposition_sources, value)),
                eligible = value(decomposition_sources)
            )
        }
    )
    bind_rows(rows)
}

#' National decomposition of every year and test, among all pupils and per
#' provisional advice (decompose_test_by_advice()).
#' @param structural Named list (by year) of structural_parameters() results.
#' @param pupils cvte_pupils.
#' @param gap DUO gap matrix.
decompose_by_advice <- function(structural, pupils, gap) {
    rows <- lapply(
        names(structural),
        function(year) {
            structure <- structural[[year]]
            population <- national_population(
                structure, pupils, as.integer(year)
            )
            bind_rows(
                lapply(
                    structure$tests$test, decompose_test_by_advice,
                    population = population, structure = structure, gap = gap
                )
            ) |>
                mutate(test_year = as.integer(year), .before = 1)
        }
    )
    bind_rows(rows)
}

#' National results of the explorer from the CvTE model, per year: the
#' decomposition of every test and of their pupil-weighted average, among
#' all pupils and per provisional advice; the observed test results per
#' provisional advice and the provisional advice itself (CvTE tables); and
#' the observed share of eligible pupils.
#' @param structural Named list (by year) of structural_parameters() results.
#' @param pupils cvte_pupils.
#' @param conditional cvte_conditional.
#' @param provisional cvte_provisional.
#' @param joint Output of joint_distribution().
#' @param gap DUO gap matrix.
national_units <- function(structural, pupils, conditional, provisional,
                           joint, gap) {
    by_test <- decompose_by_advice(structural, pupils, gap)
    # P(L = i) does not depend on the test, so the average over the tests
    # weights every test by its pupils, also per provisional advice
    average <- by_test |>
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
        decomposition = bind_rows(average, by_test),
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
