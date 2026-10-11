# Decomposition of eligibility into category design, teacher error, test
# error, and test menu (@sec-decomposition).

# Sources that can be switched on, in display order.
decomposition_sources <- c("teacher_error", "test_error", "test_menu")

#' National population on a grid of the common factor: the mixture of the
#' test populations N(mu_t, var_theta_t), weighted by pupils per test N_t.
#' @param structure Output of structural_parameters().
#' @param pupils cvte_pupils.
#' @param year Test year.
#' @return List with the grid theta and its weights (summing to 1).
national_population <- function(structure, pupils, year) {
    tests <- inner_join(
        structure$tests, filter(pupils, test_year == year),
        by = "test"
    )
    sd_theta <- sqrt(tests$var_theta)
    theta <- seq(
        min(tests$mu - 8 * sd_theta), max(tests$mu + 8 * sd_theta),
        length.out = 6001
    )
    density <- Reduce(
        `+`, Map(
            function(mu, sd, n) n * dnorm(theta, mu, sd),
            tests$mu, sd_theta, tests$pupils
        )
    )
    list(theta = theta, weight = density / sum(density))
}

#' Probability of each category for readings with the given means and error
#' sd; with sd = 0 the reading is exact.
#' @return Matrix with one row per mean and one column per category.
category_probabilities <- function(thresholds, mean, sd) {
    below <- if (sd == 0) {
        outer(mean, thresholds, function(m, edge) as.numeric(m <= edge))
    } else {
        pnorm(outer(mean, thresholds, function(m, edge) (edge - m) / sd))
    }
    below <- cbind(0, below, 1)
    below[, -1] - below[, -ncol(below)]
}

#' Shares of the national population per provisional advice and test result
#' when the sources in `on` are switched on (@eq-category-probabilities):
#' teacher error (sigma_L, else 0), test error (sigma_T,t, else 0), and test
#' menu (the test's own parameters, else the reference test's).
#' @param test_name Test taken.
#' @param on Character vector of switched-on sources.
#' @return 10 x 6 matrix, provisional advice by test result.
scenario_cells <- function(population, structure, test_name, on) {
    taken <- if ("test_menu" %in% on) test_name else reference_test
    parameters <- filter(structure$tests, test == taken)
    teacher_sd <- if ("teacher_error" %in% on) sqrt(structure$var_L) else 0
    test_sd <- if ("test_error" %in% on) parameters$sd_T else 0
    teacher <- category_probabilities(
        structure$lambda, population$theta, teacher_sd
    )
    test_result <- category_probabilities(
        structure$tau, parameters$alpha * population$theta + parameters$beta,
        test_sd
    )
    cells <- crossprod(teacher * population$weight, test_result)
    dimnames(cells) <- list(provisional_levels, test_advice_levels)
    cells
}

#' Share of eligible pupils (G(i, j) > 0) in a table of cell shares
#' (@eq-eligibility).
eligibility <- function(cells, gap) sum(cells[gap > 0])

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

#' All subsets of a set, including the empty one.
all_subsets <- function(x) {
    c(
        list(character(0)),
        unlist(
            lapply(seq_along(x), function(k) combn(x, k, simplify = FALSE)),
            recursive = FALSE
        )
    )
}

#' Name of a set of players or sources, e.g. "test_error+test_menu".
subset_key <- function(set) {
    if (length(set) == 0) "none" else paste(sort(set), collapse = "+")
}

#' Shapley values (@eq-shapley): each player's added value, averaged over all
#' orders in which the players can be switched on.
#' @param players Character vector of players.
#' @param value Function of a set of players (character vector).
#' @return Named vector with one value per player.
shapley_values <- function(players, value) {
    subsets <- all_subsets(players)
    values <- setNames(
        vapply(subsets, value, numeric(1)), sapply(subsets, subset_key)
    )
    n <- length(players)
    vapply(
        players,
        function(player) {
            before <- all_subsets(setdiff(players, player))
            gains <- vapply(
                before,
                function(set) {
                    values[[subset_key(c(set, player))]] -
                        values[[subset_key(set)]]
                },
                numeric(1)
            )
            weights <- vapply(
                before,
                function(set) {
                    factorial(length(set)) * factorial(n - length(set) - 1) /
                        factorial(n)
                },
                numeric(1)
            )
            sum(weights * gains)
        },
        numeric(1)
    )
}

#' The cells of every set of switched-on sources, named by subset_key().
#' @param cells_of Function of a set of sources that returns their cells.
subset_cells <- function(sources, cells_of) {
    subsets <- all_subsets(sources)
    setNames(lapply(subsets, cells_of), vapply(subsets, subset_key, ""))
}

#' Decomposition (@eq-decomposition) of a share of eligible pupils: category
#' design (no source switched on), the Shapley value of every source, and
#' the total.
#' @param cells Output of subset_cells().
#' @param share Function of cells that returns the share of eligible pupils.
decompose_cells <- function(cells, sources, share) {
    value <- function(on) share(cells[[subset_key(on)]])
    contributions <- shapley_values(sources, value)
    result <- c(
        category_design = value(character(0)), contributions,
        eligible = value(sources)
    )
    # the parts add up to the total by construction
    parts <- result[["category_design"]] + sum(contributions)
    stopifnot(isTRUE(all.equal(parts, result[["eligible"]])))
    result
}

#' Decomposition of every test of one year, among all pupils ("all") or
#' among the pupils with each provisional advice in `advice`, if the whole
#' national population took the test.
#' @param structure Output of structural_parameters().
#' @param pupils cvte_pupils.
#' @param gap DUO gap matrix.
#' @param year Test year.
#' @return One row per test and advice, in shares of the pupils.
decompose_national <- function(structure, pupils, gap, year,
                               advice = "all") {
    population <- national_population(structure, pupils, year)
    rows <- lapply(
        structure$tests$test,
        function(test) {
            cells <- subset_cells(
                decomposition_sources,
                \(on) scenario_cells(population, structure, test, on)
            )
            lapply(
                advice,
                function(among) {
                    shares <- decompose_cells(
                        cells, decomposition_sources,
                        \(table) eligibility_among(table, gap, among)
                    )
                    tibble(test = test, advice = among, !!!as.list(shares))
                }
            )
        }
    )
    bind_rows(unlist(rows, recursive = FALSE)) |>
        mutate(test_year = as.integer(year), .before = 1)
}

#' National decomposition per year: the decomposition of every test,
#' weighted by the number of pupils that took it, i.e., with the tests that
#' schools actually chose.
#' @param decomposition Output of decompose_national() for all years.
#' @param pupils cvte_pupils.
national_decomposition <- function(decomposition, pupils) {
    decomposition |>
        inner_join(pupils, by = c("test_year", "test")) |>
        summarise(
            across(
                c(
                    category_design, teacher_error, test_error, test_menu,
                    eligible
                ),
                \(x) weighted.mean(x, pupils)
            ),
            .by = test_year
        )
}
