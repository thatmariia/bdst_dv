# Maximum likelihood fit of the reduced model, per year.

#' Fits all tests of one year jointly (@eq-log-likelihood) with L-BFGS-B,
#' restarted once from its own solution to make sure it has converged.
#' @param counts Named list of count matrices (count_matrices()).
#' @return List with the free parameters (par), the unpacked parameters,
#'   the log-likelihood, convergence, and the counts.
fit_year <- function(counts) {
    optimize <- function(start) {
        optim(
            start, negative_log_likelihood,
            counts = counts,
            method = "L-BFGS-B", control = list(maxit = 20000)
        )
    }
    fit <- optimize(optimize(start_parameters(names(counts)))$par)
    list(
        par = fit$par,
        parameters = unpack_parameters(fit$par, names(counts)),
        log_likelihood = -fit$value,
        converged = fit$convergence == 0,
        counts = counts
    )
}

#' Reduced-form estimates and dissimilarity index (@eq-dissimilarity-index)
#' per test of one fitted year.
#' @param fit Output of fit_year().
#' @return One row per test with mu, sigma, a, b, psi, and dissimilarity.
fit_summary <- function(fit) {
    p <- fit$parameters
    rows <- lapply(
        names(fit$counts),
        function(test) {
            observed <- fit$counts[[test]] / sum(fit$counts[[test]])
            fitted <- cell_probabilities(p$reduced[[test]], p$lambda, p$tau)
            tibble(
                test = test, !!!p$reduced[[test]],
                dissimilarity = sum(abs(fitted - observed)) / 2
            )
        }
    )
    bind_rows(rows)
}
