# The reduced model's cell probabilities and log-likelihood.

# Reference test: fixes the latent scales (@sec-measurement-model).
reference_test <- "Lib paper"

#' Starting values for the free parameters: evenly spread thresholds and
#' the same reduced-form parameters for every test.
start_parameters <- function(tests) {
    c(
        -2.2, rep(log(0.45), 8), # teacher thresholds: first one, log gaps
        -1.3, rep(log(0.7), 4), # test thresholds: first one, log gaps
        0.8, # slope of the reference test
        # mu, log sigma, a, b, and log psi of every other test
        rep(c(0, 0, 0, 0.8, 0), length(tests) - 1)
    )
}

#' Thresholds and per-test reduced-form parameters from the free parameters.
#' The thresholds are the first value plus the exponentiated gaps; the
#' reference test is fixed at mu = 0, sigma = 1, a = 0, and psi = 1
#' (estimation units), with only its slope b free.
#' @param par Free parameters, as in start_parameters().
#' @param tests Test names of the year.
#' @return List with lambda, tau, and reduced: per test, a list of mu,
#'   sigma, a, b, and psi.
unpack_parameters <- function(par, tests) {
    others <- setdiff(tests, reference_test)
    per_test <- matrix(par[-(1:15)], ncol = 5, byrow = TRUE)
    reduced <- lapply(
        seq_along(others),
        function(k) {
            list(
                mu = per_test[k, 1], sigma = exp(per_test[k, 2]),
                a = per_test[k, 3], b = per_test[k, 4],
                psi = exp(per_test[k, 5])
            )
        }
    )
    names(reduced) <- others
    reduced[[reference_test]] <- list(
        mu = 0, sigma = 1, a = 0, b = par[15], psi = 1
    )
    list(
        lambda = par[1] + c(0, cumsum(exp(par[2:9]))),
        tau = par[10] + c(0, cumsum(exp(par[11:14]))),
        reduced = reduced[tests]
    )
}

#' Spread s_t of the latent test result and its correlation rho_t with the
#' latent provisional advice (@eq-reduced-variance-correlation).
test_spread <- function(reduced) {
    sqrt(reduced$b^2 * reduced$sigma^2 + reduced$psi^2)
}
test_correlation <- function(reduced) {
    reduced$b * reduced$sigma / test_spread(reduced)
}

#' Cell probabilities q_t(i, j) of one test (@eq-model-probability).
#' @param reduced Reduced-form parameters of the test (mu, sigma, a, b, psi).
#' @param lambda,tau Teacher and test thresholds.
#' @return 10 x 6 matrix, provisional advice by test result.
cell_probabilities <- function(reduced, lambda, tau) {
    s <- test_spread(reduced)
    # standardized thresholds (@eq-standardized-thresholds); +-38 stands in
    # for infinity, which pbivnorm cannot take
    u <- c(-38, (lambda - reduced$mu) / reduced$sigma, 38)
    v <- c(-38, (tau - reduced$a - reduced$b * reduced$mu) / s, 38)
    corners <- outer(
        u, v,
        function(x, y) {
            pbivnorm::pbivnorm(x, y, test_correlation(reduced))
        }
    )
    # inclusion-exclusion: upper-right corner, minus the strips to the left
    # and below, plus the lower-left corner that was subtracted twice
    n_u <- length(u)
    n_v <- length(v)
    q <- corners[-1, -1] - corners[-n_u, -1] - corners[-1, -n_v] +
        corners[-n_u, -n_v]
    pmax(q, 1e-12) # keeps log() finite
}

#' Negative log-likelihood (@eq-log-likelihood) of all tests in one year.
#' @param par Free parameters.
#' @param counts Named list of count matrices (count_matrices()).
negative_log_likelihood <- function(par, counts) {
    parameters <- unpack_parameters(par, names(counts))
    log_likelihoods <- vapply(
        names(counts),
        function(test) {
            q <- cell_probabilities(
                parameters$reduced[[test]], parameters$lambda, parameters$tau
            )
            sum(counts[[test]] * log(q))
        },
        numeric(1)
    )
    -sum(log_likelihoods)
}
