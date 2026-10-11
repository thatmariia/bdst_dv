# Separating teacher error from test error with Cito's resit simulation.

#' Share of pupils whose test result changes category on a resit with test
#' error sd `sd_error`: one minus @eq-resit-probability-same, averaged over
#' the latent test results T* ~ N(mean, sd^2) of the reference population
#' (left side of @eq-resit-share).
resit_flip_share <- function(sd_error, tau, mean, sd) {
    t_star <- seq(mean - 8 * sd, mean + 8 * sd, length.out = 8001)
    weight <- dnorm(t_star, mean, sd)
    weight <- weight / sum(weight)
    edges <- c(-Inf, tau, Inf)
    category <- findInterval(t_star, tau, left.open = TRUE) + 1
    same <- pnorm((edges[category + 1] - t_star) / sd_error) -
        pnorm((edges[category] - t_star) / sd_error)
    sum(weight * (1 - same))
}

#' Test error sd of the reference test that reproduces Cito's resit share
#' (@eq-resit-share), found with Brent's method (uniroot()).
#' @param flip Cito's share of changed test advices r, as a proportion.
anchor_test_error <- function(flip, tau, mean, sd) {
    uniroot(
        function(sd_error) resit_flip_share(sd_error, tau, mean, sd) - flip,
        interval = c(1e-4, 6), tol = 1e-10
    )$root
}

#' Original parameters of every test from a fitted year: the anchor gives
#' the reference test's error and loading and the teacher error
#' (@eq-loading-teacher-error), and @eq-original-parameters gives the rest.
#' The test scale is then converted to alpha_ref = 1 by dividing alpha_t,
#' beta_t, sigma_T,t, and tau by alpha_ref.
#' @param fit Output of fit_year().
#' @param flip Cito's share of changed test advices r, as a proportion.
#' @return List with the teacher error variance var_L, the anchor in
#'   estimation units, the thresholds, and per test mu, var_theta, alpha,
#'   beta, and sd_T.
structural_parameters <- function(fit, flip) {
    p <- fit$parameters
    ref <- p$reduced[[reference_test]]
    s_ref <- test_spread(ref)
    ref_test_sd <- anchor_test_error(
        flip, p$tau, ref$a + ref$b * ref$mu, s_ref
    )
    alpha_ref <- (s_ref^2 - ref_test_sd^2) / (ref$b * ref$sigma^2)
    teacher_var <- ref$sigma^2 - ref$b * ref$sigma^2 / alpha_ref
    if (teacher_var <= 0) stop("Negative variance for: teacher error")

    tests <- bind_rows(
        lapply(
            names(p$reduced),
            function(test) {
                reduced <- p$reduced[[test]]
                var_theta <- reduced$sigma^2 - teacher_var
                alpha <- reduced$b * reduced$sigma^2 / var_theta
                beta <- reduced$a + reduced$b * reduced$mu - alpha * reduced$mu
                test_var <- test_spread(reduced)^2 - alpha^2 * var_theta
                tibble(
                    test = test, mu = reduced$mu, var_theta = var_theta,
                    alpha = alpha / alpha_ref, beta = beta / alpha_ref,
                    var_T = test_var / alpha_ref^2
                )
            }
        )
    )
    if (any(tests$var_theta <= 0 | tests$var_T < 0)) {
        stop(
            "Negative variance for: ", paste(
                tests$test[tests$var_theta <= 0 | tests$var_T < 0],
                collapse = ", "
            )
        )
    }
    list(
        var_L = teacher_var,
        sd_T_ref_estimation = ref_test_sd,
        alpha_ref_estimation = alpha_ref,
        lambda = p$lambda,
        tau = p$tau / alpha_ref,
        tests = mutate(tests, sd_T = sqrt(var_T), .keep = "unused")
    )
}

#' Anchor per year: the reference test's error sd and loading in estimation
#' units, and the teacher error variance.
#' @param structural Named list (by year) of structural_parameters() results.
anchor_summary <- function(structural) {
    rows <- lapply(
        structural,
        function(s) {
            tibble(
                sd_T_ref = s$sd_T_ref_estimation,
                alpha_ref = s$alpha_ref_estimation,
                var_L = s$var_L
            )
        }
    )
    bind_rows(rows, .id = "test_year") |>
        mutate(test_year = as.integer(test_year))
}

#' Original parameters of every test and year in one table.
#' @param structural Named list (by year) of structural_parameters() results.
original_parameters <- function(structural) {
    tests <- bind_rows(lapply(structural, `[[`, "tests"), .id = "test_year")
    mutate(tests, test_year = as.integer(test_year))
}
