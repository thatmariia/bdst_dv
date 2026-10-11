# School-level version of the model (@sec-final-school-model). Every school s
# has its own pupils, theta ~ N(mu_s, sigma_s^2), and a teacher leniency
# delta_s that shifts its provisional advice: L* = theta + delta_s + error;
# delta_s = 0 is the teacher of the national model. The teacher error, the
# thresholds, and the parameters of the tests are the national ones of the
# same year.

# Grid of the common factor for the school-level integrals.
school_grid <- seq(-5, 5, length.out = 101)

# Sources that can be switched on at a school, in display order.
school_sources <- c("teacher_error", "leniency", "test_error", "test_menu")

# Tests of each provider in the DUO data: DUO does not say whether a school
# took the paper or the digital version.
provider_tests <- list(
    AMN = "AMN", DIA = "Dia", DOE = "DOE",
    IEP = c("IEP paper", "IEP digital"), LIB = c("Lib paper", "Lib digital"),
    "ROUTE 8" = "ROUTE 8"
)

#' A test as a mixture of versions with weights; for a provider, its paper
#' and digital versions weighted by their pupils nationally.
#' @param tests Names of the versions (CvTE tests).
#' @param structure Output of structural_parameters().
#' @param pupils cvte_pupils of the same year.
test_mixture <- function(tests, structure, pupils) {
    weight <- pupils$pupils[match(tests, pupils$test)]
    structure$tests |>
        filter(test %in% tests) |>
        arrange(match(test, tests)) |>
        mutate(weight = weight / sum(weight)) |>
        select(test, weight, alpha, beta, sd_T)
}

#' Probability of each test result for every theta of the grid, for a
#' mixture of tests, with or without test error.
test_result_probabilities <- function(mixture, tau, error = TRUE) {
    Reduce(
        `+`,
        Map(
            function(weight, alpha, beta, sd_t) {
                weight * category_probabilities(
                    tau, alpha * school_grid + beta, if (error) sd_t else 0
                )
            },
            mixture$weight, mixture$alpha, mixture$beta, mixture$sd_T
        )
    )
}

#' Shares of a school's pupils per provisional advice and test result
#' (10 x 6) when the sources in `on` are switched on: teacher error (sigma_L,
#' else 0), leniency (delta_s, else 0), test error (sigma_T, else 0), and
#' test menu (the school's test, else the reference test).
#' @param school List with mu, sigma, and delta.
#' @param model Output of school_model().
#' @param mixture The school's test, from test_mixture().
school_cells <- function(school, model, mixture, on = school_sources) {
    weight <- dnorm(school_grid, school$mu, school$sigma)
    weight <- weight / sum(weight)
    delta <- if ("leniency" %in% on) school$delta else 0
    teacher <- category_probabilities(
        model$lambda, school_grid + delta,
        if ("teacher_error" %in% on) model$teacher_sd else 0
    )
    test <- if ("test_menu" %in% on) mixture else model$reference
    result <- test_result_probabilities(
        test, model$tau, "test_error" %in% on
    )
    cells <- crossprod(teacher * weight, result)
    dimnames(cells) <- list(provisional_levels, test_advice_levels)
    cells
}

#' Expected final advice of a school: the provisional advice, raised to the
#' test result for the share `raised` of the pupils with a higher test
#' result.
final_advice_shares <- function(cells, gap, raised) {
    up <- gap > 0
    final <- rowSums(cells * (1 - raised * up))
    moved <- colSums(cells * raised * up)
    # the test results that can be higher are also teacher categories
    target <- match(colnames(gap), rownames(gap))
    final[target[moved > 0]] <- final[target[moved > 0]] + moved[moved > 0]
    final
}

#' National parameters of one year that every school shares.
#' @param structure Output of structural_parameters().
#' @param pupils cvte_pupils.
#' @param year Test year.
school_model <- function(structure, pupils, year) {
    pupils <- filter(pupils, test_year == year)
    list(
        year = year,
        lambda = structure$lambda,
        tau = structure$tau,
        teacher_sd = sqrt(structure$var_L),
        tests = structure$tests,
        mixtures = lapply(
            provider_tests, test_mixture,
            structure = structure,
            pupils = pupils
        ),
        versions = lapply(
            setNames(structure$tests$test, structure$tests$test),
            test_mixture,
            structure = structure, pupils = pupils
        ),
        reference = test_mixture(reference_test, structure, pupils),
        # offset of the final advice, set by estimate_schools()
        final_shift = 0
    )
}

#' Negative log posterior of a school's parameters (mu, log sigma, delta):
#' the final advice per category and the pupils with a higher, equal, or
#' lower test advice, plus a normal prior on the parameters. The final
#' advice is predicted with the national offset model$final_shift added to
#' the leniency (see estimate_schools()).
school_objective <- function(parameters, school, model, mixture, gap,
                             prior) {
    pupils <- list(
        mu = parameters[1], sigma = exp(parameters[2]),
        delta = parameters[3]
    )
    cells <- school_cells(pupils, model, mixture)
    higher <- sum(cells[gap > 0])
    lower <- sum(cells[gap < 0])
    shifted <- pupils
    shifted$delta <- pupils$delta + model$final_shift
    final <- final_advice_shares(
        school_cells(shifted, model, mixture), gap, school$raised_share
    )
    log_likelihood <- sum(school$advice * log(pmax(final, 1e-12))) +
        school$higher * log(higher) + school$lower * log(lower) +
        school$equal * log(max(1 - higher - lower, 1e-12))
    -log_likelihood + sum((parameters - prior$mean)^2 / (2 * prior$sd^2))
}

#' Fits every school: the posterior mode of (mu, log sigma, delta) and the
#' standard error of delta.
#' @param schools Output of school_data().
#' @param prior List with mean and sd of the three parameters.
fit_schools <- function(schools, model, gap, prior) {
    advice <- as.matrix(schools[provisional_levels])
    rows <- lapply(
        seq_len(nrow(schools)),
        function(k) {
            school <- list(
                advice = advice[k, ], higher = schools$higher[k],
                lower = schools$lower[k], equal = schools$equal[k],
                raised_share = schools$raised_share[k]
            )
            # bounded to plausible values: pupils within three standard
            # deviations of the national mean, sigma from 0.2 to 3, and a
            # leniency of at most two
            fit <- optim(
                prior$mean, school_objective,
                school = school, model = model,
                mixture = model$mixtures[[schools$provider[k]]], gap = gap,
                prior = prior, method = "L-BFGS-B",
                lower = c(-3, log(0.2), -2), upper = c(3, log(3), 2),
                hessian = TRUE
            )
            covariance <- tryCatch(solve(fit$hessian), error = \(e) NULL)
            tibble(
                mu = fit$par[1], log_sigma = fit$par[2], delta = fit$par[3],
                se_mu = sqrt(covariance[1, 1] %||% NA),
                se_log_sigma = sqrt(covariance[2, 2] %||% NA),
                se_delta = sqrt(covariance[3, 3] %||% NA)
            )
        }
    )
    bind_cols(select(schools, id), bind_rows(rows))
}

#' Empirical Bayes fit of all schools, in three steps.
#' 1. The larger schools (`large` pupils or more), fitted with a weak prior,
#'    are on average more lenient than the teacher of the national model,
#'    because the DUO final advice is higher than the model's provisional
#'    advice plus raises predicts. This average becomes the national offset
#'    of the final advice (model$final_shift), so that the leniency of the
#'    schools is centred on the national teacher.
#' 2. A second fit of the larger schools gives the spread of the parameters
#'    between schools.
#' 3. This spread becomes the prior of the final fit of all schools, so that
#'    the estimates of small schools move towards the average.
#' @return List with the schools (with mu, sigma, delta, and se_delta), the
#'   prior, and the model with its final_shift.
estimate_schools <- function(schools, model, gap, large = 50) {
    weak <- list(mean = c(0, 0, 0), sd = c(3, 2, 3))
    larger <- filter(schools, total >= large)
    average_leniency <- function(fits) {
        weighted.mean(fits$delta, larger$total[match(fits$id, larger$id)])
    }
    model$final_shift <- average_leniency(
        fit_schools(larger, model, gap, weak)
    )
    big <- fit_schools(larger, model, gap, weak)
    spread <- function(estimate, se) {
        sqrt(max(var(estimate) - mean(se^2, na.rm = TRUE), 0.01^2))
    }
    prior <- list(
        mean = c(mean(big$mu), mean(big$log_sigma), mean(big$delta)),
        sd = c(
            spread(big$mu, big$se_mu),
            spread(big$log_sigma, big$se_log_sigma),
            spread(big$delta, big$se_delta)
        )
    )
    fits <- fit_schools(schools, model, gap, prior)
    list(
        schools = schools |>
            inner_join(fits, by = "id") |>
            mutate(sigma = exp(log_sigma), .keep = "unused"),
        prior = prior,
        model = model
    )
}

#' A school's decomposition (Shapley values over school_sources) and its
#' cells with every source switched on.
school_results <- function(school, model, gap) {
    mixture <- model$mixtures[[school$provider]]
    cells <- subset_cells(
        school_sources, \(on) school_cells(school, model, mixture, on)
    )
    list(
        decomposition = decompose_cells(
            cells, school_sources, \(table) eligibility(table, gap)
        ),
        cells = cells[[subset_key(school_sources)]]
    )
}
