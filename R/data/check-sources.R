#' Shares of test advice lower, equal to, or higher than provisional advice.
#' @param marginal Provisional advice distribution (any scale).
#' @param conditional 10 x 6 row percentages of test advice per provisional.
#' @param gap DUO gap matrix from parse_gap_matrix().
#' @return Named percentages: lower, equal, higher.
gap_shares <- function(marginal, conditional, gap) {
    joint <- (marginal / sum(marginal)) * conditional / 100
    100 * c(
        lower = sum(joint[gap < 0]), equal = sum(joint[gap == 0]),
        higher = sum(joint[gap > 0])
    )
}

#' Provisional distribution or conditional table of one test as a matrix.
cvte_matrix <- function(table, year, name) {
    rows <- filter(table, test_year == year, test == name)
    if ("test_advice" %in% names(rows)) {
        matrix(rows$pct, nrow = 10, dimnames = list(
            provisional_levels, test_advice_levels
        ))
    } else {
        rows$pct
    }
}

#' Lower/equal/higher shares per test and mode in its own population.
#' @return Tibble with test, pupils, lower, equal, and higher.
cvte_test_shares <- function(year, conditional, provisional, pupils, gap) {
    tests <- unique(conditional$test[conditional$test_year == year])
    tests <- setdiff(tests, "national")
    bind_rows(lapply(tests, function(name) {
        shares <- gap_shares(
            cvte_matrix(provisional, year, name),
            cvte_matrix(conditional, year, name), gap
        )
        tibble(
            test = name,
            pupils = pupils$pupils[
                pupils$test_year == year & pupils$test == name
            ],
            !!!as.list(shares)
        )
    }))
}

#' Compare the sources against each other and against published figures.
#' @return Tibble with one row per comparison, the two values, and ok.
check_sources <- function(advice_summary, reconsiderations, main_provider,
                          conditional,
                          provisional, pupils, agreement, misclassification,
                          gap) {
    check <- function(name, ours, reference, tolerance) {
        tibble(
            check = name, ours = round(ours, 1), reference = reference,
            tolerance = tolerance, ok = abs(ours - reference) <= tolerance
        )
    }

    # Year coding (D2): combined advices and raised advices should both
    # jump most in test year 2024 (PEILJAAR 2023), when raising became
    # mandatory.
    largest_jump <- function(data, value, weight) {
        yearly <- data |>
            summarise(
                share = weighted.mean({{ value }}, {{ weight }}),
                .by = test_year
            ) |>
            arrange(test_year)
        yearly$test_year[which.max(diff(yearly$share)) + 1]
    }
    combined_jump <- largest_jump(advice_summary, share_combined, pupils)
    raised_jump <- largest_jump(reconsiderations, raised_share, total)

    # Gap matrix on CvTE's national 2026 table, with the provisional
    # distribution weighted by pupils per test and mode.
    tests_2026 <- cvte_test_shares(2026, conditional, provisional, pupils, gap)
    national_marginal <- Reduce(`+`, Map(function(name, n) {
        n * cvte_matrix(provisional, 2026, name) / 100
    }, tests_2026$test, tests_2026$pupils))
    national <- gap_shares(
        national_marginal, cvte_matrix(conditional, 2026, "national"), gap
    )
    published <- setNames(agreement$pct, agreement$comparison)

    # Test year 2025: CvTE tables (provisional advice) against DUO school
    # files (original advice) for schools with one main provider.
    cvte_2025 <- cvte_test_shares(
        2025, conditional, provisional, pupils, gap
    ) |>
        mutate(provider = toupper(sub(" .*$", "", test))) |>
        group_by(provider) |>
        summarise(across(c(lower, higher), ~ weighted.mean(.x, pupils)))
    duo_2025 <- reconsiderations |>
        filter(test_year == 2025L) |>
        inner_join(filter(main_provider, main_share >= 0.9), by = school_key) |>
        summarise(
            lower = 100 * sum(lower) / sum(total),
            higher = 100 * sum(higher) / sum(total),
            .by = provider
        )
    cross <- cvte_2025 |>
        inner_join(duo_2025, by = "provider", suffix = c("_cvte", "_duo")) |>
        filter(provider %in% c("LIB", "IEP"))

    total <- filter(misclassification, test_advice == "total")
    bind_rows(
        check("Largest rise in combined advices (test year)",
            combined_jump, 2024,
            tolerance = 0
        ),
        check("Largest rise in raised advices (test year)",
            raised_jump, 2024,
            tolerance = 0
        ),
        lapply(names(published), function(side) {
            check(
                paste0("CvTE 2026 national ", side, " vs Tabel 14 (%)"),
                national[[side]], published[[side]], 1.5
            )
        }),
        lapply(seq_len(nrow(cross)), function(i) {
            bind_rows(lapply(c("higher", "lower"), function(side) {
                check(
                    paste0(
                        "2025 ", cross$provider[i], " ", side,
                        ": CvTE vs DUO (%)"
                    ),
                    cross[[paste0(side, "_cvte")]][i],
                    round(cross[[paste0(side, "_duo")]][i], 1), 1
                )
            }))
        }),
        check("Cito 2022 misclassified test advices (%)",
            total$lower + total$higher, 23.3,
            tolerance = 0.05
        )
    )
}
