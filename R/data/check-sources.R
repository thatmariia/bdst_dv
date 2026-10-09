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
check_sources <- function(final_advice, reconsiderations, providers,
                          conditional,
                          provisional, pupils, agreement, misclassification,
                          gap) {
    check <- function(name, ours, reference, tolerance) {
        tibble(
            check = name, ours = round(ours, 1), reference = reference,
            tolerance = tolerance, ok = abs(ours - reference) <= tolerance
        )
    }

    # Combined advices jump when advice raising becomes mandatory (reference
    # year 2023): regular-school pupils, all advice codes, suppressed cells
    # at midpoint.
    combined <- final_advice |>
        filter(school_type == "regular", reference_year %in% 2022:2023) |>
        group_by(reference_year) |>
        summarise(pct = 100 * sum(n_mid[grepl("/", advice)]) / sum(n_mid))

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
        filter(
            school_type == "regular", reference_year == 2024L, total_mid > 0
        ) |>
        inner_join(
            filter(providers, main_share >= 0.9),
            by = school_year_key
        ) |>
        group_by(provider) |>
        summarise(
            lower = 100 * sum(lower_mid) / sum(total_mid),
            higher = 100 * sum(higher_mid) / sum(total_mid)
        )
    cross <- cvte_2025 |>
        inner_join(duo_2025, by = "provider", suffix = c("_cvte", "_duo")) |>
        filter(provider %in% c("LIB", "IEP"))

    total <- filter(misclassification, test_advice == "total")
    bind_rows(
        check("DUO combined advices, reference year 2022 (%)",
            combined$pct[1], 33.3,
            tolerance = 0.1
        ),
        check("DUO combined advices, reference year 2023 (%)",
            combined$pct[2], 43.0,
            tolerance = 0.1
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
