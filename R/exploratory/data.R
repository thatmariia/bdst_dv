country_compliance <- advice_reconsiderations |>
    filter(!is.na(raised)) |>
    group_by(test_year) |>
    summarise(
        pupils = sum(total),
        compliance = sum(raised) / sum(higher),
        higher_est = sum(higher) / pupils,
        lower_est = sum(lower) / pupils,
        raised_est = sum(raised) / pupils,
        across(
            matches("_(lo|hi)$"),
            \(x) sum(x) / pupils
        )
    )

gap_long <- as_tibble(gap_matrix, rownames = "provisional") |>
    pivot_longer(-provisional, names_to = "test_advice", values_to = "gap")

tests <- list(
    "2025" = c("AMN", "Dia", "DOE", "IEP digital", "IEP paper", "Lib digital", "Lib paper", "ROUTE 8"), # nolint: line_length_linter
    "2026" = c("AMN", "Dia", "IEP digital", "IEP paper", "Lib digital", "Lib paper", "ROUTE 8") # nolint: line_length_linter
)

country_provisional <- cvte_provisional |>
    inner_join(cvte_pupils, by = c("test_year", "test")) |>
    filter(test %in% unlist(tests)) |>
    group_by(test_year, provisional) |>
    summarise(m = weighted.mean(pct, pupils) / 100, .groups = "drop") |>
    group_by(test_year) |>
    mutate(m = m / sum(m)) |>
    ungroup()

#' Joint distribution of provisional and conditional advice for a given year.
country_joint <- function(year) {
    cvte_conditional |>
        filter(test_year == year, test == "national") |>
        group_by(provisional) |>
        mutate(p = pct / sum(pct)) |> # rows sum to one
        ungroup() |>
        inner_join(
            filter(country_provisional, test_year == year),
            by = c("test_year", "provisional")
        ) |>
        inner_join(gap_long, by = c("provisional", "test_advice")) |>
        mutate(share = m * p)
}
