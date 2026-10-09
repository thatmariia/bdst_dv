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
