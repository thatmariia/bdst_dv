# Plots of the model's input counts and its fit.

#' Interactive pseudo counts n_t(i, j): the bubble grid of @fig-joint-plot,
#' with radio buttons for the test year and test; the first option sums all
#' tests of the year. Dot area is the share within the chosen table; hover
#' shows the pseudo count.
#' @param counts Output of pseudo_counts().
#' @param gap DUO gap matrix.
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
model_counts_plot <- function(
    counts,
    gap,
    title = "Pseudo counts per advice",
    subtitle = NULL
) {
    all_tests <- counts |>
        summarise(n = sum(n), .by = c(test_year, provisional, test_advice)) |>
        mutate(test = "All tests", joint = n / sum(n), .by = test_year)

    bind_rows(all_tests, counts) |>
        # "All tests" first, then the tests in alphabetical order
        arrange(test_year, test != "All tests", test) |>
        mutate(
            provisional = as.character(provisional),
            test_advice = as.character(test_advice),
            gap = gap[cbind(provisional, test_advice)],
            share = joint,
            tooltip = sprintf(
                "provisional: %s<br>test: %s<br>%s pupils (%s of %s)",
                short_advice(provisional), short_advice(test_advice),
                scales::comma(n), pct(joint),
                if_else(test == "All tests", "all tests", "the test")
            )
        ) |>
        advice_bubble_plot(
            title = title, subtitle = subtitle,
            selectors = c(test_year = "Test year", test = "Test"),
            initial = list(
                test_year = max(counts$test_year), test = "All tests"
            )
        )
}

#' Interactive dot plot of the dissimilarity index per test and year: provider
#' colours, open circles for the first year and filled for the latest.
#' @param fit_quality Output of fit_summary() for all years, with test_year.
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
dissimilarity_plot <- function(
    fit_quality,
    title = "Dissimilarity indices",
    subtitle = NULL
) {
    points <- fit_quality |>
        with_provider_markers() |>
        mutate(
            tooltip = sprintf(
                "%s, %d<br>dissimilarity: %s",
                test, test_year, pct(dissimilarity)
            )
        )
    test_order <- points |>
        summarise(dissimilarity = mean(dissimilarity), .by = test) |>
        arrange(dissimilarity)

    provider_traces(points, x = ~dissimilarity, y = ~test) |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle, TRUE)),
            showlegend = TRUE,
            legend = plotly_legend("top", title = "test provider: "),
            xaxis = list(
                title = "dissimilarity index", tickformat = ".0%",
                rangemode = "tozero"
            ),
            yaxis = list(
                title = "", type = "category",
                categoryorder = "array", categoryarray = test_order$test
            )
        )
}
