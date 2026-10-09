#' Interactive standardised leniency against disagreement per test and mode:
#' open circles are the first year, filled the latest, arrows run between
#' them, and tests are labelled at their latest year on the side the arrow
#' points to. Built in plotly directly, because ggplotly drops arrowheads and
#' per-label sides.
#' @param standardized Output of standardize_tests().
standardized_plot <- function(standardized) {
    first_year <- min(standardized$test_year)
    points <- standardized |>
        mutate(
            colour = provider_colour(test),
            group = provider_group(test),
            symbol = if_else(test_year == first_year, "circle-open", "circle"),
            tooltip = sprintf(
                "%s, %d<br>higher: %s<br>lower: %s<br>leniency: %s<br>disagreement: %s", # nolint: line_length_linter.
                test, test_year, pct(higher), pct(lower),
                pct(leniency), pct(disagreement)
            )
        )
    moves <- points |>
        arrange(test_year) |>
        summarise(
            x0 = first(leniency), y0 = first(disagreement),
            x1 = last(leniency), y1 = last(disagreement),
            colour = first(colour), years = n(),
            .by = test
        ) |>
        mutate(side = if_else(x1 < x0, -1, 1))
    arrows <- filter(moves, years > 1)

    standardized_traces(points) |>
        plotly::layout(
            title = plotly_title(
                paste(
                    "For the same pupils, Lib gives higher test advice",
                    "than other tests"
                ),
                paste(
                    "Shares standardised to the national mix of provisional",
                    "advice. Open circle: 2025, filled: 2026."
                )
            ),
            margin = list(t = 110),
            showlegend = TRUE,
            legend = plotly_legend("top", title = "test provider: "),
            xaxis = list(
                title = "leniency: share test higher − share test lower",
                tickformat = ".0%",
                zeroline = TRUE, zerolinecolor = ink[["muted"]]
            ),
            yaxis = list(
                title = "disagreement: share test higher + share test lower",
                tickformat = ".0%"
            ),
            annotations = c(
                plotly_arrows(
                    arrows$x0, arrows$y0, arrows$x1, arrows$y1, arrows$colour
                ),
                plotly_labels(
                    moves$x1, moves$y1, moves$test,
                    side = moves$side
                ),
                list(list(
                    x = 0, y = 1, yref = "paper", text = "balanced",
                    showarrow = FALSE, xanchor = "left", yanchor = "top",
                    xshift = 4, font = list(size = 11, color = ink[["muted"]])
                ))
            )
        )
}

#' One marker trace per test. Each provider group has one legend entry (on
#' its first test), which toggles all tests of that group.
standardized_traces <- function(points) {
    tests <- unique(points$test)
    first_in_group <- tests[!duplicated(provider_group(tests))]
    Reduce(
        function(plot, name) {
            # Latest year first, so the legend shows a filled circle.
            test_points <- points |>
                filter(test == name) |>
                arrange(desc(test_year))
            group <- test_points$group[[1]]
            plotly::add_trace(
                plot,
                data = test_points, x = ~leniency, y = ~disagreement,
                type = "scatter", mode = "markers", name = I(group),
                legendgroup = I(group), showlegend = name %in% first_in_group,
                legendrank = match(group, names(provider_colours)),
                marker = list(
                    size = 9, color = test_points$colour,
                    symbol = test_points$symbol,
                    line = list(width = 1.5, color = test_points$colour)
                ),
                text = ~tooltip, hoverinfo = "text"
            )
        },
        tests,
        plotly::plot_ly()
    )
}
