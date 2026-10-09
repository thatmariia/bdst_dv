provider_colour <- function(test) {
    case_when(
        startsWith(test, "Lib") ~ pal[["blue"]],
        startsWith(test, "IEP") ~ pal[["orange"]],
        TRUE ~ pal[["green"]]
    )
}

# Each test is labelled at its latest year, on the side its arrow points to.
standardized_labels <- standardized |>
    arrange(test_year) |>
    summarise(
        side = if_else(last(leniency) < first(leniency), -1, 1),
        leniency = last(leniency),
        disagreement = last(disagreement),
        .by = test
    )

#' Interactive standardised leniency against disagreement per test and mode:
#' open circles are 2025, filled 2026, arrows run from 2025 to 2026. Built in
#' plotly directly, because ggplotly drops arrowheads and per-label sides.
percent <- scales::percent_format(accuracy = 0.1)
standardized_points <- standardized |>
    arrange(test, test_year) |>
    mutate(
        colour = provider_colour(test),
        tooltip = sprintf(
            "%s, %d<br>higher: %s<br>lower: %s<br>leniency: %s<br>disagreement: %s", # nolint: line_length_linter.
            test, test_year, percent(higher), percent(lower),
            percent(leniency), percent(disagreement)
        )
    )

standardized_arrows <- standardized_points |>
    summarise(
        x = last(leniency), y = last(disagreement),
        ax = first(leniency), ay = first(disagreement),
        colour = first(colour), years = n(),
        .by = test
    ) |>
    filter(years > 1)

standardized_plot <- Reduce(
    function(plot, name) {
        points <- filter(standardized_points, test == name)
        plotly::add_trace(
            plot,
            data = points, x = ~leniency, y = ~disagreement,
            type = "scatter", mode = "markers", name = name,
            marker = list(
                size = 9, color = points$colour,
                symbol = if_else(
                    points$test_year == min(standardized$test_year),
                    "circle-open", "circle"
                ),
                line = list(width = 1.5, color = points$colour)
            ),
            text = ~tooltip, hoverinfo = "text"
        )
    },
    unique(standardized_points$test),
    plotly::plot_ly()
) |>
    plotly::layout(
        showlegend = FALSE,
        xaxis = list(
            title = "leniency: higher \u2212 lower", tickformat = ".0%",
            zeroline = TRUE, zerolinecolor = "grey"
        ),
        yaxis = list(
            title = "disagreement: higher + lower", tickformat = ".0%"
        ),
        annotations = c(
            # Arrows from 2025 to 2026.
            lapply(seq_len(nrow(standardized_arrows)), function(i) {
                with(standardized_arrows[i, ], list(
                    x = x, y = y, ax = ax, ay = ay,
                    xref = "x", yref = "y", axref = "x", ayref = "y",
                    text = "", showarrow = TRUE, arrowhead = 2,
                    arrowsize = 1, arrowwidth = 1.5, arrowcolor = colour,
                    standoff = 5
                ))
            }),
            # Labels at the latest year, on the side the arrow points to.
            lapply(seq_len(nrow(standardized_labels)), function(i) {
                label <- standardized_labels[i, ]
                list(
                    x = label$leniency, y = label$disagreement,
                    text = label$test, showarrow = FALSE,
                    xanchor = if (label$side < 0) "right" else "left",
                    xshift = 8 * label$side,
                    font = list(size = 11, color = provider_colour(label$test))
                )
            })
        )
    )
