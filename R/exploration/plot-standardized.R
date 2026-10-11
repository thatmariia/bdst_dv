#' Interactive standardised leniency against disagreement per test and mode:
#' open circles are the first year, filled the latest, arrows run between
#' them, and tests are labelled at their latest year on the side the arrow
#' points to. Built in plotly directly, because ggplotly drops arrowheads and
#' per-label sides.
#' @param standardized Output of standardize_tests().
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
standardized_plot <- function(
    standardized,
    title = "Leniency and disagreement per test",
    subtitle = NULL
) {
    points <- standardized |>
        with_provider_markers() |>
        mutate(
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

    provider_traces(points, x = ~leniency, y = ~disagreement) |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle, TRUE)),
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
                list(
                    list(
                        x = 0, y = 1, yref = "paper", text = "balanced",
                        showarrow = FALSE, xanchor = "left", yanchor = "top",
                        xshift = 4,
                        font = list(size = 11, color = ink[["muted"]])
                    )
                )
            )
        )
}
