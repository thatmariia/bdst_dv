# First test year in which raising the advice was mandatory.
mandatory_from <- 2024

#' Heatmap strip: share of eligible pupils (test advice higher than the
#' teacher's) whose advice was raised, per test year. Years without a test
#' get a "no test" tile; a line marks when raising became mandatory.
#' @param compliance Output of compliance_by_year().
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
compliance_heatmap <- function(
    compliance,
    title = "Compliance with raising the advice policy",
    subtitle = NULL
) {
    years <- seq(min(compliance$test_year), max(compliance$test_year))
    ramp <- scales::col_numeric(raised_ramp, domain = c(0, 1))
    tiles <- tibble(test_year = years) |>
        left_join(compliance, by = "test_year") |>
        mutate(
            missing = is.na(compliance),
            label = if_else(missing, "no test", pct_whole(compliance)),
            # the ramp stays light enough for dark labels on every tile
            colour = if_else(missing, ink[["shade"]], ramp(compliance)),
            tooltip = if_else(
                missing, paste0(test_year, ": no test"),
                sprintf("%d: %s raised", test_year, pct(compliance))
            )
        )
    plotly::plot_ly(tiles) |>
        plotly::add_bars(
            x = ~test_year, y = 1, width = 1, text = ~label,
            textposition = "inside", insidetextanchor = "middle", textangle = 0,
            textfont = plotly_font(12, ink[["primary"]]),
            marker = list(
                color = ~colour, line = list(color = "white", width = 2)
            ),
            hovertext = ~tooltip, hoverinfo = "text", showlegend = FALSE
        ) |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(
                t = plotly_top_margin(subtitle), b = 30, l = 10, r = 10
            ),
            bargap = 0,
            xaxis = list(
                title = "", tickvals = years, showgrid = FALSE,
                showline = FALSE, fixedrange = TRUE
            ),
            yaxis = list(
                title = "", range = c(0, 1), showgrid = FALSE,
                showticklabels = FALSE, fixedrange = TRUE
            ),
            shapes = list(
                modifyList(
                    plotly_vline(mandatory_from - 0.5),
                    list(line = list(width = 2))
                )
            )
        ) |>
        plotly_style()
}

#' Interactive lines: shares of pupils with a higher or lower test advice
#' than the teacher's, and with a raised advice, per test year. Ribbons are
#' the suppression bounds; lines break at years without a test.
#' @param compliance Output of compliance_by_year().
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
compliance_plot <- function(
    compliance,
    title = "Shares of higher/lower test advice and raised advice",
    subtitle = NULL
) {
    measures <- compliance_measures(compliance) |>
        mutate(
            tooltip = sprintf(
                "%s: %s (bounds %s–%s)", measure, pct(est), pct(lo), pct(hi)
            )
        )
    years <- seq(min(measures$test_year), max(measures$test_year))
    ymax <- max(measures$hi)
    ends <- filter(measures, test_year == max(test_year))

    plot <- plotly::plot_ly()
    for (name in levels(measures$measure)) {
        colour <- measure_colours[[name]]
        rows <- filter(measures, measure == name)
        # a band per segment, so that bands break at years without a test
        for (part in split(rows, droplevels(rows$segment))) {
            plot <- plotly::add_trace(
                plot,
                x = c(part$test_year, rev(part$test_year)),
                y = c(part$lo, rev(part$hi)), type = "scatter",
                mode = "lines", fill = "toself",
                fillcolor = plotly::toRGB(colour, 0.15),
                line = list(width = 0), legendgroup = name,
                showlegend = FALSE, hoverinfo = "skip"
            )
        }
        # missing years as NA, so that the line breaks there
        line <- tibble(test_year = years) |>
            left_join(rows, by = "test_year")
        plot <- plotly::add_trace(
            plot,
            x = line$test_year, y = line$est, type = "scatter",
            mode = "lines+markers", name = name, legendgroup = name,
            line = list(color = colour, width = 2),
            marker = list(color = colour, size = 7),
            text = line$tooltip, hoverinfo = "text"
        )
    }
    plot |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle), b = 90),
            hovermode = "x unified",
            legend = plotly_legend("bottom"),
            xaxis = list(
                title = "Test year", tickvals = years, showgrid = FALSE,
                showline = TRUE,
                range = c(min(years) - 0.3, max(years) + 2.2)
            ),
            yaxis = list(
                title = "Share of pupils", tickformat = ".0%",
                range = c(0, ymax * 1.05), showline = TRUE
            ),
            shapes = list(
                list(
                    type = "rect", layer = "below", fillcolor = ink[["shade"]],
                    line = list(width = 0), x0 = mandatory_from - 0.5,
                    x1 = max(years) + 0.5, y0 = 0, y1 = ymax
                )
            ),
            annotations = c(
                plotly_labels(
                    ends$test_year, ends$est, as.character(ends$measure)
                ),
                list(
                    list(
                        x = mandatory_from - 0.5, y = ymax,
                        text = "raising mandatory →", showarrow = FALSE,
                        xanchor = "left", yanchor = "bottom", xshift = 4,
                        font = list(size = 11, color = ink[["secondary"]])
                    )
                )
            )
        ) |>
        plotly_style()
}
