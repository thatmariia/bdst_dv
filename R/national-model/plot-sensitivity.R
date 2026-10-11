# Plot of the sensitivity of the decomposition to the anchor
# (@fig-sensitivity).

# Series of the sensitivity plot: the total and the four sources.
sensitivity_series <- tibble(
    key = c(
        "eligible", "category_design", "teacher_error", "test_error",
        "test_menu"
    ),
    label = c(
        "eligible (total)", "category design", "teacher error", "test error",
        "test menu"
    ),
    colour = c(ink[["primary"]], unname(source_colours))
)

#' Shaded rectangles over the resit shares that the tables rule out, with a
#' label on the wide ones.
#' @param rows Rows of anchor_sensitivity() for one year, sorted by flip.
ruled_out <- function(rows) {
    runs <- rle(!rows$feasible)
    ends <- cumsum(runs$lengths)
    starts <- ends - runs$lengths + 1
    # each run reaches halfway to its allowed neighbours
    edge <- function(k, side) {
        if (side < 0 && k == 1) {
            return(0)
        }
        if (side > 0 && k == nrow(rows)) {
            return(max(rows$flip))
        }
        (rows$flip[k] + rows$flip[k + side]) / 2
    }
    regions <- Map(
        function(first, last) c(edge(first, -1), edge(last, 1)),
        starts[runs$values], ends[runs$values]
    )
    list(
        shapes = lapply(
            regions,
            function(region) {
                list(
                    type = "rect", xref = "x", yref = "y domain",
                    x0 = region[1], x1 = region[2], y0 = 0, y1 = 1,
                    fillcolor = ink[["shade"]], line = list(width = 0),
                    layer = "below"
                )
            }
        ),
        annotations = lapply(
            Filter(\(region) diff(region) > 0.1, regions),
            function(region) {
                list(
                    x = mean(region), xref = "x", y = 1, yref = "y domain",
                    yanchor = "top", showarrow = FALSE,
                    text = "ruled out<br>by the tables",
                    font = plotly_font(11, ink[["muted"]])
                )
            }
        )
    )
}

#' Interactive sensitivity of the national decomposition to the anchor: the
#' total and every source for each resit share r, with the shares that the
#' tables rule out shaded and Cito's share marked. Radio buttons select the
#' year.
#' @param sensitivity Output of anchor_sensitivity().
#' @param flip Cito's resit share r, as a proportion.
#' @param title,subtitle Plot title and optional subtitle.
sensitivity_plot <- function(
    sensitivity, flip,
    title = "Decomposition sensitivity to anchor",
    subtitle = NULL
) {
    years <- sort(unique(sensitivity$test_year))
    sensitivity <- arrange(sensitivity, test_year, flip)
    allowed <- filter(sensitivity, feasible)
    y_range <- range(pretty(100 * c(allowed$test_menu, allowed$eligible)))
    cito_label <- sprintf("Cito's resit share (%.1f%%)", 100 * flip)

    # legend entries for the shading and Cito's line, outside the axis range
    plot <- plotly::plot_ly() |>
        plotly::add_markers(
            x = -1, y = 0, name = "infeasible",
            marker = list(symbol = "square", size = 14, color = ink[["shade"]]),
            hoverinfo = "skip"
        ) |>
        plotly::add_lines(
            x = c(-1, -0.9), y = c(0, 0), name = cito_label,
            line = list(color = ink[["secondary"]], width = 1),
            hoverinfo = "skip"
        )
    plot <- Reduce(
        function(plot, k) {
            year <- years[(k - 1) %/% nrow(sensitivity_series) + 1]
            series <- sensitivity_series[
                (k - 1) %% nrow(sensitivity_series) + 1,
            ]
            rows <- filter(sensitivity, test_year == year)
            plotly::add_lines(
                plot,
                x = rows$flip, y = 100 * rows[[series$key]],
                name = series$label, connectgaps = FALSE,
                line = list(color = series$colour, width = 2),
                hovertemplate = "%{y:.1f}", legendrank = k,
                visible = year == max(years)
            )
        },
        seq_len(length(years) * nrow(sensitivity_series)),
        plot
    )

    layouts <- lapply(
        years,
        function(year) {
            regions <- ruled_out(filter(sensitivity, test_year == year))
            list(
                key = list(test_year = year),
                patch = list(
                    shapes = c(
                        regions$shapes,
                        list(plotly_vline(flip, colour = ink[["secondary"]]))
                    ),
                    annotations = regions$annotations
                )
            )
        }
    )

    plot |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle, TRUE) + 18),
            legend = plotly_legend("top"),
            hovermode = "x unified",
            xaxis = list(
                title = "resit share r: pupils whose test advice changes on a resit", # nolint: line_length_linter.
                tickformat = ".0%", range = c(0, max(sensitivity$flip)),
                zeroline = FALSE, fixedrange = TRUE
            ),
            yaxis = list(
                title = "percentage points of all pupils", range = y_range,
                zerolinecolor = ink[["axis"]], fixedrange = TRUE
            )
        ) |>
        plotly::config(displayModeBar = FALSE) |>
        with_selectors(
            # the legend entries for every year, then one trace per year and
            # series
            traces = tibble(
                test_year = c(
                    NA, NA, rep(years, each = nrow(sensitivity_series))
                )
            ),
            selectors = c(test_year = "Test year"),
            initial = list(test_year = max(years)),
            layouts = layouts
        )
}
