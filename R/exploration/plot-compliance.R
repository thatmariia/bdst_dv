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
    tibble(test_year = years) |>
        left_join(compliance, by = "test_year") |>
        mutate(
            label = if_else(
                is.na(compliance), "no test", pct_whole(compliance)
            )
        ) |>
        ggplot(aes(x = test_year, y = 1, fill = compliance)) +
        geom_tile(colour = "white", linewidth = 1) +
        # the ramp stays light enough for dark labels on every tile
        geom_text(aes(label = label), colour = ink[["primary"]], size = 3.5) +
        geom_vline(
            xintercept = mandatory_from - 0.5,
            colour = ink[["primary"]], linewidth = 0.6
        ) +
        scale_fill_gradient(
            low = raised_ramp[1], high = raised_ramp[2], limits = c(0, 1),
            na.value = ink[["shade"]], guide = "none"
        ) +
        scale_x_continuous(breaks = years, expand = c(0, 0)) +
        labs(
            title = title, subtitle = subtitle, x = NULL, y = NULL
        ) +
        theme_report() +
        theme(
            axis.line = element_blank(),
            axis.ticks = element_blank(),
            axis.text.y = element_blank(),
            plot.subtitle = element_text(margin = margin(b = 6))
        )
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

    plot <- measures |>
        # text is for the plotly tooltip; ggplot2 only checks layer
        # aesthetics, and the group keeps per-point text from splitting lines.
        ggplot(
            aes(
                x = test_year, y = est, color = measure, fill = measure,
                group = segment, text = tooltip
            )
        ) +
        # plotly cannot draw infinite limits, so the band uses the data range.
        annotate(
            "rect",
            fill = ink[["shade"]],
            xmin = mandatory_from - 0.5, xmax = max(years) + 0.5,
            ymin = 0, ymax = ymax
        ) +
        geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, color = NA) +
        geom_line(linewidth = 0.7) +
        geom_point(size = 2) +
        scale_x_continuous(
            breaks = years, expand = expansion(add = c(0.3, 2.2))
        ) +
        scale_y_continuous(
            labels = pct_whole,
            limits = c(0, NA), expand = expansion(mult = c(0, 0.05))
        ) +
        scale_color_manual(values = measure_colours) +
        scale_fill_manual(values = measure_colours) +
        labs(
            x = "Test year", y = "Share of pupils", color = NULL, fill = NULL
        ) +
        theme_report(grid_y = TRUE)

    tidy_ggplotly(plot) |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle), b = 90),
            hovermode = "x unified",
            legend = plotly_legend("bottom"),
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
