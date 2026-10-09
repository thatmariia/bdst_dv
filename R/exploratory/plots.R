years_compliance <- min(country_compliance$test_year):max(country_compliance$test_year) # nolint: line_length_linter.
comp_col <- c("higher" = "#1b9e77", "lower" = "#d95f02", "raised" = "#7570b3")

compliance_heatmap <- country_compliance |>
    select(test_year, compliance) |>
    ggplot(aes(x = test_year, y = 1, fill = compliance)) +
    geom_tile() +
    geom_text(
        aes(label = scales::percent(compliance, accuracy = 1)),
        color = "black"
    ) +
    scale_fill_gradient(
        low = "#f2f2ef", high = "#1b9e77",
        labels = scales::percent_format(accuracy = 1)
    ) +
    labs(
        title = "Share of eligible pupils with raised advice",
        x = "Year",
        y = NULL,
        fill = "Share of pupils"
    ) +
    scale_x_continuous(breaks = years_compliance) +
    theme_void() +
    theme(
        legend.position = "none",
        axis.text.x = element_text()
    )

compliance_long <- country_compliance |>
    pivot_longer(
        cols = matches("^(higher|lower|raised)_(est|lo|hi)$"),
        names_to = c("type", ".value"),
        names_pattern = "(higher|lower|raised)_(est|lo|hi)"
    ) |>
    mutate(tooltip = sprintf(
        "%s, %d<br>%s (bounds %s\u2013%s)",
        type, test_year,
        scales::percent(est, accuracy = 0.1),
        scales::percent(lo, accuracy = 0.1),
        scales::percent(hi, accuracy = 0.1)
    ))

compliance_plot <- compliance_long |>
    # text is for the plotly tooltip; ggplot2 only checks layer aesthetics,
    # and the explicit group keeps the per-point text from splitting lines.
    ggplot(aes(
        x = test_year, y = est, color = type, fill = type,
        group = type, text = tooltip
    )) +
    # plotly cannot draw infinite limits, so the band uses the data range.
    annotate(
        "rect",
        fill = "#f2f2ef",
        xmin = 2023.5, xmax = 2025.5,
        ymin = 0, ymax = max(compliance_long$hi)
    ) +
    geom_ribbon(aes(ymin = lo, ymax = hi), alpha = .15, color = NA) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = years_compliance) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_color_manual(values = comp_col) +
    scale_fill_manual(values = comp_col) +
    labs(
        title = "Share of pupils with raised advice, by year",
        x = "Year",
        y = "Share of pupils",
        color = NULL, fill = NULL
    ) +
    theme_minimal()

# Interactive version: hover shows the points only.
compliance_plot <- plotly::ggplotly(compliance_plot, tooltip = "text")
compliance_plot$x$data <- lapply(compliance_plot$x$data, function(trace) {
    trace$name <- sub("^\\((.*),1\\)$", "\\1", trace$name)
    # Ribbons and the background band: no hover, no legend entry.
    is_area <- identical(trace$fill, "toself") || !nzchar(trace$name)
    if (is_area) trace$hoverinfo <- "skip"
    # One legend entry per type; clicking it toggles line, points and ribbon.
    trace$legendgroup <- trace$name
    trace$showlegend <- !is_area
    trace
})
