# Interactive bubble grid of provisional advice by test advice, shared by the
# joint distribution (@fig-joint-plot) and the pseudo counts
# (@fig-model-counts).

#' Bubble grid with radio buttons to choose what is shown: dot area is the
#' share, colour compares the test advice with the provisional advice. Cells
#' under 0.05% are left out.
#' @param cells One row per selector combination, provisional advice, and
#'   test advice, with gap, share, and tooltip columns.
#' @param selectors Named character vector: selector columns of `cells` and
#'   their labels, e.g. c(test_year = "Test year").
#' @param initial Named list with the first choice for every selector.
#' @param title,subtitle Plot title and optional subtitle.
advice_bubble_plot <- function(
    cells, selectors, initial, title,
    subtitle = NULL
) {
    cells <- cells |>
        filter(share > 0.0005) |>
        mutate(
            comparison = case_when(
                gap < 0 ~ "test lower",
                gap == 0 ~ "equal",
                gap > 0 ~ "test higher"
            )
        )
    # one trace per selector combination and comparison
    traces <- cells |>
        distinct(across(all_of(names(selectors)))) |>
        cross_join(tibble(comparison = names(comparison_colours)))
    is_initial <- Reduce(
        `&`, Map(
            function(column, value) traces[[column]] == value,
            names(initial), initial
        )
    )
    size_ref <- 2 * max(cells$share) / 30^2

    plot <- Reduce(
        function(plot, k) {
            trace_cells <- semi_join(cells, traces[k, ], by = names(traces))
            comparison <- traces$comparison[k]
            plotly::add_trace(
                plot,
                data = trace_cells, type = "scatter", mode = "markers",
                x = ~ short_advice(test_advice),
                y = ~ short_advice(provisional),
                name = comparison, legendgroup = comparison,
                visible = is_initial[k],
                marker = list(
                    size = ~share, sizemode = "area", sizeref = size_ref,
                    sizemin = 1, opacity = 1,
                    color = comparison_colours[[comparison]],
                    line = list(width = 0)
                ),
                text = ~tooltip, hoverinfo = "text"
            )
        },
        seq_len(nrow(traces)),
        plotly::plot_ly()
    )

    advice_axis <- function(levels, title, angle = 0) {
        list(
            title = title, type = "category", categoryorder = "array",
            categoryarray = short_advice(levels), tickangle = angle,
            showgrid = FALSE, showline = TRUE, linecolor = ink[["axis"]],
            ticks = "outside", tickcolor = ink[["muted"]]
        )
    }
    plot |>
        plotly::layout(
            title = plotly_title(title, subtitle),
            legend = c(
                plotly_legend("top", title = "test advice vs teacher's: "),
                itemsizing = "constant"
            ),
            margin = list(t = plotly_top_margin(subtitle, TRUE)),
            xaxis = advice_axis(test_advice_levels, "test advice", -25),
            yaxis = advice_axis(
                rev(provisional_levels), "teacher's provisional advice"
            )
        ) |>
        plotly_style() |>
        with_selectors(traces, selectors, initial)
}
