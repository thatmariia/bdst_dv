# Plot of the Shapley decomposition of eligibility.

#' Segments of the decomposition bars: category design, teacher error, and
#' test error stack up from zero; the test menu segment runs from their end
#' back (or on) to the total.
#' @param decomposition Output of decompose_national() for all years.
decomposition_segments <- function(decomposition) {
    ends <- decomposition |>
        mutate(
            end_design = category_design,
            end_teacher = end_design + teacher_error,
            end_test = end_teacher + test_error
        )
    segment <- function(source, start, value) {
        transmute(
            ends, test_year, test, eligible,
            source = source, start = {{ start }}, value = {{ value }}
        )
    }
    bind_rows(
        segment("category design", 0, category_design),
        segment("teacher error", end_design, teacher_error),
        segment("test error", end_teacher, test_error),
        segment("test menu", end_test, test_menu)
    )
}

#' Bar style per source; the test menu is a hatched band over the part of
#' the bar it removes.
source_marker <- function(source) {
    if (source != "test menu") {
        return(list(color = source_colours[[source]]))
    }
    list(
        color = "rgba(255, 255, 255, 0.6)",
        pattern = list(
            shape = "/", fgcolor = source_colours[[source]], size = 6
        ),
        line = list(color = source_colours[[source]], width = 1)
    )
}

#' Traces of one year: a bar segment per source and test, a dot at the
#' total, and the total as text in a column right of the bars.
#' @param segments Output of decomposition_segments() for one year.
#' @param label_x Position of the column of totals.
add_decomposition_year <- function(plot, segments, label_x, visible) {
    totals <- segments |>
        distinct(test, test_year, eligible) |>
        mutate(label = pct(eligible))
    plot <- Reduce(
        function(plot, name) {
            source_segments <- segments |>
                filter(source == name) |>
                mutate(
                    tooltip = sprintf(
                        "%s, %d<br>%s: %s",
                        test, test_year, name,
                        if (name == "category design") {
                            pct(value)
                        } else {
                            sprintf("%+.1f pp", 100 * value)
                        }
                    )
                )
            plotly::add_bars(
                plot,
                data = source_segments, x = ~value, base = ~start, y = ~test,
                orientation = "h", name = name, legendgroup = name,
                marker = source_marker(name), visible = visible,
                text = ~tooltip, hoverinfo = "text", textposition = "none"
            )
        },
        names(source_colours),
        plot
    )
    plot |>
        plotly::add_trace(
            data = totals, x = ~eligible, y = ~test, type = "scatter",
            mode = "markers", name = "eligible", legendgroup = "eligible",
            marker = list(color = ink[["primary"]], size = 8),
            visible = visible,
            text = ~ paste("eligible:", label), hoverinfo = "text"
        ) |>
        plotly::add_trace(
            data = totals, x = label_x, y = ~test, type = "scatter",
            mode = "text", text = ~label, textposition = "middle right",
            textfont = list(color = ink[["secondary"]], size = 11),
            showlegend = FALSE, hoverinfo = "skip", visible = visible
        )
}

#' Interactive Shapley decomposition of eligibility per test
#' (@eq-decomposition), with radio buttons for the test year.
#' @param decomposition Output of decompose_national() for all years.
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
decomposition_plot <- function(
    decomposition,
    title = "Shapley decomposition of eligibility",
    subtitle = NULL
) {
    segments <- decomposition_segments(decomposition)
    years <- sort(unique(segments$test_year))
    # one column of totals and one scale for every year
    label_x <- max(segments$start + pmax(segments$value, 0))
    test_order <- function(year) {
        segments |>
            filter(test_year == year) |>
            distinct(test, eligible) |>
            arrange(eligible) |>
            pull(test)
    }
    plot <- Reduce(
        function(plot, year) {
            add_decomposition_year(
                plot, filter(segments, test_year == year), label_x,
                visible = year == max(years)
            )
        },
        years,
        plotly::plot_ly()
    )
    plot |>
        plotly::layout(
            barmode = "overlay",
            title = plotly_title(title, subtitle),
            margin = list(t = plotly_top_margin(subtitle), b = 90),
            legend = plotly_legend("bottom", title = "source: "),
            xaxis = list(
                title = "share of pupils eligible for a higher track",
                tickformat = ".0%", range = c(0, label_x * 1.12),
                fixedrange = TRUE
            ),
            yaxis = list(
                title = "", type = "category", categoryorder = "array",
                categoryarray = test_order(max(years)), fixedrange = TRUE
            )
        ) |>
        plotly_style() |>
        with_selectors(
            # six traces per year: four sources, the totals, their labels
            traces = tibble(test_year = rep(years, each = 6)),
            selectors = c(test_year = "Test year"),
            initial = list(test_year = max(years)),
            layouts = lapply(
                years,
                \(year) {
                    list(
                        key = list(test_year = year),
                        patch = list(yaxis.categoryarray = test_order(year))
                    )
                }
            )
        )
}
