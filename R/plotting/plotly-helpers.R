# Small builders for the plotly layouts shared by the interactive plots.

# Layout defaults of the report: the fonts and ink of R/plotting/style.R,
# hairline gridlines, no zero lines, and white hover labels.
plotly_defaults <- list(
    font = list(
        family = fonts[["text"]], size = 12, color = ink[["secondary"]]
    ),
    title = list(font = list(
        family = fonts[["title"]], size = 17, color = ink[["primary"]]
    )),
    paper_bgcolor = "rgba(0, 0, 0, 0)",
    plot_bgcolor = "rgba(0, 0, 0, 0)",
    colorway = unname(pal),
    hoverlabel = list(
        bgcolor = "white", bordercolor = ink[["axis"]], align = "left",
        font = list(
            family = fonts[["text"]], size = 12, color = ink[["primary"]]
        )
    ),
    legend = list(
        bgcolor = "rgba(0, 0, 0, 0)",
        font = list(size = 12, color = ink[["secondary"]])
    )
)
plotly_axis_defaults <- list(
    gridcolor = ink[["grid"]], gridwidth = 1, linecolor = ink[["axis"]],
    zeroline = FALSE, ticks = "", tickcolor = ink[["axis"]],
    tickfont = list(size = 11, color = ink[["muted"]]),
    title = list(font = list(size = 12, color = ink[["secondary"]])),
    automargin = TRUE
)

#' Finishes a plotly figure in the style of the report: the defaults above
#' for the layout and every axis (what the figure sets itself wins), and no
#' mode bar. (A plotly template would be neater, but plotly for R drops the
#' name of its layout.)
plotly_style <- function(plot) {
    built <- plotly::plotly_build(plot)
    layout <- built$x$layout
    with_defaults <- function(values, defaults) {
        modifyList(defaults, values %||% list())
    }
    layout <- with_defaults(layout, plotly_defaults)
    # a title given as text only keeps the default font
    if (is.character(layout$title)) layout$title <- list(text = layout$title)
    layout$title <- with_defaults(layout$title, plotly_defaults$title)
    for (axis in grep("^[xy]axis[0-9]*$", names(layout), value = TRUE)) {
        layout[[axis]] <- with_defaults(layout[[axis]], plotly_axis_defaults)
    }
    built$x$layout <- layout
    plotly::config(built, displayModeBar = FALSE)
}

#' Bold title with an optional smaller grey subtitle, anchored to the
#' top left of the figure, so that every title lines up with the text.
plotly_title <- function(title, subtitle = NULL) {
    text <- paste0("<b>", title, "</b>")
    if (!is.null(subtitle)) {
        text <- paste0(
            text, "<br><span style='font-family:", fonts[["text"]],
            ";font-size:13px;color:", ink[["secondary"]], "'>", subtitle,
            "</span>"
        )
    }
    list(
        text = text,
        x = 0, xanchor = "left", xref = "container",
        # Multi-line titles grow upwards, so the pad keeps them on the page.
        y = 1, yanchor = "top", yref = "container", pad = list(t = 24)
    )
}

#' Top margin that fits the title, the subtitle lines (split by <br>), and a
#' legend above the plot area.
plotly_top_margin <- function(subtitle = NULL, legend_on_top = FALSE) {
    subtitle_lines <- if (is.null(subtitle)) {
        0
    } else {
        length(strsplit(subtitle, "<br>", fixed = TRUE)[[1]])
    }
    70 + 19 * subtitle_lines + 35 * legend_on_top
}

#' Horizontal legend above the plot area ("top") or below it ("bottom").
plotly_legend <- function(position = c("top", "bottom"), title = NULL) {
    position <- match.arg(position)
    legend <- list(
        orientation = "h", x = 0, xanchor = "left",
        y = if (position == "top") 1 else -0.2,
        yanchor = if (position == "top") "bottom" else "top"
    )
    if (!is.null(title)) legend$title <- list(text = title)
    legend
}

#' Text annotations at data coordinates, in text ink (marks carry colour).
#' side: 1 puts the text right of the point, -1 left of it.
plotly_labels <- function(x, y, text, side = 1, colour = ink[["secondary"]]) {
    side <- rep_len(side, length(x))
    lapply(
        seq_along(x),
        function(i) {
            list(
                x = x[[i]], y = y[[i]], text = text[[i]], showarrow = FALSE,
                xanchor = if (side[[i]] < 0) "right" else "left",
                xshift = 8 * side[[i]],
                font = list(size = 11, color = colour)
            )
        }
    )
}

#' Arrows in data coordinates, from (x0, y0) to (x1, y1).
plotly_arrows <- function(x0, y0, x1, y1, colour) {
    lapply(
        seq_along(x0),
        function(i) {
            list(
                x = x1[[i]], y = y1[[i]], ax = x0[[i]], ay = y0[[i]],
                xref = "x", yref = "y", axref = "x", ayref = "y",
                text = "", showarrow = TRUE, arrowhead = 2,
                arrowsize = 1, arrowwidth = 1.5, arrowcolor = colour[[i]],
                standoff = 5
            )
        }
    )
}

#' Adds the provider encoding to rows with a test and test_year: colour and
#' legend group by provider, open circles for the first year and filled
#' circles for later years.
with_provider_markers <- function(data) {
    first_year <- min(data$test_year)
    mutate(
        data,
        colour = provider_colour(test),
        group = provider_group(test),
        symbol = if_else(test_year == first_year, "circle-open", "circle")
    )
}

#' One marker trace per test, positioned by the formulas x and y. Each
#' provider group has one legend entry (on its first test), which toggles
#' all tests of that group.
#' @param points Output of with_provider_markers(), with a tooltip column.
provider_traces <- function(points, x, y) {
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
                data = test_points, x = x, y = y,
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

#' Text with <br> line breaks, because plotly does not wrap titles and
#' annotations.
wrap_html <- function(text, width) {
    paste(strwrap(text, width), collapse = "<br>")
}

#' Font in a text colour (marks carry the colour, text never does).
plotly_font <- function(size = 12, colour = ink[["secondary"]]) {
    list(size = size, color = colour)
}

#' Vertical line at x over the full height of an axis domain.
plotly_vline <- function(x, xref = "x", yref = "y domain",
                         colour = ink[["primary"]]) {
    list(
        type = "line", xref = xref, yref = yref, x0 = x, x1 = x, y0 = 0,
        y1 = 1, line = list(color = colour, width = 1)
    )
}
