# Small builders for the plotly layouts shared by the interactive plots.

#' Bold title with a smaller grey subtitle; anchored to the top left.
plotly_title <- function(title, subtitle) {
    list(
        text = paste0(
            "<b>", title, "</b><br><span style='font-size:12px;color:",
            ink[["secondary"]], "'>", subtitle, "</span>"
        ),
        x = 0, xanchor = "left", xref = "paper",
        # Multi-line titles grow upwards, so the pad keeps them on the page.
        y = 1, yanchor = "top", yref = "container", pad = list(t = 30)
    )
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
    lapply(seq_along(x), function(i) {
        list(
            x = x[[i]], y = y[[i]], text = text[[i]], showarrow = FALSE,
            xanchor = if (side[[i]] < 0) "right" else "left",
            xshift = 8 * side[[i]],
            font = list(size = 11, color = colour)
        )
    })
}

#' Arrows in data coordinates, from (x0, y0) to (x1, y1).
plotly_arrows <- function(x0, y0, x1, y1, colour) {
    lapply(seq_along(x0), function(i) {
        list(
            x = x1[[i]], y = y1[[i]], ax = x0[[i]], ay = y0[[i]],
            xref = "x", yref = "y", axref = "x", ayref = "y",
            text = "", showarrow = TRUE, arrowhead = 2,
            arrowsize = 1, arrowwidth = 1.5, arrowcolor = colour[[i]],
            standoff = 5
        )
    })
}

#' ggplotly with tidied traces: plain legend names, one legend entry per
#' name (which toggles all its traces), and no hover or legend entry for
#' filled areas (ribbons, background bands).
tidy_ggplotly <- function(plot) {
    widget <- plotly::ggplotly(plot, tooltip = "text")
    in_legend <- character()
    widget$x$data <- lapply(widget$x$data, function(trace) {
        trace$name <- sub("^\\((.*),1\\)$", "\\1", trace$name)
        is_area <- identical(trace$fill, "toself") || !nzchar(trace$name)
        if (is_area) trace$hoverinfo <- "skip"
        trace$legendgroup <- trace$name
        trace$showlegend <- !is_area && !trace$name %in% in_legend
        if (trace$showlegend) in_legend <<- c(in_legend, trace$name)
        trace
    })
    widget
}
