# Radio-button selectors for plotly widgets (R/plotting/selectors.js).

#' Radio buttons above a plotly widget that show only the traces of the
#' chosen selector values.
#' @param traces One row of selector values per trace, in trace order; NA
#'   for a trace that is shown for every value of that selector.
#' @param selectors Named character vector: selector columns of `traces` and
#'   their labels, e.g. c(test_year = "Test year").
#' @param initial Named list with the first choice for every selector.
#' @param layouts Optional layout updates for each combination of selector
#'   values: a list of list(key = <named list of selector values>,
#'   patch = <named list of Plotly.relayout() updates>).
with_selectors <- function(widget, traces, selectors, initial, layouts = NULL) {
    script <- paste(
        readLines("R/plotting/selectors.js", encoding = "UTF-8"),
        collapse = "\n"
    )
    htmlwidgets::onRender(
        widget, script,
        data = list(
            selectors = lapply(
                names(selectors),
                function(column) {
                    list(column = column, label = selectors[[column]])
                }
            ),
            keys = select(traces, all_of(names(selectors))),
            initial = initial,
            layouts = layouts
        )
    )
}
