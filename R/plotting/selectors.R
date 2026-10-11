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

#' Whether a combination of selector values is the initial choice.
#' @param choice One-row data frame (or list) of selector values.
is_initial_choice <- function(choice, initial) {
    all(vapply(
        names(initial),
        \(name) identical(as.character(choice[[name]]), as.character(initial[[name]])), # nolint: line_length_linter.
        logical(1)
    ))
}

#' Layout updates for with_selectors() from a list of combinations, each a
#' list with the selector values (choice) and its layout updates (patch).
selector_layouts <- function(combinations) {
    lapply(
        combinations,
        \(combination) {
            list(key = as.list(combination$choice), patch = combination$patch)
        }
    )
}

#' Selector values of every trace, for combinations that add `per_choice`
#' traces each.
selector_keys <- function(combinations, per_choice) {
    bind_rows(lapply(
        combinations,
        \(combination) combination$choice[rep(1, per_choice), ]
    ))
}
