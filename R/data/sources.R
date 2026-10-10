#' Read the source registry (data/sources.csv): one row per dataset with
#' its provenance. An empty years cell is computed from the dataset's
#' test_year; {name} in content is filled from `values`.
#' @param path Registry CSV path.
#' @return Tibble with dataset, source, citation (Pandoc keys), content,
#'   years, and unit.
read_sources <- function(path = "data/sources.csv") {
    read_csv(
        path,
        col_types = cols(.default = col_character()),
        na = "", progress = FALSE
    )
}

#' Format test years as a range, naming missing years ("2015-2025 (no 2020)").
format_test_years <- function(years) {
    years <- sort(unique(years))
    missing <- setdiff(seq(min(years), max(years)), years)
    range <- paste0(min(years), "\u2013", max(years))
    if (length(years) == 1) range <- years
    paste0(
        "test years ", range,
        if (length(missing)) {
            paste0(" (no ", paste(missing, collapse = ", "), ")")
        }
    )
}

#' Column names with short types, e.g. "test_year (int), n (dbl)".
describe_columns <- function(data) {
    data <- as.data.frame(data)
    types <- vapply(data, pillar::type_sum, "")
    paste0(names(data), " (", types, ")", collapse = ", ")
}

#' Overview of every dataset: provenance from the registry plus what was
#' loaded (rows, columns, and types).
#' @param sources Output of read_sources().
#' @param data Named list of the datasets in the registry.
#' @param values Named values for {name} placeholders in content.
#' @return Tibble with one row per dataset.
overview_table <- function(sources, data, values = list()) {
    stopifnot(setequal(sources$dataset, names(data)))
    data <- data[sources$dataset]
    sources |>
        mutate(
            content = vapply(content, function(text) {
                as.character(glue::glue_data(values, text))
            }, "", USE.NAMES = FALSE),
            years = mapply(function(given, x) {
                if (is.na(given)) format_test_years(x$test_year) else given
            }, years, data, USE.NAMES = FALSE),
            rows = vapply(data, NROW, 0L),
            columns = vapply(data, describe_columns, "")
        )
}
