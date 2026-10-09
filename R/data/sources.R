#' Read the source registry (data/sources.csv): one row per source, worded as
#' in the paper, with the datasets it feeds. An empty years cell is computed
#' from the datasets' test_year; {name} in content is filled from `values`.
#' @param path Registry CSV path.
#' @return Tibble with source, content, years, unit, and datasets (list).
read_sources <- function(path = "data/sources.csv") {
    read_csv(
        path,
        col_types = cols(.default = col_character()),
        na = "", progress = FALSE
    ) |>
        mutate(datasets = strsplit(datasets, " ", fixed = TRUE))
}

#' Format test years as a range, naming missing years ("2015-2025 (no 2020)").
format_test_years <- function(years) {
    years <- sort(unique(years))
    missing <- setdiff(seq(min(years), max(years)), years)
    range <- paste0(min(years), "–", max(years))
    if (length(years) == 1) range <- years
    paste0(
        "test years ", range,
        if (length(missing)) {
            paste0(" (no ", paste(missing, collapse = ", "), ")")
        }
    )
}

#' Table of data sources, ready for knitr::kable().
#' @param sources Output of read_sources().
#' @param data Named list of all datasets in the registry.
#' @param values Named values for {name} placeholders in content.
#' @return Tibble with source, content, years, and unit.
sources_table <- function(sources, data, values = list()) {
    stopifnot(all(unlist(sources$datasets) %in% names(data)))
    sources |>
        rowwise() |>
        mutate(
            years = if (is.na(years)) {
                format_test_years(unlist(lapply(data[datasets], \(x) {
                    x$test_year
                })))
            } else {
                years
            },
            content = as.character(glue::glue_data(values, content))
        ) |>
        ungroup() |>
        select(source, content, years, unit)
}
