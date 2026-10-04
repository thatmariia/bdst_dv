#' Download a source once, keeping interrupted downloads out of the cache.
#' @param url Public source URL.
#' @param path Destination file path.
#' @return Path to the cached file.
download_file <- function(url, path) {
    if (file.exists(path)) {
        return(path)
    }
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(tmpdir = dirname(path))
    on.exit(unlink(temporary))
    status <- download.file(url, temporary, mode = "wb", quiet = TRUE)
    stopifnot(status == 0, file.info(temporary)$size > 0)
    stopifnot(file.rename(temporary, path))
    path
}

#' Cache CBS population counts for one fixed vintage (2022).
#' @param path Destination CSV path.
#' @return Path to the cached population file.
download_population <- function(path) {
    if (file.exists(path)) {
        return(path)
    }
    endpoint <- "https://opendata.cbs.nl/ODataApi/odata/83503NED/TypedDataSet"
    query <- paste(
        "Geslacht eq 'T001038' and Perioden eq '2022JJ00' and",
        "(Migratieachtergrond eq 'T001040' or",
        "Migratieachtergrond eq '2012657')"
    )
    url <- paste0(
        endpoint, "?$filter=", URLencode(query, reserved = TRUE)
    )
    pages <- list()
    while (!is.null(url)) {
        response <- fromJSON(url)
        pages[[length(pages) + 1L]] <- response$value
        url <- response[["odata.nextLink"]]
    }
    population <- bind_rows(pages)
    stopifnot(nrow(population) > 0)
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    temporary <- tempfile(tmpdir = dirname(path))
    on.exit(unlink(temporary))
    write_csv(population, temporary)
    stopifnot(file.rename(temporary, path))
    path
}

#' Download the four public sources used in the exploration.
#' @param raw_dir Folder for cached raw files.
#' @return Named paths for origins, totals, intake, and population.
download_data <- function(raw_dir = "data/raw") {
    options(timeout = max(300, getOption("timeout")))
    base <- "https://onderwijsdata.duo.nl/dataset/"
    sources <- c(
        origins = paste0(
            base, "4ea91ae5-51ef-48a4-aa67-7098343f75bc/resource/",
            "402c5a69-eea8-41b4-b8ad-ecac301c28a8/download/brin6_pc4.csv"
        ),
        totals = paste0(
            base, "cf80e90d-ed19-4a10-a138-00c5c345cb5e/resource/",
            "9278ae97-4014-49f4-91fc-8cc255c2595d/download/brin6_totaal.csv"
        ),
        intake = paste0(
            "https://duo.nl/open_onderwijsdata/images/",
            "03.-leerlingen-po-totaaloverzicht-2023-2024.csv"
        )
    )
    paths <- file.path(raw_dir, paste0(names(sources), ".csv"))
    names(paths) <- names(sources)
    for (name in names(sources)) download_file(sources[[name]], paths[[name]])
    c(paths, population = download_population(
        file.path(raw_dir, "population_2022.csv")
    ))
}
