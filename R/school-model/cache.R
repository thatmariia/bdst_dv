# Cache for slow results, so that the report renders quickly after the first
# time. The cache is in data/derived/ (not in Git) and keyed by a hash of
# the inputs, so a change in the inputs computes the result again.

#' Value of `compute()`, read from the cache if the inputs are unchanged.
#' @param name Name of the cached result.
#' @param inputs List of everything the result depends on.
#' @param compute Function without arguments that computes the result.
cached <- function(name, inputs, compute) {
    path <- file.path(
        "data/derived", paste0(name, "-", rlang::hash(inputs), ".rds")
    )
    if (file.exists(path)) {
        return(readRDS(path))
    }
    result <- compute()
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    unlink(Sys.glob(file.path(dirname(path), paste0(name, "-*.rds"))))
    saveRDS(result, path)
    result
}
