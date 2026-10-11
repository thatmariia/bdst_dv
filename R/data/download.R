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
    for (attempt in seq_len(3L)) {
        status <- tryCatch(
            download.file(url, temporary, mode = "wb"),
            error = function(error) {
                message(conditionMessage(error))
                1L
            }
        )
        if (status == 0 && isTRUE(file.info(temporary)$size > 0)) break
        unlink(temporary)
        if (attempt == 3L) stop("Download failed after 3 attempts: ", url)
        message("Retrying download: ", url)
        Sys.sleep(2^attempt)
    }
    stopifnot(file.rename(temporary, path))
    path
}
