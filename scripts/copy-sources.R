# Quarto runs this after rendering, from the project root.
reports <- list.files("report", pattern = "\\.Rmd$", full.names = TRUE)
destination <- file.path(Sys.getenv("QUARTO_PROJECT_OUTPUT_DIR"), "downloads")
dir.create(destination, recursive = TRUE, showWarnings = FALSE)
stopifnot(all(file.copy(reports, destination, overwrite = TRUE)))
