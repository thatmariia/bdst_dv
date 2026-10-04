# --fix formats files; a directory argument checks a staged Git snapshot.
args <- commandArgs(trailingOnly = TRUE)
fix <- identical(args, "--fix")
if (length(args) && !fix) setwd(args[[1]])
files <- list.files(
    c("R", "scripts", "report"),
    pattern = "\\.(R|Rmd)$", full.names = TRUE, recursive = TRUE
)
files <- c(files, "index.qmd")
stopifnot(length(files) > 0)
styler::cache_deactivate()
style <- styler::style_file(
    files,
    dry = if (fix) "off" else "on", indent_by = 4L
)
lint_count <- 0L
for (file in files) {
    lints <- lintr::lint(file, cache = FALSE)
    if (length(lints)) print(lints)
    lint_count <- lint_count + length(lints)
}
if (anyNA(style$changed) || (!fix && any(style$changed)) || lint_count > 0) {
    stop(
        "Run Rscript scripts/check.R --fix, then review and stage the changes."
    )
}
message("Lint and style checks passed.")
