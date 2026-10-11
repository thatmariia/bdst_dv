# --fix formats files; a directory argument checks a staged Git snapshot.
args <- commandArgs(trailingOnly = TRUE)
fix <- identical(args, "--fix")
if (length(args) && !fix) setwd(args[[1]])
files <- list.files(
    c("R", "scripts", "report"),
    pattern = "\\.(R|Rmd|qmd)$", full.names = TRUE, recursive = TRUE
)
stopifnot(length(files) > 0)
# lintr only skips chunks whose engine knitr knows; Quarto renders these
# itself, so register no-op engines to keep them out of the R lints.
knitr::knit_engines$set(mermaid = function(options) NULL)
styler::cache_deactivate()
# tidyverse_style() does not pass indent_by to the rule for function
# declarations with one argument per line, which then indents by 2.
transformers <- styler::tidyverse_style(indent_by = 4L)
unindent <- transformers$indention$unindent_function_declaration
transformers$indention$unindent_function_declaration <- function(pd) {
    unindent(pd, indent_by = 4L)
}
style <- styler::style_file(
    files,
    dry = if (fix) "off" else "on", transformers = transformers
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
