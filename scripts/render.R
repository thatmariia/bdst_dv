# Run from the project root; each report renders in its own environment.
reports <- list.files("report", pattern = "\\.Rmd$", full.names = TRUE)
stopifnot(length(reports) > 0)
dir.create("_site", showWarnings = FALSE)
html <- vapply(reports, function(report) {
  rmarkdown::render(report, envir = new.env(), quiet = TRUE)
}, character(1))
stopifnot(all(file.copy(html, "_site", overwrite = TRUE)))
links <- sprintf(
  '<li><a href="%s">%s</a></li>',
  basename(html), tools::file_path_sans_ext(basename(html))
)
writeLines(c(
  '<!doctype html><html lang="en"><meta charset="utf-8">',
  '<meta name="viewport" content="width=device-width, initial-scale=1">',
  "<title>Data visualization — Mariia Steeghs-Turchina</title>",
  "<h1>Data visualization</h1><p>Mariia Steeghs-Turchina · DV7</p><ul>",
  links, "</ul></html>"
), "_site/index.html")
message("Reports ready in report/ and _site/.")
