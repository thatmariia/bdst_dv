# Run from the project root; each report renders in its own environment.
reports <- list.files("report", pattern = "\\.Rmd$", full.names = TRUE)
stopifnot(length(reports) > 0)
dir.create("_site", showWarnings = FALSE)
html <- vapply(reports, function(report) {
    rmarkdown::render(report, envir = new.env(), quiet = TRUE)
}, character(1))
stopifnot(all(file.copy(html, "_site", overwrite = TRUE)))
stopifnot(all(file.copy(reports, "_site", overwrite = TRUE)))
titles <- vapply(reports, function(report) {
    title <- rmarkdown::yaml_front_matter(report)$title
    if (is.null(title)) tools::file_path_sans_ext(basename(report)) else title
}, character(1))
links <- sprintf(
    paste0(
        '<li><h2>%s</h2><a href="%s">HTML report</a> · ',
        '<a href="%s" download>R Markdown (.Rmd)</a></li>'
    ),
    htmltools::htmlEscape(titles),
    URLencode(basename(html), reserved = TRUE),
    URLencode(basename(reports), reserved = TRUE)
)
writeLines(c(
    '<!doctype html><html lang="en"><head><meta charset="utf-8">',
    '<meta name="viewport" content="width=device-width, initial-scale=1">',
    "<title>Data visualization — Mariia Steeghs-Turchina</title>",
    "<style>",
    "body {font: 1rem/1.6 system-ui, sans-serif; color: #243746;",
    "  background: #faf9f6; max-width: 52rem; margin: auto; padding: 2rem;}",
    "h1 {line-height: 1.2;} h2 {font-size: 1.15rem; margin: 0 0 .5rem;}",
    "a {color: #006b9b;} a:hover {text-decoration-thickness: 2px;}",
    "ul {list-style: none; padding: 0;} li {padding: 1.5rem 0;",
    "  border-top: 1px solid #cdd4d9;}",
    "</style></head><body><main>",
    "<h1>Data visualization</h1><p>Mariia Steeghs-Turchina · DV7</p>",
    '<p><a href="https://github.com/thatmariia/bdst_dv">',
    "GitHub repository</a></p><ul>",
    links, "</ul></main></body></html>"
), "_site/index.html")
message("Reports ready in report/ and _site/.")
