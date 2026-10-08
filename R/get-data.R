# Downloads the doorstroomtoets sources into data/raw/ (skipping files that are
# already there) and loads them. Run from the project root.
library(jsonlite)
library(readr)
library(readODS)

source("R/download.R")

raw_dir <- "data/raw"

#' Look up the current download URL of a DUO open-data dataset.
#' @param id DUO CKAN dataset id.
#' @return URL of the dataset's first resource.
duo_resource_url <- function(id) {
    api <- "https://onderwijsdata.duo.nl/api/3/action/package_show?id="
    fromJSON(paste0(api, id))$result$resources$url[[1]]
}

duo_datasets <- c(
    heroverwegingen = "wpoheroverweging-v1",
    eindscores = "wpo-eindscores",
    schooladviezen = "wpoadvies-v1",
    referentieniveaus = "wporeferentieniveau-v1"
)
other_sources <- c(
    schoolweging = paste0(
        "https://www.onderwijsinspectie.nl/site/binaries/site-content/",
        "collections/documents/2026/02/09/",
        "schoolweging-2022-2023---2023-2024---2024-2025/",
        "schoolweging-2022-2023-2024.ods"
    ),
    toetsadvies_tabellen = paste0(
        "https://duo.nl/open_onderwijsdata/images/",
        "tabellen-toetsscore-naar-toetsadvies-heroverwegingen-en-bijgestelde-",
        "adviezen.pdf"
    )
)

paths <- file.path(raw_dir, c(
    heroverwegingen = "brin6_heroverweging.csv",
    eindscores = "brin6_eindscores.csv",
    schooladviezen = "brin6_advies.csv",
    referentieniveaus = "brin6_referentieniveau.csv",
    schoolweging = "schoolweging_2022-2025.ods",
    toetsadvies_tabellen = "duo_tabellen_toetsadvies.pdf"
))
names(paths) <- c(names(duo_datasets), names(other_sources))

# Only ask the DUO API for a URL when the file still needs downloading.
for (name in names(duo_datasets)) {
    if (!file.exists(paths[[name]])) {
        download_file(duo_resource_url(duo_datasets[[name]]), paths[[name]])
    }
}
for (name in names(other_sources)) {
    download_file(other_sources[[name]], paths[[name]])
}

# DUO files hold every year in long format; -1 marks a suppressed count.
read_duo_csv <- function(path) {
    read_csv(path, na = c("", "NA"), show_col_types = FALSE, progress = FALSE)
}
heroverwegingen <- read_duo_csv(paths[["heroverwegingen"]])
eindscores <- read_duo_csv(paths[["eindscores"]])
schooladviezen <- read_duo_csv(paths[["schooladviezen"]])
referentieniveaus <- read_duo_csv(paths[["referentieniveaus"]])

# One sheet per school year; OVT ("00AP|C1") starts with INSTELLINGSCODE, but
# the part after "|" does not match VESTIGINGSCODE.
schoolweging <- read_ods(paths[["schoolweging"]], sheet = "2024-2025")

for (name in c(names(duo_datasets), "schoolweging")) {
    table <- get(name)
    stopifnot(nrow(table) > 0)
    message(name, ": ", nrow(table), " rows, ", ncol(table), " columns")
}
