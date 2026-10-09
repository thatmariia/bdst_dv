# Downloads the DUO and schoolweging files into data/raw/ (skipping files that
# are already there), tidies them, reads the report tables in data/reports/
# (made once by the gitignored R/one-off/), and checks the sources against each
# other. Run from the project root.
library(dplyr)
library(jsonlite)
library(readr)
library(readODS)
library(tidyr)

source("R/download.R")
source("R/tidy-duo.R")
source("R/check-sources.R")

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
schoolweging_url <- paste0(
    "https://www.onderwijsinspectie.nl/site/binaries/site-content/",
    "collections/documents/2026/02/09/",
    "schoolweging-2022-2023---2023-2024---2024-2025/",
    "schoolweging-2022-2023-2024.ods"
)

paths <- file.path(raw_dir, c(
    heroverwegingen = "brin6_heroverweging.csv",
    eindscores = "brin6_eindscores.csv",
    schooladviezen = "brin6_advies.csv",
    referentieniveaus = "brin6_referentieniveau.csv",
    schoolweging = "schoolweging_2022-2025.ods"
))
names(paths) <- c(names(duo_datasets), "schoolweging")

# Only ask the DUO API for a URL when the file still needs downloading.
for (name in names(duo_datasets)) {
    if (!file.exists(paths[[name]])) {
        download_file(duo_resource_url(duo_datasets[[name]]), paths[[name]])
    }
}
download_file(schoolweging_url, paths[["schoolweging"]])

# ---- DUO school files and schoolweging ----
# test_year = PEILJAAR + 1; suppressed counts carry n_lo/n_mid/n_hi.
heroverwegingen <- tidy_heroverwegingen(paths[["heroverwegingen"]])
eindscores <- tidy_eindscores(paths[["eindscores"]])
main_provider <- main_providers(eindscores)
schooladviezen <- tidy_advies(paths[["schooladviezen"]])
referentieniveaus <- tidy_referentieniveaus(paths[["referentieniveaus"]])
schoolweging <- tidy_schoolweging(paths[["schoolweging"]])

# ---- Report tables (CvTE Terugblik 2025/2026, Cito 2022, DUO gap matrix) ----
read_report_table <- function(name) {
    read_csv(
        file.path("data/reports", paste0(name, ".csv")),
        show_col_types = FALSE, progress = FALSE
    )
}
cvte_conditional <- read_report_table("cvte_conditional")
cvte_provisional <- read_report_table("cvte_provisional")
cvte_pupils <- read_report_table("cvte_pupils")
cvte_agreement <- read_report_table("cvte_agreement")
cito_misclassification <- read_report_table("cito_misclassification")
gap_cells <- read_report_table("gap_matrix")
gap_matrix <- matrix(
    NA_real_,
    nrow = length(provisional_levels), ncol = length(test_advice_levels),
    dimnames = list(provisional_levels, test_advice_levels)
)
gap_matrix[cbind(gap_cells$provisional, gap_cells$test_advice)] <- gap_cells$gap
stopifnot(!anyNA(gap_matrix))

# ---- Checks between sources ----
source_checks <- check_sources(
    schooladviezen, heroverwegingen, main_provider, cvte_conditional,
    cvte_provisional, cvte_pupils, cvte_agreement, cito_misclassification,
    gap_matrix
)
print(source_checks, n = Inf)
if (!all(source_checks$ok)) stop("Sources disagree; see source_checks.")
