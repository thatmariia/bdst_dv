# Downloads the DUO and school weighting (schoolweging) files into data/raw/
# (skipping files that are already there), tidies them, reads the report
# tables in data/reports/ (made once by the gitignored R/data/one-off/), and
# checks the sources against each other. Run from the project root.
library(dplyr)
library(jsonlite)
library(readr)
library(readODS)
library(tidyr)

source("R/data/download.R")
source("R/data/tidy-duo.R")
source("R/data/check-sources.R")
source("R/data/filter-data.R")
source("R/data/measures.R")
source("R/data/sources.R")

raw_dir <- "data/raw"

#' Look up the current download URL of a DUO open-data dataset.
#' @param id DUO CKAN dataset id.
#' @return URL of the dataset's first resource.
duo_resource_url <- function(id) {
    api <- "https://onderwijsdata.duo.nl/api/3/action/package_show?id="
    fromJSON(paste0(api, id))$result$resources$url[[1]]
}

duo_datasets <- c(
    advice_reconsiderations = "wpoheroverweging-v1",
    test_scores = "wpo-eindscores",
    final_advice = "wpoadvies-v1",
    reference_levels = "wporeferentieniveau-v1"
)
school_weighting_url <- paste0(
    "https://www.onderwijsinspectie.nl/site/binaries/site-content/",
    "collections/documents/2026/02/09/",
    "schoolweging-2022-2023---2023-2024---2024-2025/",
    "schoolweging-2022-2023-2024.ods"
)

paths <- file.path(
    raw_dir, c(
        advice_reconsiderations = "brin6_heroverweging.csv",
        test_scores = "brin6_eindscores.csv",
        final_advice = "brin6_advies.csv",
        reference_levels = "brin6_referentieniveau.csv",
        school_weighting = "schoolweging_2022-2025.ods"
    )
)
names(paths) <- c(names(duo_datasets), "school_weighting")

# Only ask the DUO API for a URL when the file still needs downloading.
for (name in names(duo_datasets)) {
    if (!file.exists(paths[[name]])) {
        download_file(duo_resource_url(duo_datasets[[name]]), paths[[name]])
    }
}
download_file(school_weighting_url, paths[["school_weighting"]])

# ---- DUO school files and school weighting ----
# test_year = reference_year + 1; suppressed counts carry n_lo/n_mid/n_hi.
advice_reconsiderations <- tidy_advice_reconsiderations(
    paths[["advice_reconsiderations"]]
)
test_scores <- tidy_test_scores(paths[["test_scores"]])
main_provider <- main_providers(test_scores)
final_advice <- tidy_final_advice(paths[["final_advice"]])
reference_levels <- tidy_reference_levels(paths[["reference_levels"]])
school_weighting <- tidy_school_weighting(paths[["school_weighting"]])

# ---- Conventions D1-D8 (R/data/filter-data.R, R/data/measures.R) ----
# Only the columns the study uses; main_provider needs the unfiltered
# test_scores. Join main_provider with provider_sample() when needed.
advice_reconsiderations <- advice_reconsiderations |>
    filter_reconsiderations() |>
    add_revision_shares()
main_provider <- filter_main_provider(main_provider)
test_scores <- filter_test_scores(test_scores)
final_advice <- filter_final_advice(final_advice)
advice_summary <- summarise_final_advice(final_advice)
reading_2f <- summarise_reading_2f(filter_reading_levels(reference_levels))
school_weighting <- filter_school_weighting(school_weighting)
rm(reference_levels)

# ---- Report tables (CvTE Terugblik 2025/2026, Cito 2022, DUO gap matrix) ----
read_report_table <- function(name) {
    read_csv(
        file.path("data/reports", paste0(name, ".csv")),
        show_col_types = FALSE, progress = FALSE
    ) |>
        mutate(across(any_of("test_year"), as.integer))
}
cvte_conditional <- read_report_table("cvte_conditional")
cvte_provisional <- read_report_table("cvte_provisional")
cvte_pupils <- read_report_table("cvte_pupils") |> filter(test_year >= 2025)
cvte_agreement <- read_report_table("cvte_agreement")
cvte_coverage <- read_report_table("cvte_coverage")
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
    advice_summary, advice_reconsiderations, main_provider, cvte_conditional,
    cvte_provisional, cvte_pupils, cvte_agreement, cito_misclassification,
    gap_matrix
)
print(source_checks, n = Inf)
if (!all(source_checks$ok)) stop("Sources disagree; see source_checks.")

# ---- Data overview: provenance (data/sources.csv) and loaded data ----
sources <- read_sources()
datasets <- sources$dataset
misclassified <- with(
    filter(cito_misclassification, test_advice == "total"), lower + higher
)
data_overview <- overview_table(
    sources, mget(datasets),
    values = list(misclassified = misclassified)
)
