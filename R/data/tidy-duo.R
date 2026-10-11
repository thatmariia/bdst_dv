#' Provisional (school) advice categories in the CvTE tables, low to high.
provisional_levels <- c(
    "pro", "vmbo bb", "vmbo bb/kb", "vmbo kb", "vmbo kb/gt", "vmbo gt",
    "vmbo gt/havo", "havo", "havo/vwo", "vwo"
)
#' Test advice categories, low to high.
test_advice_levels <- c(
    "pro/vmbo bb", "vmbo bb/kb", "vmbo kb/gt", "vmbo gt/havo", "havo/vwo", "vwo"
)

#' DUO advice codes 1-12 in brin6_advies, in DUO's documented order
#' (toelichting bestand nr. 4), using the same labels as the CvTE tables.
duo_advice_levels <- c(
    "vso", "pro", "vmbo bb", "vmbo bb/kb", "vmbo kb", "vmbo kb/gt", "vmbo gt",
    "vmbo gt/havo", "havo", "havo/vwo", "vwo", "no advice possible"
)

#' Read a DUO school file with English names and the shared year coding.
#' reference_year (DUO's PEILJAAR) 2023 = school year 2023-24 = test in
#' spring 2024. A suppressed count (-1, meaning 1-4 pupils) is kept as bounds
#' (1, 4) and midpoint 2.5.
#' @param path DUO brin6 CSV path.
#' @return Tibble with school keys, n_lo, n_mid, n_hi, and suppressed.
read_duo_school_file <- function(path) {
    read_csv(
        path,
        col_types = cols(.default = col_character()),
        progress = FALSE, num_threads = 1
    ) |>
        rename(
            institution_code = INSTELLINGSCODE,
            location_code = VESTIGINGSCODE,
            school_type = TYPE_PO,
            reference_year = PEILJAAR,
            pupils = AANTAL_LEERLINGEN
        ) |>
        mutate(
            school_type = recode(school_type, BO = "regular", SBO = "special"),
            reference_year = as.integer(reference_year),
            test_year = reference_year + 1L,
            pupils = as.numeric(pupils),
            suppressed = pupils == -1,
            n_lo = if_else(suppressed, 1, pupils),
            n_mid = if_else(suppressed, 2.5, pupils),
            n_hi = if_else(suppressed, 4, pupils)
        ) |>
        select(-pupils)
}

school_year_key <- c(
    "institution_code", "location_code", "school_type", "reference_year",
    "test_year"
)

#' Advice reconsiderations per school location and year, one column per
#' count. Categories missing for a school-year count as zero.
#' @param path brin6_heroverweging CSV path.
#' @return Tibble with total, higher, lower, raised as _lo/_mid/_hi.
tidy_advice_reconsiderations <- function(path) {
    read_duo_school_file(path) |>
        mutate(
            count = recode(
                TOETSEN_EN_ADVIEZEN,
                TOTAAL = "total",
                AANTAL_HOGER_TOETSADVIES = "higher",
                AANTAL_LAGER_TOETSADVIES = "lower",
                AANTAL_BIJSTELLINGEN = "raised"
            )
        ) |>
        select(all_of(school_year_key), count, n_lo, n_mid, n_hi) |>
        pivot_wider(
            names_from = count, values_from = c(n_lo, n_mid, n_hi),
            names_glue = "{count}_{sub('n_', '', .value)}", values_fill = 0
        ) |>
        relocate(
            starts_with("total"), starts_with("higher"),
            starts_with("lower"), starts_with("raised"),
            .after = test_year
        )
}

#' Test provider, mean score, and pupil count per school, year, and provider.
#' @param path brin6_eindscores CSV path.
#' @return Tibble with grade, provider, mean_score, and pupil counts.
tidy_test_scores <- function(path) {
    read_duo_school_file(path) |>
        transmute(
            across(all_of(school_year_key)),
            grade = as.integer(LEERJAAR),
            provider = trimws(DOORSTROOMTOETS_PO),
            mean_score = as.numeric(GEMIDDELDE_UITSLAG),
            n_exempted = as.numeric(AANTAL_ONTHEFFINGEN),
            suppressed, n_lo, n_mid, n_hi
        )
}

#' Main provider per school-year: the one with most grade-8 pupils, ties
#' broken alphabetically; main_share is its fraction of tested pupils.
#' @param test_scores Output of tidy_test_scores().
#' @return One row per school-year with provider and main_share.
main_providers <- function(test_scores) {
    test_scores |>
        filter(grade == 8L) |>
        group_by(across(all_of(school_year_key))) |>
        mutate(n_tested = sum(n_mid), main_share = n_mid / n_tested) |>
        arrange(desc(n_mid), provider, .by_group = TRUE) |>
        slice_head(n = 1) |>
        ungroup() |>
        select(all_of(school_year_key), provider, n_tested, main_share)
}

#' Final advice distribution (after revision) per school and year.
#' @param path brin6_advies CSV path.
#' @return Tibble with advice as an ordered factor and pupil counts.
tidy_final_advice <- function(path) {
    read_duo_school_file(path) |>
        transmute(
            across(all_of(school_year_key)),
            advice = factor(
                duo_advice_levels[as.integer(ADVIES)],
                levels = duo_advice_levels
            ),
            suppressed, n_lo, n_mid, n_hi
        )
}

#' Inspectorate school weighting (schoolweging) for all three school years.
#' OVT ("00AP|C1") starts with the institution code; the part after "|" is a
#' location, but not numbered like location_code, so join on institution_code.
#' @param path Schoolweging ODS path.
#' @return Tibble per school and year with weighting, spread, and pupils.
tidy_school_weighting <- function(path) {
    sheets <- c("2022-2023", "2023-2024", "2024-2025")
    bind_rows(
        lapply(
            sheets,
            function(sheet) {
                read_ods(path, sheet = sheet) |>
                    mutate(reference_year = as.integer(substr(sheet, 1, 4)))
            }
        )
    ) |>
        transmute(
            institution_code = sub("\\|.*$", "", OVT),
            inspectorate_location = sub("^.*\\|", "", OVT),
            name = Naam,
            board_number = as.character(BGNR),
            reference_year,
            test_year = reference_year + 1L,
            school_weighting = as.numeric(schoolweging),
            spread = as.numeric(spreiding),
            pupils = as.numeric(aantal_leerlingen)
        )
}

#' Names and regions of the school locations, from the DUO file of school
#' advices 2024-2025 that the course uses (semicolon-separated).
#' PEILDATUM_LEERLINGEN 20241001 is school year 2024-25, test year 2025.
#' @param path schooladviezen_2024-2025 CSV path.
#' @return Tibble per school location with name, place, municipality,
#'   province, and board number.
tidy_school_locations <- function(path) {
    read_delim(
        path,
        delim = ";", col_types = cols(.default = col_character()),
        progress = FALSE
    ) |>
        transmute(
            institution_code = INSTELLINGSCODE,
            location_code = VESTIGINGSCODE,
            school_type = recode(SOORT_PO, Bo = "regular", Sbo = "special"),
            test_year = as.integer(substr(PEILDATUM_LEERLINGEN, 1, 4)) + 1L,
            name = INSTELLINGSNAAM_VESTIGING,
            place = tools::toTitleCase(tolower(PLAATSNAAM)),
            municipality = GEMEENTENAAM,
            province = PROVINCIE,
            board_number = BEVOEGD_GEZAG_NUMMER
        )
}

#' Names of the school boards (bevoegde gezagen) in DUO's address register.
#' @param path besturenbo CSV path.
#' @return Tibble with board_number and board.
tidy_school_boards <- function(path) {
    read_csv(
        path,
        col_types = cols(.default = col_character()), progress = FALSE
    ) |>
        transmute(
            board_number = `BEVOEGD GEZAG NUMMER`,
            board = `BEVOEGD GEZAG NAAM`
        )
}
