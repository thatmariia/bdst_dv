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
    "vmbo gt/havo", "havo", "havo/vwo", "vwo", "advies niet mogelijk"
)

#' Providers behind the DOORSTROOMTOETS_PO codes in brin6_referentieniveau,
#' identified by matching school-years against brin6_eindscores.
duo_reference_providers <- c(
    "0011" = "CET", "0012" = "ROUTE 8", "0013" = "IEP", "0014" = "DIA",
    "0015" = "CESAN", "0016" = "AMN", "0017" = "DOE", "0018" = "LIB"
)

#' Read a DUO school file with lowercase names and the shared year coding.
#' PEILJAAR 2023 = school year 2023-24 = test in spring 2024. A suppressed
#' count (-1, meaning 1-4 pupils) is kept as bounds (1, 4) and midpoint 2.5.
#' @param path DUO brin6 CSV path.
#' @return Tibble with test_year, n_lo, n_mid, n_hi, and suppressed.
read_duo_school_file <- function(path) {
    read_csv(
        path,
        col_types = cols(.default = col_character()),
        progress = FALSE, num_threads = 1
    ) |>
        rename_with(tolower) |>
        mutate(
            peiljaar = as.integer(peiljaar),
            test_year = peiljaar + 1L,
            aantal_leerlingen = as.numeric(aantal_leerlingen),
            suppressed = aantal_leerlingen == -1,
            n_lo = if_else(suppressed, 1, aantal_leerlingen),
            n_mid = if_else(suppressed, 2.5, aantal_leerlingen),
            n_hi = if_else(suppressed, 4, aantal_leerlingen)
        ) |>
        select(-aantal_leerlingen)
}

school_year_key <- c(
    "instellingscode", "vestigingscode", "type_po", "peiljaar", "test_year"
)

#' Heroverwegingen per school location and year, one column per count.
#' Categories missing for a school-year count as zero.
#' @param path brin6_heroverweging CSV path.
#' @return Tibble with total, higher, lower, raised as _lo/_mid/_hi.
tidy_heroverwegingen <- function(path) {
    read_duo_school_file(path) |>
        mutate(count = recode(
            toetsen_en_adviezen,
            TOTAAL = "total",
            AANTAL_HOGER_TOETSADVIES = "higher",
            AANTAL_LAGER_TOETSADVIES = "lower",
            AANTAL_BIJSTELLINGEN = "raised"
        )) |>
        select(all_of(school_year_key), count, n_lo, n_mid, n_hi) |>
        pivot_wider(
            names_from = count, values_from = c(n_lo, n_mid, n_hi),
            names_glue = "{count}_{sub('n_', '', .value)}", values_fill = 0
        ) |>
        relocate(starts_with("total"), starts_with("higher"),
            starts_with("lower"), starts_with("raised"),
            .after = test_year
        )
}

#' Test provider and pupil count per school, year, and provider.
#' @param path brin6_eindscores CSV path.
#' @return Tibble with provider, leerjaar, mean score, and pupil counts.
tidy_eindscores <- function(path) {
    read_duo_school_file(path) |>
        mutate(
            provider = toupper(trimws(doorstroomtoets_po)),
            provider = recode(provider, "CITO EINDTOETS" = "CITO"),
            leerjaar = as.integer(leerjaar),
            gemiddelde_uitslag = as.numeric(gemiddelde_uitslag)
        )
}

#' Main provider per school-year: the one with most grade-8 pupils.
#' @param eindscores Output of tidy_eindscores().
#' @return One row per school-year with provider and main_share.
main_providers <- function(eindscores) {
    eindscores |>
        filter(leerjaar == 8L) |>
        group_by(across(all_of(school_year_key))) |>
        mutate(n_tested = sum(n_mid), main_share = n_mid / n_tested) |>
        slice_max(n_mid, n = 1, with_ties = FALSE) |>
        ungroup() |>
        select(all_of(school_year_key), provider, n_tested, main_share)
}

#' Final advice distribution (after revision) per school and year.
#' @param path brin6_advies CSV path.
#' @return Tibble with advice as an ordered factor and pupil counts.
tidy_advies <- function(path) {
    read_duo_school_file(path) |>
        mutate(advice = factor(
            duo_advice_levels[as.integer(advies)],
            levels = duo_advice_levels
        ))
}

#' Reference levels per school, year, and provider.
#' @param path brin6_referentieniveau CSV path.
#' @return Tibble with provider, subject, and level.
tidy_referentieniveaus <- function(path) {
    read_duo_school_file(path) |>
        mutate(
            provider = unname(duo_reference_providers[doorstroomtoets_po]),
            subject = sub("_(LAGER_1F|1F|1S|2F)$", "", referentieniveau),
            level = sub("^.*_(LAGER_1F|1F|1S|2F)$", "\\1", referentieniveau),
            level = factor(
                sub("LAGER_1F", "<1F", level),
                levels = c("<1F", "1F", "1S", "2F")
            )
        )
}

#' Inspectorate schoolweging for all three school-year sheets.
#' OVT ("00AP|C1") starts with the instellingscode; the part after "|" is a
#' location, but not numbered like VESTIGINGSCODE, so join on instellingscode.
#' @param path Schoolweging ODS path.
#' @return Tibble per school and year with schoolweging, spread, and pupils.
tidy_schoolweging <- function(path) {
    sheets <- c("2022-2023", "2023-2024", "2024-2025")
    bind_rows(lapply(sheets, function(sheet) {
        read_ods(path, sheet = sheet) |>
            mutate(peiljaar = as.integer(substr(sheet, 1, 4)))
    })) |>
        transmute(
            instellingscode = sub("\\|.*$", "", OVT),
            ovt_location = sub("^.*\\|", "", OVT),
            naam = Naam,
            bgnr = as.character(BGNR),
            peiljaar,
            test_year = peiljaar + 1L,
            schoolweging = as.numeric(schoolweging),
            spreiding = as.numeric(spreiding),
            aantal_leerlingen = as.numeric(aantal_leerlingen)
        )
}
