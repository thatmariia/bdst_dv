# Small hand-calculated cases for suppression, joins, and sample selection.
source("R/prepare.R")
directory <- tempfile()
dir.create(directory)
paths <- setNames(
    file.path(directory, c("origins.csv", "totals.csv", "population.csv")),
    c("origins", "totals", "population")
)
population <- data.frame(
    Perioden = "2022JJ00", Geslacht = "T001038",
    Postcode = rep(c("PC1234 ", "PC5678 "), each = 2),
    Migratieachtergrond = rep(c("T001040", "2012657"), 2),
    Bevolking_1 = c(100, 20, 100, 60)
)
origins <- data.frame(
    INSTELLINGSCODE = c("00AA", "00AA", "00BB", "00CC", "00DD"),
    VESTIGINGSCODE = "00", TYPE_PO = "BO", PEILJAAR = "2022",
    POSTCODE4_DEELNEMER = c("1234", "5678", "0000", "1234", "1234"),
    AANTAL_LEERLINGEN = c(60, 30, 100, -1, 60)
)
totals <- data.frame(
    INSTELLINGSCODE = c("00AA", "00BB", "00CC", "00DD"),
    VESTIGINGSCODE = "00", TYPE_PO = "BO", PEILJAAR = "2022",
    AANTAL_LEERLINGEN = c(100, 100, 10, -1)
)
readr::write_csv(population, paths[["population"]])
readr::write_csv(rbind(
    origins, transform(origins, PEILJAAR = "2023")
), paths[["origins"]])
readr::write_csv(rbind(
    totals, transform(totals, PEILJAAR = "2023")
), paths[["totals"]])
result <- prepare_data(paths)
schools <- subset(result$schools, year == 2022)
stopifnot(
    identical(schools$school_id, c("00AA00", "00BB00", "00CC00", "00DD00")),
    isTRUE(all.equal(schools$composition[1], 1 / 3)),
    schools$coverage[1] == 0.9,
    all(is.na(schools$composition[2:3])),
    is.na(schools$coverage[4]),
    identical(schools$eligible, c(TRUE, FALSE, FALSE, FALSE)),
    nrow(result$pairs) == 1L,
    result$pairs$change == 0
)
# Duplicate origin keys must fail instead of multiplying counts in a join.
readr::write_csv(rbind(origins, origins[1, ]), paths[["origins"]])
stopifnot(inherits(try(prepare_data(paths), silent = TRUE), "try-error"))
intake_path <- file.path(directory, "intake.csv")
readr::write_delim(data.frame(
    SOORT_PO = c("Bo", "Bo", "Bo", "Sbo"),
    LEEFTIJD_4 = c("0", "12", "<5", "<5")
), intake_path, delim = ";")
intake <- check_intake(intake_path)
stopifnot(intake$positive_cells == 2, intake$suppressed_share == 0.5)
unlink(directory, recursive = TRUE)
message("Data checks passed.")
