# Cached school estimates, and the groups of schools of the explorer
# (province, municipality, school board, and similar school weighting).

# Levels of the groups of schools, with the column that defines them.
unit_levels <- c(
    province = "province", municipality = "municipality",
    board = "school board", similar = "similar schools"
)

#' Group of similar schools: school weighting in bands of two points.
weighting_band <- function(weighting) {
    low <- 2 * floor(weighting / 2)
    if_else(
        is.na(weighting), "school weighting unknown",
        sprintf("school weighting %d–%d", low, low + 2)
    )
}

#' Estimates and results of every school, cached because the fits take a few
#' minutes; a change in the data or in the code of the school-level model
#' computes them again.
#' @return List with the schools (data and estimates), the model with its
#'   final_shift, the prior, and per school the decomposition and the
#'   eligibility per test (both rules) and the cells.
estimate_all_schools <- function(schools, model, gap) {
    code <- readLines("R/school-model/school-model.R")
    cached("schools", list(schools, model, gap, code), function() {
        estimates <- estimate_schools(schools, model, gap)
        results <- lapply(
            seq_len(nrow(estimates$schools)),
            \(k) school_results(as.list(estimates$schools[k, ]), estimates$model, gap) # nolint: line_length_linter.
        )
        c(estimates, list(results = results))
    })
}
