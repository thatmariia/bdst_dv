# Cached school estimates and pupil-weighted results for groups of schools
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

#' Pupil-weighted results of groups of schools.
#' @param estimates Output of estimate_all_schools().
#' @return One row per group, with the decomposition and the cells (list
#'   columns), the observed shares, and the tests used.
aggregate_schools <- function(estimates) {
    members <- estimates$schools |>
        mutate(
            similar = weighting_band(school_weighting),
            row = row_number()
        )
    groups <- bind_rows(
        lapply(
            names(unit_levels),
            function(level) {
                column <- if (level == "board") "board" else level
                members |>
                    transmute(level = level, name = .data[[column]], row)
            }
        ),
        tibble(level = "national", name = "the Netherlands", row = members$row)
    )
    weighted <- function(rows, get) {
        weight <- members$total[rows] / sum(members$total[rows])
        Reduce(`+`, Map(\(k, w) w * get(estimates$results[[k]]), rows, weight))
    }
    groups |>
        summarise(rows = list(row), .by = c(level, name)) |>
        mutate(
            schools = lengths(rows),
            pupils = vapply(rows, \(r) sum(members$total[r]), 0),
            higher = vapply(rows, \(r) sum(members$higher[r]), 0) / pupils,
            lower = vapply(rows, \(r) sum(members$lower[r]), 0) / pupils,
            raised = vapply(rows, \(r) sum(members$raised[r]), 0) /
                vapply(rows, \(r) sum(members$higher[r]), 0),
            tests = vapply(
                rows,
                function(r) {
                    share <- tapply(members$total[r], members$provider[r], sum)
                    share <- sort(share / sum(share), decreasing = TRUE)
                    label <- paste0(round(100 * share), "%")
                    label[share < 0.005] <- "<1%"
                    paste0(names(share), " ", label, collapse = ", ")
                },
                ""
            ),
            decomposition = lapply(rows, weighted, \(r) r$decomposition),
            cells = lapply(rows, weighted, \(r) r$cells),
            .keep = "unused"
        )
}
