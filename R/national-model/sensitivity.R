# Sensitivity of the decomposition to the anchor, Cito's resit share r
# (@eq-resit-share).

#' National decomposition per year for every resit share in `flips`. A share
#' that gives a negative variance is incompatible with the tables; it is kept
#' with feasible = FALSE and missing values.
#' @param fits Named list (by year) of fit_year() results.
#' @param pupils cvte_pupils.
#' @param gap DUO gap matrix.
#' @param flips Resit shares r, as proportions.
anchor_sensitivity <- function(fits, pupils, gap, flips) {
    rows <- lapply(
        names(fits),
        function(year) {
            lapply(
                flips,
                function(flip) {
                    structure <- tryCatch(
                        structural_parameters(fits[[year]], flip),
                        error = function(e) NULL
                    )
                    if (is.null(structure)) {
                        return(tibble(
                            test_year = as.integer(year), flip = flip,
                            feasible = FALSE
                        ))
                    }
                    decompose_national(
                        structure, pupils, gap, as.integer(year)
                    ) |>
                        select(-advice) |>
                        national_decomposition(pupils) |>
                        mutate(flip = flip, feasible = TRUE)
                }
            )
        }
    )
    bind_rows(unlist(rows, recursive = FALSE))
}

#' What the tables fix and what depends on the anchor, per year: the
#' allowed resit shares, the range of the total and of category design over
#' them, whether category design is always the largest source, the share at
#' which test error catches up with teacher error, and the ratio of the two
#' at Cito's share.
#' @param sensitivity Output of anchor_sensitivity().
#' @param flip Cito's resit share r, as a proportion.
sensitivity_summary <- function(sensitivity, flip) {
    sensitivity |>
        filter(feasible) |>
        arrange(test_year, flip) |>
        summarise(
            allowed_low = min(flip),
            allowed_high = max(flip),
            eligible_low = min(eligible),
            eligible_high = max(eligible),
            design_low = min(category_design),
            design_high = max(category_design),
            design_largest = all(
                category_design > pmax(teacher_error, test_error, -test_menu)
            ),
            crossing = approx(teacher_error - test_error, flip, xout = 0)$y,
            ratio_at_anchor = (teacher_error / test_error)[
                which.min(abs(flip - .env$flip))
            ],
            .by = test_year
        )
}
