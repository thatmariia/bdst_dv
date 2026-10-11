#' Position of each teacher advice on the school-type ladder (D6).
advice_values <- setNames(
    c(0, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5), provisional_levels
)

#' D6: mean advice, share vwo, and share combined advice per school-year.
#' @param final_advice Output of filter_final_advice().
#' @return One row per school-year, using midpoint counts.
summarise_final_advice <- function(final_advice) {
    final_advice |>
        summarise(
            pupils = sum(n),
            mean_advice = weighted.mean(advice_values[advice], n),
            share_vwo = sum(n[advice == "vwo"]) / pupils,
            share_combined = sum(n[grepl("/", advice)]) / pupils,
            .by = all_of(school_key)
        )
}
