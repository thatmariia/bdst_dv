#' D4: revision shares per school-year: up = H/N, down = L/N, and
#' raised_share = B/N from midpoints, each with _lo/_hi from the suppression
#' bounds (N itself is never suppressed here), plus lean and disagreement.
#' @param reconsiderations Output of filter_reconsiderations().
#' @return The input plus the share columns.
add_revision_shares <- function(reconsiderations) {
    reconsiderations |>
        mutate(
            up = higher_mid / total,
            up_lo = higher_lo / total, up_hi = higher_hi / total,
            down = lower_mid / total,
            down_lo = lower_lo / total, down_hi = lower_hi / total,
            raised_share = raised_mid / total,
            raised_share_lo = raised_lo / total,
            raised_share_hi = raised_hi / total,
            lean = up - down,
            disagreement = up + down
        )
}

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
            pupils = sum(n_mid),
            mean_advice = weighted.mean(advice_values[advice], n_mid),
            share_vwo = sum(n_mid[advice == "vwo"]) / pupils,
            share_combined = sum(n_mid[grepl("/", advice)]) / pupils,
            .by = all_of(school_key)
        )
}

#' D8: share of tested pupils at reading level 2F per school, year, and
#' provider, with bounds from the suppression bounds.
#' @param reading_levels Output of filter_reading_levels().
#' @return One row per school-year-provider with share_2f and its bounds.
summarise_reading_2f <- function(reading_levels) {
    reading_levels |>
        summarise(
            tested = sum(n_mid),
            share_2f = sum(n_mid[level == "2F"]) / tested,
            share_2f_lo = sum(n_lo[level == "2F"]) /
                (sum(n_lo[level == "2F"]) + sum(n_hi[level != "2F"])),
            share_2f_hi = sum(n_hi[level == "2F"]) /
                (sum(n_hi[level == "2F"]) + sum(n_lo[level != "2F"])),
            .by = c(all_of(school_key), provider)
        )
}
