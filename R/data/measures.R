#' D4: revision shares per school-year from midpoint counts: up = H/N,
#' down = L/N, raised_share = B/N, lean = up - down, and
#' disagreement = up + down. Bounds follow from the _lo/_hi counts.
#' @param reconsiderations Output of filter_reconsiderations().
#' @return The input plus the share columns.
add_revision_shares <- function(reconsiderations) {
    reconsiderations |>
        mutate(
            up = higher / total,
            down = lower / total,
            raised_share = raised / total,
            lean = up - down,
            disagreement = up + down
        )
}

#' D5: school-years for analyses by provider (main share >= 0.9, N >= 10).
#' @param reconsiderations Output of add_revision_shares().
#' @param main_provider Output of filter_main_provider().
#' @return reconsiderations restricted to those school-years, with provider.
provider_sample <- function(reconsiderations, main_provider) {
    reconsiderations |>
        inner_join(main_provider, by = school_key) |>
        filter(main_share >= 0.9, total >= 10)
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
            pupils = sum(n),
            mean_advice = weighted.mean(advice_values[advice], n),
            share_vwo = sum(n[advice == "vwo"]) / pupils,
            share_combined = sum(n[grepl("/", advice)]) / pupils,
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
            tested = sum(n),
            share_2f = sum(n[level == "2F"]) / tested,
            share_2f_lo = sum(n_lo[level == "2F"]) /
                (sum(n_lo[level == "2F"]) + sum(n_hi[level != "2F"])),
            share_2f_hi = sum(n_hi[level == "2F"]) /
                (sum(n_hi[level == "2F"]) + sum(n_lo[level != "2F"])),
            .by = c(all_of(school_key), provider)
        )
}
