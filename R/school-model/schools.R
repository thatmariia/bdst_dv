# School data of one test year for the school-level model: the counts that
# the model is fitted to, the test, and the names and regions to look up a
# school.

#' One row per school location: pupils with a higher, equal, or lower test
#' advice, the share of raised advices, the final advice per category, the
#' main test, the school weighting, and the name, place, municipality,
#' province, and school board.
#' @param year Test year.
#' @param reconsiderations advice_reconsiderations.
#' @param final_advice final_advice.
#' @param main_provider main_provider.
#' @param weighting school_weighting.
#' @param locations school_locations.
#' @param boards school_boards.
school_data <- function(year, reconsiderations, final_advice, main_provider,
                        weighting, locations, boards) {
    key <- c("institution_code", "location_code")
    advice <- final_advice |>
        filter(test_year == year) |>
        select(all_of(key), advice, n) |>
        tidyr::pivot_wider(
            names_from = advice, values_from = n, values_fill = 0,
            names_expand = TRUE
        )
    reconsiderations |>
        filter(test_year == year) |>
        mutate(equal = pmax(total - higher - lower, 0)) |>
        select(all_of(key), total, higher, lower, equal, raised) |>
        inner_join(
            main_provider |>
                filter(test_year == year) |>
                select(all_of(key), provider),
            by = key
        ) |>
        inner_join(advice, by = key) |>
        inner_join(
            locations |>
                filter(test_year == year) |>
                select(-test_year),
            by = key
        ) |>
        left_join(boards, by = "board_number") |>
        left_join(
            weighting |>
                filter(test_year == year) |>
                select(institution_code, school_weighting),
            by = "institution_code"
        ) |>
        mutate(
            id = paste0(institution_code, location_code),
            board = coalesce(board, paste("board", board_number)),
            # without pupils with a higher test advice, the national share
            raised_share = if_else(
                higher > 0, pmin(raised / higher, 1),
                sum(raised) / sum(higher)
            )
        )
}
