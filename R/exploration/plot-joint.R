#' Interactive joint distribution of provisional and test advice: dot area is
#' the share of pupils (same scale in every year); cells under 0.05% of pupils
#' are left out. Hover shows both advices and the shares; radio buttons select
#' the test year.
#' @param joint Output of joint_distribution().
#' @param years Test years to include; defaults to all in `joint`.
#' @param title,subtitle Plot title and subtitle; subtitle = NULL leaves
#'   the subtitle out.
joint_plot <- function(
    joint,
    years = sort(unique(joint$test_year)),
    title = "Test advice is often lower than the teacher's advice",
    subtitle = NULL
) {
    joint |>
        filter(test_year %in% years) |>
        mutate(
            tooltip = sprintf(
                "provisional: %s<br>test: %s<br>%s of all pupils<br>%s of pupils with this provisional advice", # nolint: line_length_linter.
                short_advice(provisional), short_advice(test_advice),
                pct(share), pct(p)
            )
        ) |>
        advice_bubble_plot(
            title = title, subtitle = subtitle,
            selectors = c(test_year = "Test year"),
            initial = list(test_year = max(years))
        )
}
