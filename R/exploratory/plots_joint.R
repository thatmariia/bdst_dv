#' Interactive joint distribution of provisional and test advice: dot area is
#' the share of pupils (same scale in every year); cells under 0.05% of pupils
#' are left out. Hover shows both advices and the shares; a slider selects the
#' test year.
#' @param joint Output of joint_distribution().
#' @param years Test years to include; defaults to all in `joint`.
joint_plot <- function(joint, years = sort(unique(joint$test_year))) {
    plot <- joint |>
        filter(test_year %in% years, share > 0.0005) |>
        mutate(
            provisional = factor(provisional, levels = rev(provisional_levels)),
            test_advice = factor(test_advice, levels = test_advice_levels),
            comparison = factor(
                case_when(
                    gap < 0 ~ "test lower",
                    gap == 0 ~ "equal",
                    gap > 0 ~ "test higher"
                ),
                levels = names(comparison_colours)
            ),
            tooltip = sprintf(
                "provisional: %s<br>test: %s<br>%s of all pupils<br>%s of pupils with this provisional advice", # nolint: line_length_linter.
                short_advice(provisional), short_advice(test_advice),
                pct(share), pct(p)
            )
        ) |>
        # text is for the plotly tooltip; ggplot2 only checks layer aesthetics.
        ggplot(aes(
            test_advice, provisional,
            size = share, colour = comparison, text = tooltip,
            frame = test_year, ids = paste(provisional, test_advice)
        )) +
        geom_point() +
        scale_x_discrete(labels = short_advice) +
        scale_y_discrete(labels = short_advice) +
        scale_size_area(max_size = 8, guide = "none") +
        scale_colour_manual(values = comparison_colours, name = NULL) +
        labs(x = "test advice", y = "teacher's provisional advice") +
        theme_report() +
        theme(axis.text.x = element_text(angle = 25, hjust = 1))

    # The legend goes above the plot and the year slider below the x-axis
    # title. The frames act as a year selector: no play button, no transition.
    tidy_ggplotly(plot) |>
        plotly::layout(
            title = plotly_title(
                "Test advice is often lower than the teacher's advice",
                paste(
                    "Share of pupils per combination of provisional and test",
                    "advice (dot area).<br>Cells under 0.05% of pupils are",
                    "hidden."
                )
            ),
            legend = plotly_legend("top", title = "test advice vs teacher's: "),
            margin = list(t = 125, b = 90)
        ) |>
        plotly::animation_opts(frame = 0, transition = 0, redraw = TRUE) |>
        plotly::animation_button(visible = FALSE) |>
        plotly::animation_slider(
            y = -0.15, yanchor = "top",
            currentvalue = list(
                prefix = "Test year: ", xanchor = "left",
                font = list(color = ink[["primary"]], size = 13)
            )
        )
}
