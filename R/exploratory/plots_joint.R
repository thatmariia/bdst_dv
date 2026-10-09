pal <- c(
    blue = "#2a78d6",
    orange = "#eb6834",
    green = "#1baf7a",
    red = "#e34948",
    grey = "#d9d7d1"
)

# Display labels for the axes only ("vmbo bb/kb" -> "vmbo-b/k", "pro" ->
# "PrO"); the data keeps the shared labels from R/data/tidy-duo.R.
short_advice <- function(x) {
    x <- gsub("\\b(b|k)b\\b", "\\1", x)
    x <- gsub("vmbo ", "vmbo-", x)
    sub("^pro\\b", "PrO", x)
}

comparison_colours <- c(
    "test lower" = pal[["blue"]], "equal" = pal[["grey"]],
    "test higher" = pal[["red"]]
)

#' Interactive joint distribution of provisional and test advice: dot area is
#' the share of pupils (same scale in every year); cells under 0.05% of pupils
#' are left out. Hover shows both advices and the shares; a slider selects the
#' test year.
#' @param years Test years to include; defaults to all with CvTE tables.
joint_plot <- function(years = sort(unique(cvte_conditional$test_year))) {
    joint <- bind_rows(lapply(years, country_joint))
    unknown <- c(
        setdiff(joint$provisional, provisional_levels),
        setdiff(joint$test_advice, test_advice_levels)
    )
    if (length(unknown)) {
        stop(
            "Advice labels not in provisional_levels/test_advice_levels: ",
            paste(unknown, collapse = ", "),
            ". Restart R or re-source R/data/tidy-duo.R."
        )
    }
    plot <- joint |>
        filter(share > 0.0005) |>
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
                scales::percent(share, accuracy = 0.1),
                scales::percent(p, accuracy = 0.1)
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
        guides(colour = guide_legend(override.aes = list(size = 4))) +
        labs(x = "test advice", y = "teacher's provisional advice") +
        theme_classic() +
        theme(
            axis.line = element_line(colour = "grey85"),
            axis.ticks = element_line(colour = "grey40"),
            axis.text = element_text(colour = "grey30", size = 11),
            axis.title = element_text(colour = "grey30"),
            axis.text.x = element_text(angle = 25, hjust = 1),
            legend.position = "bottom"
        )

    # plotly ignores legend.position: the legend goes above the plot and the
    # year slider below the x-axis title. The frames act as a year selector:
    # no play button, no transition.
    plotly::ggplotly(plot, tooltip = "text") |>
        plotly::layout(
            legend = list(
                orientation = "h", x = 0.5, xanchor = "center",
                y = 1.02, yanchor = "bottom"
            ),
            margin = list(b = 90)
        ) |>
        plotly::animation_opts(frame = 0, transition = 0, redraw = TRUE) |>
        plotly::animation_button(visible = FALSE) |>
        plotly::animation_slider(
            y = -0.15, yanchor = "top",
            currentvalue = list(prefix = "Test year: ", xanchor = "left")
        )
}
