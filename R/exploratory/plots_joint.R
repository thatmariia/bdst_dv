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
    "test higher (eligible)" = pal[["red"]]
)

#' Joint distribution of provisional and test advice in one test year: dot
#' area is the share of pupils; cells under 0.05% of pupils are left out.
joint_plot <- function(year) {
    joint <- country_joint(year)
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
    joint |>
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
            )
        ) |>
        ggplot(aes(
            test_advice, provisional,
            size = share, colour = comparison
        )) +
        geom_point() +
        scale_x_discrete(labels = short_advice) +
        scale_y_discrete(labels = short_advice) +
        scale_size_area(max_size = 14, guide = "none") +
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
}
