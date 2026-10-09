provider_colour <- function(test) {
    case_when(
        startsWith(test, "Lib") ~ pal[["blue"]],
        startsWith(test, "IEP") ~ pal[["orange"]],
        TRUE ~ pal[["green"]]
    )
}

# Each test is labelled at its latest year, on the side its arrow points to.
standardized_labels <- standardized |>
    arrange(test_year) |>
    summarise(
        side = if_else(last(leniency) < first(leniency), -1, 1),
        leniency = last(leniency),
        disagreement = last(disagreement),
        .by = test
    )

#' Standardised leniency against disagreement per test and mode; arrows run
#' from 2025 to 2026.
standardized_plot <- standardized |>
    arrange(test, test_year) |>
    ggplot(aes(leniency, disagreement, colour = provider_colour(test))) +
    geom_vline(xintercept = 0, colour = "grey60") +
    geom_path(
        aes(group = test),
        arrow = arrow(length = unit(2, "mm"), type = "closed")
    ) +
    geom_point(aes(shape = factor(test_year)), size = 2.5) +
    geom_text(
        data = standardized_labels,
        aes(x = leniency + side * 0.008, label = test, hjust = (1 - side) / 2),
        size = 3
    ) +
    scale_colour_identity() +
    scale_shape_manual(values = c("2025" = 1, "2026" = 16), name = NULL) +
    scale_x_continuous(
        labels = scales::percent_format(accuracy = 1),
        expand = expansion(mult = 0.12)
    ) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
        x = "leniency: higher \u2212 lower",
        y = "disagreement: higher + lower"
    ) +
    theme_classic()
