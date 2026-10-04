#' Return the shared ggplot theme for the exploratory figures.
exploration_theme <- function() {
    theme_minimal(base_size = 12) +
        theme(
            panel.grid.minor = element_blank(),
            plot.title.position = "plot",
            plot.title = element_text(face = "bold"),
            legend.position = "bottom"
        )
}

#' Plot count coverage in 2023 and highlight the 80% threshold.
#' @param schools School-year table from prepare_data().
#' @return A ggplot histogram.
plot_coverage <- function(schools) {
    ggplot(
        filter(schools, year == 2023, !is.na(coverage)),
        aes(coverage)
    ) +
        geom_histogram(
            binwidth = 0.025, boundary = 0, fill = "#0072B2"
        ) +
        geom_vline(
            xintercept = 0.8, colour = "#D55E00", linewidth = 1
        ) +
        annotate(
            "text",
            x = 0.78, y = Inf, label = "80% coverage rule",
            hjust = 1, vjust = 1.5, colour = "#A54800"
        ) +
        scale_x_continuous(labels = label_percent()) +
        labs(
            title = "A. Check what the published counts cover",
            subtitle = paste(
                "Regular primary schools in 2023;", "missing totals excluded"
            ),
            x = paste(
                "Pupils with an exact count and usable CBS match",
                "/ total pupils"
            ),
            y = "Schools"
        ) +
        exploration_theme()
}

#' Plot neighbourhood composition across eligible schools in 2023.
#' @param schools School-year table from prepare_data().
#' @return A ggplot histogram.
plot_composition <- function(schools) {
    ggplot(
        filter(schools, year == 2023, eligible),
        aes(composition)
    ) +
        geom_histogram(
            binwidth = 0.025, boundary = 0, fill = "#0072B2"
        ) +
        scale_x_continuous(labels = label_percent()) +
        labs(
            title = "B. Schools draw from different neighbourhoods",
            subtitle = paste(
                "2023 enrolment weighted by",
                "home-postcode composition in 2022"
            ),
            x = "Neighbourhood composition proxy (definition above)",
            y = "Schools"
        ) +
        exploration_theme()
}

#' Compare each school's composition across years and flag large changes.
#' @param pairs Eligible two-year pairs from prepare_data().
#' @return A ggplot scatter plot.
plot_persistence <- function(pairs) {
    pairs$highlight <- abs(pairs$change) > 0.05
    pairs$tooltip <- paste0(
        "School: ", pairs$school_id,
        "<br>2022: ", label_percent()(pairs$composition_2022),
        "<br>2023: ", label_percent()(pairs$composition_2023)
    )
    ggplot(
        pairs,
        aes(composition_2022, composition_2023, text = tooltip)
    ) +
        geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
        geom_point(colour = "#0072B2", alpha = 0.2, size = 1.2) +
        geom_point(
            data = filter(pairs, highlight), shape = 17,
            colour = "#D55E00", size = 2.5
        ) +
        scale_x_continuous(labels = label_percent()) +
        scale_y_continuous(labels = label_percent()) +
        coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
        labs(
            title = "C. Composition changes little within a school",
            subtitle = paste(
                "Orange triangles: changes larger than", "5 percentage points"
            ),
            x = "Neighbourhood composition proxy, 2022 enrolment",
            y = "Neighbourhood composition proxy, 2023 enrolment",
            caption = paste(
                "Dashed line: no change.",
                "Both years use the same CBS vintage."
            )
        ) +
        exploration_theme()
}
