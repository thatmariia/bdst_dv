exploration_theme <- function() {
  ggplot2::theme_minimal(base_size = 12) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      plot.title.position = "plot",
      plot.title = ggplot2::element_text(face = "bold"),
      legend.position = "bottom"
    )
}

plot_coverage <- function(schools) {
  ggplot2::ggplot(
    dplyr::filter(schools, year == 2023, !is.na(coverage)),
    ggplot2::aes(coverage)
  ) +
    ggplot2::geom_histogram(binwidth = 0.025, boundary = 0, fill = "#0072B2") +
    ggplot2::geom_vline(xintercept = 0.8, colour = "#D55E00", linewidth = 1) +
    ggplot2::annotate(
      "text",
      x = 0.78, y = Inf, label = "80% coverage rule",
      hjust = 1, vjust = 1.5, colour = "#A54800"
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      title = "A. Check what the published counts cover",
      subtitle = "Regular primary schools in 2023; missing totals excluded",
      x = "Pupils with an exact count and usable CBS match / total pupils",
      y = "Schools"
    ) +
    exploration_theme()
}

plot_composition <- function(schools) {
  ggplot2::ggplot(
    dplyr::filter(schools, year == 2023, eligible),
    ggplot2::aes(composition)
  ) +
    ggplot2::geom_histogram(binwidth = 0.025, boundary = 0, fill = "#0072B2") +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::labs(
      title = "B. Schools draw from different neighbourhoods",
      subtitle = "2023 enrolment weighted by home-postcode composition in 2022",
      x = "Neighbourhood composition proxy (definition above)", y = "Schools"
    ) +
    exploration_theme()
}

plot_persistence <- function(pairs) {
  pairs$highlight <- abs(pairs$change) > 0.05
  ggplot2::ggplot(pairs, ggplot2::aes(composition_2022, composition_2023)) +
    ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    ggplot2::geom_point(colour = "#0072B2", alpha = 0.2, size = 1.2) +
    ggplot2::geom_point(
      data = dplyr::filter(pairs, highlight), shape = 17,
      colour = "#D55E00", size = 2.5
    ) +
    ggplot2::scale_x_continuous(labels = scales::label_percent()) +
    ggplot2::scale_y_continuous(labels = scales::label_percent()) +
    ggplot2::coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
    ggplot2::labs(
      title = "C. Composition changes little within a school",
      subtitle = "Orange triangles: changes larger than 5 percentage points",
      x = "Neighbourhood composition proxy, 2022 enrolment",
      y = "Neighbourhood composition proxy, 2023 enrolment",
      caption = "Dashed line: no change. Both years use the same CBS vintage."
    ) +
    exploration_theme()
}
