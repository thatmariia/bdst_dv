# The eligibility explorer (@fig-explorer): where eligibility for a higher
# track comes from, for the Netherlands, a group of schools, or a single
# school, with what-ifs; and the test results of a child with a chosen
# provisional advice. R builds the two plotly figures and the data;
# R/final/explorer.js adds the controls and fills the figures.

# Rows of the waterfall: the sources and the total, with what they mean and
# who can change them (shown on hover over the info icon).
explorer_rows <- tibble(
    name = c(
        "Category design", "Teacher error", "Teachers' leniency",
        "Test error", "Test menu", "Eligible"
    ),
    info = c(
        paste(
            "Eligible even with a perfect teacher and a perfect test: the",
            "test reports two levels at once (such as havo/vwo), which counts",
            "as higher than a single advice (such as havo). Set by the",
            "national rules: the advice levels and when to raise."
        ),
        paste(
            "Teachers misjudge some pupils. Schools can change it in how they",
            "make the provisional advice."
        ),
        paste(
            "How much higher or lower the school's teachers advise than the",
            "teacher of the national model, for the same pupils. Not",
            "estimated for the Netherlands as a whole."
        ),
        paste(
            "Tests misjudge some pupils. Test providers can change it with",
            "more precise tests."
        ),
        paste(
            "How much stricter or more lenient the test is than the",
            "reference test, Lib paper. The CvTE can change it by equating",
            "the tests."
        ),
        paste(
            "Pupils whose test advice is higher than their provisional",
            "advice: the school must reconsider their advice."
        )
    ),
    colour = c(
        unname(source_colours[1:2]), leniency_colour,
        unname(source_colours[3:4]), ink[["secondary"]]
    )
)

# Test result compared with the provisional advice, by the gap G(i, j).
explorer_relations <- c(
    "-1" = "a level or more lower",
    "-0.5" = "half a level lower",
    "0" = "the same level",
    "0.5" = "half a level higher",
    "1" = "a level or more higher"
)

# Left margin, for the names of the sources.
explorer_left <- 150

#' Numeric columns rounded, because plotly writes up to 50 digits.
rounded <- function(data, digits = 5) {
    mutate(data, across(where(is.double), \(x) round(x, digits)))
}

#' Empty explorer figure: the waterfall of the sources of eligibility, in
#' percent of the pupils, with markers for the observed share and for the
#' share without the what-ifs (top); and 100 of the pupils as an icon array,
#' coloured by their test result, with the outcomes next to it (bottom). The
#' explorer fills in the values, texts, and annotations.
explorer_figure <- function(subtitle) {
    labels <- explorer_rows$name
    eligible <- labels[length(labels)]
    # a line colour (plotly_style()) would switch the axis line on
    hidden <- list(
        title = "", showgrid = FALSE, zeroline = FALSE, showline = FALSE,
        showticklabels = FALSE, fixedrange = TRUE
    )
    plot <- plotly::plot_ly() |>
        plotly::add_bars(
            x = rep(0, 6), base = rep(0, 6), y = labels, width = 0.5,
            marker = list(color = explorer_rows$colour), text = "",
            orientation = "h", textposition = "outside", cliponaxis = FALSE,
            textfont = plotly_font(12, ink[["primary"]]), hoverinfo = "text",
            showlegend = FALSE
        ) |>
        plotly::add_markers(
            x = 0, y = eligible, name = "observed",
            marker = list(
                symbol = "diamond", size = 11, color = ink[["primary"]],
                line = list(color = "white", width = 1.5)
            ),
            hoverinfo = "text"
        ) |>
        plotly::add_markers(
            x = 0, y = eligible, name = "without the what-if",
            marker = list(
                symbol = "line-ns-open", size = 22, color = ink[["primary"]],
                line = list(color = ink[["primary"]], width = 2)
            ),
            hoverinfo = "text", visible = FALSE
        )
    # the icons; their colours are named in the outcome texts next to them
    plot |>
        plotly::add_markers(
            x = 1, y = 1, marker = list(size = 12, color = ink[["muted"]]),
            hoverinfo = "text", showlegend = FALSE, xaxis = "x2", yaxis = "y2"
        ) |>
        plotly::layout(
            title = plotly_title("", subtitle),
            # room for a headline of three lines
            margin = list(
                t = plotly_top_margin(subtitle) + 44, l = explorer_left,
                r = 30, b = 20
            ),
            hoverlabel = list(align = "left"),
            # the legend names the markers on the waterfall, right of its title
            legend = list(
                orientation = "h", x = 1, xanchor = "right", y = 0.965,
                yanchor = "bottom", font = plotly_font(11)
            ),
            xaxis = list(
                title = "", ticksuffix = "%", anchor = "y",
                zeroline = FALSE, fixedrange = TRUE
            ),
            yaxis = list(
                title = "", domain = c(0.5, 0.95), type = "category",
                autorange = "reversed", categoryorder = "array",
                categoryarray = labels, ticks = "", showgrid = FALSE,
                # the names are annotations with an info icon (explorer.js)
                showticklabels = FALSE, fixedrange = TRUE
            ),
            # icons: square cells, the first row at the top
            xaxis2 = c(
                hidden,
                list(domain = c(0, 0.38), range = c(0.4, 10.6), anchor = "y2")
            ),
            yaxis2 = c(
                hidden,
                list(
                    domain = c(0, 0.38), range = c(10.6, 0.4), anchor = "x2",
                    scaleanchor = "x2", constrain = "domain",
                    constraintoward = "top"
                )
            )
        ) |>
        plotly_style()
}

#' Data of the explorer: the school model, the schools, the Netherlands, and
#' the shared labels and colours.
#' @param estimates Output of estimate_all_schools().
#' @param national Output of national_units().
#' @param compliance Output of compliance_by_year().
explorer_data <- function(estimates, national, gap, compliance) {
    model <- estimates$model
    schools <- estimates$schools |>
        transmute(
            id, name, place, municipality, province, board,
            similar = weighting_band(school_weighting),
            weighting = school_weighting, pupils = total, provider,
            higher = higher / total, raised_pupils = raised,
            raised = raised_share, mu, sigma, delta, se_delta
        ) |>
        rounded()
    latest_compliance <- filter(compliance, test_year == max(test_year))
    list(
        model = list(
            grid = list(
                from = min(school_grid), to = max(school_grid),
                n = length(school_grid)
            ),
            lambda = round(model$lambda, 6), tau = round(model$tau, 6),
            teacher_sd = round(model$teacher_sd, 6),
            versions = rounded(select(model$tests, test, alpha, beta, sd_T), 6),
            mixtures = lapply(
                model$mixtures,
                \(mixture) rounded(select(mixture, test, weight), 6)
            ),
            reference = reference_test
        ),
        gap = unname(gap),
        schools = as.list(schools),
        national = list(
            decomposition = rounded(national$decomposition),
            results = rounded(national$results),
            provisional = rounded(national$provisional),
            observed = rounded(national$observed),
            pupils = national$pupils,
            raised = list(
                year = latest_compliance$test_year,
                share = round(latest_compliance$compliance, 5)
            )
        ),
        labels = list(
            levels = as.list(
                c(national = "the Netherlands", unit_levels, school = "school")
            ),
            provisional = short_advice(provisional_levels),
            provisional_keys = provisional_levels,
            results = short_advice(test_advice_levels),
            result_keys = test_advice_levels,
            relations = as.list(explorer_relations),
            rows = explorer_rows$name,
            info = vapply(explorer_rows$info, wrap_html, "", 52,
                USE.NAMES = FALSE
            ),
            providers = lapply(provider_tests, as.list)
        ),
        style = list(
            ink = as.list(ink), gaps = as.list(gap_colours),
            left = explorer_left
        )
    )
}

#' Interactive eligibility explorer with its controls (R/final/explorer.js).
#' @param title,subtitle Title and subtitle; by default (NULL) the title is
#'   the headline of the chosen unit, and there is no subtitle.
eligibility_explorer <- function(estimates, national, gap, compliance,
                                 title = NULL, subtitle = NULL) {
    script <- paste(
        readLines("R/final/explorer.js", encoding = "UTF-8"),
        collapse = "\n"
    )
    data <- explorer_data(estimates, national, gap, compliance)
    data$title <- title
    data$subtitle <- subtitle
    htmlwidgets::onRender(explorer_figure(subtitle), script, data = data)
}
