# Shared colours, labels, formats, and theme, so the same concept looks the
# same in every plot. Hues are the dataviz skill's reference palette; the
# combinations below were checked with its validator (light surface).
pal <- c(
    blue = "#2a78d6",
    orange = "#eb6834",
    green = "#1baf7a",
    violet = "#4a3aa7",
    red = "#e34948",
    grey = "#898781"
)

# Text and chrome colours.
ink <- c(
    primary = "#0b0b0b",
    secondary = "#52514e",
    muted = "#898781",
    axis = "#c3c2b7",
    shade = "#f0efec"
)

# Test advice relative to the teacher's (provisional/original) advice:
# diverging, with blue and red as opposite poles and a neutral grey middle.
comparison_colours <- c(
    "test lower" = pal[["blue"]],
    "equal" = pal[["grey"]],
    "test higher" = pal[["red"]]
)

# Revision measures over time: lower and higher as above, raised in violet.
measure_colours <- c(
    "test advice higher" = pal[["red"]],
    "test advice lower" = pal[["blue"]],
    "advice raised" = pal[["violet"]]
)

# One-hue ramp for the share raised (compliance), ending in the raised colour.
raised_ramp <- c("#ecebf7", pal[["violet"]])

# Providers: Lib, IEP, and all other tests.
provider_colours <- c(
    "Lib" = pal[["blue"]], "IEP" = pal[["orange"]],
    "other tests" = pal[["green"]]
)

#' Provider group of a test ("Lib digital" -> "Lib").
provider_group <- function(test) {
    case_when(
        startsWith(test, "Lib") ~ "Lib",
        startsWith(test, "IEP") ~ "IEP",
        TRUE ~ "other tests"
    )
}

provider_colour <- function(test) unname(provider_colours[provider_group(test)])

#' Display labels for advice categories ("vmbo bb/kb" -> "vmbo-b/k",
#' "pro" -> "PrO"); the data keeps the shared labels from R/data/tidy-duo.R.
short_advice <- function(x) {
    x <- gsub("\\b(b|k)b\\b", "\\1", x)
    x <- gsub("vmbo ", "vmbo-", x)
    sub("^pro\\b", "PrO", x)
}

# Percentage formats: one decimal for values, none for axes and tiles.
pct <- scales::percent_format(accuracy = 0.1)
pct_whole <- scales::percent_format(accuracy = 1)

#' Base ggplot theme: white, hairline axes, ink-coloured text, and optional
#' horizontal gridlines.
theme_report <- function(grid_y = FALSE) {
    theme_classic(base_size = 11) +
        theme(
            axis.line = element_line(colour = ink[["axis"]]),
            axis.ticks = element_line(colour = ink[["muted"]]),
            axis.text = element_text(colour = ink[["secondary"]]),
            axis.title = element_text(colour = ink[["secondary"]]),
            plot.title = element_text(face = "bold"),
            plot.subtitle = element_text(colour = ink[["secondary"]]),
            plot.title.position = "plot",
            panel.grid.major.y = if (grid_y) {
                element_line(colour = "#e1e0d9", linewidth = 0.3)
            } else {
                element_blank()
            }
        )
}
