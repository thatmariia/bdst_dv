# Shared colours, labels, formats, and theme, so the same concept looks the
# same in every plot. Hues are the dataviz skill's reference palette; the
# combinations below were checked with its validator (light surface).
pal <- c(
    blue = "#2a78d6",
    orange = "#eb6834",
    aqua = "#1baf7a",
    green = "#008300",
    magenta = "#e87ba4",
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

# The same comparison in five steps, by the gap G(i, j): a full level (or
# more) and half a level lower or higher. The half steps are lighter tints
# of the same hues; each arm passes the validator's ordinal checks.
gap_colours <- c(
    "-1" = pal[["blue"]],
    "-0.5" = "#86b6ef",
    "0" = pal[["grey"]],
    "0.5" = "#f29b9a",
    "1" = pal[["red"]]
)

# Revision measures over time: lower and higher as above, raised in violet.
measure_colours <- c(
    "test advice higher" = pal[["red"]],
    "test advice lower" = pal[["blue"]],
    "advice raised" = pal[["violet"]]
)

# One-hue ramp for the share raised (compliance), ending in the raised colour.
raised_ramp <- c("#ecebf7", pal[["violet"]])

# One-hue ramp for counts; it ends at a mid blue so that dark cell labels
# stay readable.
count_ramp <- c(ink[["shade"]], "#6da7ec")

# Sources of eligibility in the decomposition; category design is the
# neutral baseline.
source_colours <- c(
    "category design" = pal[["grey"]],
    "teacher error" = pal[["magenta"]],
    "test error" = pal[["green"]],
    "test menu" = pal[["violet"]]
)

# A school's teacher leniency (school-level model): a darker step of the
# teacher error's magenta, validated between teacher error and test error.
leniency_colour <- "#a8336a"

# Providers: Lib, IEP, and all other tests.
provider_colours <- c(
    "Lib" = pal[["blue"]], "IEP" = pal[["orange"]],
    "other tests" = pal[["aqua"]]
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
