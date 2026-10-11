# Shared colours, fonts, labels, formats, and theme, so the same concept
# looks the same in every plot. The combinations below were checked with the
# dataviz skill's palette validator (light surface).
pal <- c(
    blue = "#3D69DD",
    sky = "#5DB1F7",
    green = "#3CBA77",
    aqua = "#64E9D1",
    pink = "#FF7BAC",
    periwinkle = "#7C7FD3",
    coral = "#F87553",
    orange = "#FF8C29",
    yellow = "#FED255",
    olive = "#BCBD42",
    grey = "#8E8D86"
)

# Text and chrome colours.
ink <- c(
    primary = "#1C1B19",
    secondary = "#55524C",
    muted = "#8E8C86",
    axis = "#D3D0C8",
    grid = "#ECE9E2",
    shade = "#F4F2ED"
)

# Fonts of the report (loaded in the report header): the same sans as the
# text and headings of the report, for titles too.
fonts <- c(
    title = "'IBM Plex Sans', 'Helvetica Neue', Arial, sans-serif",
    text = "'IBM Plex Sans', 'Helvetica Neue', Arial, sans-serif"
)

# Test advice relative to the teacher's (provisional/original) advice:
# diverging, with blue and a deep coral as opposite poles and a neutral grey
# middle.
comparison_colours <- c(
    "test lower" = pal[["blue"]],
    "equal" = pal[["grey"]],
    "test higher" = "#B83A24"
)

# The same comparison in five steps, by the gap G(i, j): a full level (or
# more) and half a level lower or higher. Each arm is one hue, lighter for
# half a level, and passes the validator's ordinal checks.
gap_colours <- c(
    "-1" = pal[["blue"]],
    "-0.5" = pal[["sky"]],
    "0" = pal[["grey"]],
    "0.5" = pal[["coral"]],
    "1" = comparison_colours[["test higher"]]
)

# Revision measures over time: lower and higher as above, raised in green
# (the lines are labelled directly, which the validator asks for this pair).
measure_colours <- c(
    "test advice higher" = comparison_colours[["test higher"]],
    "test advice lower" = pal[["blue"]],
    "advice raised" = pal[["green"]]
)

# One-hue ramp for the share raised (compliance), ending in the raised colour.
raised_ramp <- c("#E9F6EE", pal[["green"]])

# One-hue ramp for counts.
count_ramp <- c(ink[["shade"]], pal[["sky"]])

# Sources of eligibility in the decomposition; category design is the
# neutral baseline.
source_colours <- c(
    "category design" = pal[["grey"]],
    "teacher error" = pal[["pink"]],
    "test error" = pal[["green"]],
    "test menu" = pal[["periwinkle"]]
)

# A school's teacher leniency (school-level model): a deeper step of the
# teacher error's pink, validated between teacher error and test error.
leniency_colour <- "#C94A7F"

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
            plot.title = element_text(
                face = "bold", size = 14, colour = ink[["primary"]]
            ),
            plot.subtitle = element_text(colour = ink[["secondary"]]),
            plot.title.position = "plot",
            panel.grid.major.y = if (grid_y) {
                element_line(colour = ink[["grid"]], linewidth = 0.3)
            } else {
                element_blank()
            }
        )
}
