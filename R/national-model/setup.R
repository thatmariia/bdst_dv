# Loads the model code, fits the reduced model per year, separates the
# teacher and test errors, and decomposes eligibility. Run after
# R/data/get-data.R, from the project root.
source("R/plotting/style.R")
source("R/plotting/plotly-helpers.R")
source("R/plotting/selectors.R")
source("R/plotting/advice-bubbles.R")
source("R/national-model/counts.R")
source("R/national-model/likelihood.R")
source("R/national-model/fit.R")
source("R/national-model/structure.R")
source("R/national-model/decomposition.R")
source("R/national-model/sensitivity.R")
source("R/national-model/plot-fit.R")
source("R/national-model/plot-decomposition.R")
source("R/national-model/plot-sensitivity.R")

model_counts <- pseudo_counts(
    cvte_conditional, cvte_provisional, cvte_pupils, cvte_coverage
)
model_years <- sort(unique(model_counts$test_year))
names(model_years) <- model_years

# Step 1: fit the reduced model to the tables of each year.
fits <- lapply(
    model_years,
    function(year) {
        fit_year(count_matrices(model_counts, year))
    }
)
fit_quality <- bind_rows(lapply(fits, fit_summary), .id = "test_year") |>
    mutate(test_year = as.integer(test_year))

# Step 2: separate teacher and test error with Cito's resit share r.
resit_flip <- cito_misclassification |>
    filter(test_advice == "total") |>
    with((lower + higher) / 100)
structural <- lapply(fits, structural_parameters, flip = resit_flip)

# Step 3: decompose eligibility in the national population, among all
# pupils and per provisional advice (the latter for the explorer).
decomposition_by_advice <- bind_rows(
    lapply(
        model_years,
        function(year) {
            decompose_national(
                structural[[as.character(year)]], cvte_pupils, gap_matrix,
                year,
                advice = c("all", provisional_levels)
            )
        }
    )
)
decomposition <- decomposition_by_advice |>
    filter(advice == "all") |>
    select(-advice)

# Step 4: repeat the decomposition for other resit shares r.
sensitivity <- anchor_sensitivity(
    fits, cvte_pupils, gap_matrix,
    # with Cito's share itself, so it matches the decomposition above
    flips = sort(c(seq(0.01, 0.6, by = 0.01), resit_flip))
)
