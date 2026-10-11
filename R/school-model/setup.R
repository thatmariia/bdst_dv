# Fits the school-level model and aggregates its results. Run after
# R/national-model/setup.R, from the project root. School fits are cached
# in data/derived/.
source("R/school-model/cache.R")
source("R/school-model/schools.R")
source("R/school-model/school-model.R")
source("R/school-model/results.R")

school_year <- 2025
schools <- school_data(
    school_year, advice_reconsiderations, final_advice, main_provider,
    school_weighting, school_locations, school_boards
)
school_estimates <- estimate_all_schools(
    schools,
    school_model(
        structural[[as.character(school_year)]], cvte_pupils,
        school_year
    ),
    gap_matrix
)
school_groups <- aggregate_schools(school_estimates)
