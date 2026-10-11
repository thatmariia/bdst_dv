# Prepares the eligibility explorer from the national and school results.
# Run after R/national-model/setup.R and R/school-model/setup.R, from the
# project root.
source("R/final/units.R")
source("R/final/explorer.R")

national_explorer <- national_units(
    structural, cvte_pupils, cvte_conditional, cvte_provisional, joint,
    gap_matrix
)
