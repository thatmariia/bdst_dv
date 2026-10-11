# Loads the exploration plot code and builds the datasets the plots use.
# Run after R/data/get-data.R, from the project root.
source("R/plotting/style.R")
source("R/plotting/plotly-helpers.R")
source("R/plotting/advice-bubbles.R")
source("R/exploration/data.R")
source("R/exploration/plot-compliance.R")
source("R/exploration/plot-joint.R")
source("R/exploration/plot-standardized.R")

compliance <- compliance_by_year(advice_reconsiderations)
national_mix <- national_provisional_mix(
    cvte_provisional, cvte_pupils, cvte_conditional
)
joint <- joint_distribution(cvte_conditional, national_mix, gap_matrix)
standardized <- standardize_tests(cvte_conditional, national_mix, gap_matrix)
