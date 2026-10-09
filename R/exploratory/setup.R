# Loads the exploratory plot code and builds the datasets the plots use.
# Run after R/data/get-data.R, from the project root.
source("R/exploratory/style.R")
source("R/exploratory/plotly-helpers.R")
source("R/exploratory/data.R")
source("R/exploratory/plots_compliance.R")
source("R/exploratory/plots_joint.R")
source("R/exploratory/plots_standard.R")

compliance <- compliance_by_year(advice_reconsiderations)
national_mix <- national_provisional_mix(
    cvte_provisional, cvte_pupils, cvte_conditional
)
joint <- joint_distribution(cvte_conditional, national_mix, gap_matrix)
standardized <- standardize_tests(cvte_conditional, national_mix, gap_matrix)
