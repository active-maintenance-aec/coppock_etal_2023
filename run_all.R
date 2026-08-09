# coppock_etal_2023/run_all.R
# Runs the whole reproduction in order: fetch and verify the deposited archive,
# fit every model from the survey data, then the figures, then the tables, then
# the in-text quantities, then the ground truth. Every script is self-contained
# and can also be run on its own.

library(here)
here::i_am("run_all.R")

# Deposited archive ----
# Downloads from Dataverse on a fresh clone; verifies checksums either way.
source(here::here("download_original.R"))

# Models ----
# Everything downstream reads the fits these write, so they have to run first.
source(here::here("maintained", "fit_all_models.R"))
source(here::here("maintained", "fit_balance_and_attrition.R"))

# Main text figures ----
source(here::here("maintained", "figure_1_fc_traffic_and_control_agreement.R"))
source(here::here("maintained", "figure_2_raw_outcomes_by_treatment.R"))
source(here::here("maintained", "figure_3_cates_by_party.R"))
source(here::here("maintained", "figure_4_persistence.R"))
source(here::here("maintained", "figure_5_heterogeneity_by_trait.R"))
source(here::here("maintained", "figure_6_ft_targets.R"))

# Appendix figures ----
source(here::here("maintained", "figures_a1_a2_time_with_treatments.R"))
source(here::here("maintained", "figure_a3_fact_check_traffic_over_time.R"))
source(here::here("maintained", "figures_a4_a6_covariate_balance.R"))
source(here::here("maintained", "figure_a7_joint_balance_test.R"))
source(here::here("maintained", "figure_a8_attrition.R"))
source(here::here("maintained", "figure_a9_attitude_effects_by_congeniality.R"))

# Appendix tables ----
source(here::here("maintained", "table_a1_sample_characteristics.R"))
source(here::here("maintained", "tables_a2_a8_ate_cate_persistence.R"))

# In-text quantities ----
source(here::here("maintained", "text_main_claims.R"))
source(here::here("maintained", "text_balance_and_attrition.R"))
source(here::here("maintained", "text_descriptive_claims.R"))
source(here::here("maintained", "text_archive_fit_check.R"))

# The deposited archive as a program ----
# Runs every deposited script in a scratch copy, as shipped and again stripped to
# data plus code, and records where each one stopped. Then reads out of the
# deposit every published quantity it can answer, which is what value_script in
# the ground truth is built from.
source(here::here("ground_truth", "run_archive.R"))
source(here::here("ground_truth", "extract_archive_values.R"))

# Figure timestamps ----
# R's pdf() device stamps a wall-clock /CreationDate and /ModDate into every figure it
# writes, and those two fields are the only reason two runs of this pipeline produce
# differing files. Blanking them lets the determinism check cover every file the
# pipeline writes rather than all but the figures.
source(here::here("maintained", "helpers.R"))
walk(
  list.files(here::here("maintained", "output"), pattern = "\\.pdf$", full.names = TRUE),
  blank_pdf_timestamps
)

# Ground truth ----
# Rebuilds the comparison table from the outputs above, so it cannot go stale.
# The build also runs in_text_claims.R under capture.output for the coverage
# gate; the call below is the human-readable pass.
source(here::here("ground_truth", "build_ground_truth.R"))
source(here::here("maintained", "in_text_claims.R"))

# Deposited archive, again ----
# The gate inside download_original.R is a precondition: sourcing it first proves
# original/ was intact when the run began and says nothing about what the run did
# to it. Re-sourcing it here is what catches a script that damaged the deposit.
source(here::here("download_original.R"))
