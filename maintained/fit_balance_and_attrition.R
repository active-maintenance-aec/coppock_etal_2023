# coppock_etal_2023/maintained/fit_balance_and_attrition.R
# Output: output/fits/balance_estimates.csv, output/fits/joint_balance_tests.csv,
#   output/fits/attrition_estimates.csv
# Depends on: helpers.R, original/data/all_panels_long.rds
# Description: The models behind appendix Figures 4 to 8: each pre-treatment
#   covariate regressed on the condition indicators, a joint multinomial test of
#   balance, and the effect of condition on whether a respondent answered in
#   each wave.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds")) |>
  mutate(
    race_asian = as.numeric(lucid_race == "Asian"),
    race_black = as.numeric(lucid_race == "Black"),
    race_hispanic = as.numeric(lucid_race == "Hispanic"),
    race_other = as.numeric(lucid_race == "Other"),
    race_white = as.numeric(lucid_race == "White"),
    responded_w1 = !is.na(outcome_w1),
    responded_w2 = !is.na(outcome_w2),
    responded_w3 = !is.na(outcome_w3)
  )

balance_outcomes <- c(
  "lucid_pid_7n", "lucid_age", "race_asian", "race_black", "race_hispanic",
  "race_other", "race_white", "lucid_hhin", "political_knowledge_pre",
  "political_interest_pre", "cognitive_reflection_pre", "need_for_cognition_pre",
  "extraversion_pre", "agreeableness_pre", "conscientiousness_pre",
  "emotional_stability_pre", "openness_to_experience_pre"
)

# Appendix Figures 4 to 6: each covariate regressed on the condition indicators ----
balance_regs <- all_panels_long |>
  group_by(fc, panel_factor) |>
  reframe(tidy(lm_robust(
    formula(paste0("cbind(", paste(balance_outcomes, collapse = ", "), ") ~ treatment")),
    data = pick(everything())
  ))) |>
  filter(term != "(Intercept)") |>
  mutate(term = str_remove(term, "treatment"))

write_fit(balance_regs, "balance_estimates")

# Appendix Figure 7: a joint likelihood-ratio test per fact check and panel ----
joint_balance <- all_panels_long |>
  group_by(fc, panel_factor) |>
  group_map(function(d, key) {
    # multinom() evaluates its data argument in the calling frame rather than in
    # the frame it was called from, so passing the group through by name fails
    # inside group_map. do.call substitutes the data before the call is made.
    fit <- do.call(multinom, list(
      formula = formula(paste("treatment ~", rhs(covariates_full))),
      data = d, trace = FALSE
    ))
    tibble(fc = key$fc, panel_factor = key$panel_factor,
           raw = lrtest(fit)$`Pr(>Chisq)`[2])
  }) |>
  list_rbind() |>
  mutate(adjusted = p.adjust(raw, method = "BH"))

write_fit(joint_balance, "joint_balance_tests")

# Appendix Figure 8: treatment effects on whether a respondent answered ----
attrition_regs <- all_panels_long |>
  group_by(fc, panel, panel_factor, rating) |>
  reframe(tidy(lm_robust(
    cbind(responded_w1, responded_w2, responded_w3) ~ treatment,
    data = pick(everything())
  ))) |>
  filter(term != "(Intercept)", !is.nan(p.value)) |>
  mutate(
    term = str_remove(term, "treatment"),
    outcome = case_match(
      outcome,
      "responded_w1" ~ "Responded Wave 1",
      "responded_w2" ~ "Responded Wave 2",
      "responded_w3" ~ "Responded Wave 3"
    )
  )

write_fit(attrition_regs, "attrition_estimates")

print(tibble(
  balance_rows = nrow(balance_regs),
  joint_rows = nrow(joint_balance),
  attrition_rows = nrow(attrition_regs)
))
