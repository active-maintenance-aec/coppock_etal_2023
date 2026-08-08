# coppock_etal_2023/maintained/figures_a4_a6_covariate_balance.R
# Output: output/figure_a4_covariate_balance_race.pdf/.png/.csv,
#   output/figure_a5_covariate_balance_demographics.pdf/.png/.csv,
#   output/figure_a6_covariate_balance_personality.pdf/.png/.csv
# Depends on: helpers.R, fit_balance_and_attrition.R output
# Description: Appendix Figures 4 to 6. Each pre-treatment covariate regressed on
#   the condition indicators, one page per family of covariates. The three pages
#   are one plot applied to three subsets of a single set of estimates, so they
#   share a script.

source(here::here("maintained", "helpers.R"))

covariate_sets <- list(
  figure_a4_covariate_balance_race = c(
    "race_asian", "race_black", "race_hispanic", "race_other", "race_white"
  ),
  figure_a5_covariate_balance_demographics = c(
    "lucid_pid_7n", "lucid_age", "lucid_hhin", "political_knowledge_pre",
    "political_interest_pre", "cognitive_reflection_pre", "need_for_cognition_pre"
  ),
  figure_a6_covariate_balance_personality = c(
    "extraversion_pre", "agreeableness_pre", "conscientiousness_pre",
    "emotional_stability_pre", "openness_to_experience_pre"
  )
)

balance_regs <- read_fit("balance_estimates") |>
  mutate(significant = p.value <= 0.05)

stopifnot(setequal(unique(balance_regs$outcome), unlist(covariate_sets)))

balance_page <- function(data) {
  data |>
    ggplot(aes(estimate, term, group = fc, shape = significant, color = significant)) +
    geom_vline(xintercept = 0, linetype = "dashed") +
    geom_point(position = position_dodge(width = .5)) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = .5)) +
    scale_color_manual(values = c(`FALSE` = gray(0.5), `TRUE` = gray(0.1))) +
    facet_grid(panel_factor ~ outcome, scales = "free_x") +
    theme_bw() +
    theme(strip.background = element_blank(), strip.text.x = element_text(size = 6),
          axis.title.y = element_blank(), legend.position = "none") +
    labs(x = "Estimated effect on pre-treatment covariate, relative to the misinformation condition")
}

walk2(covariate_sets, names(covariate_sets), function(outcomes, name) {
  page_df <- balance_regs |> filter(outcome %in% outcomes)
  save_figure(balance_page(page_df), page_df, name, width = 8, height = 9)
})

print(balance_regs |> count(significant))
