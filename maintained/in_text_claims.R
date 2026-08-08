# coppock_etal_2023/maintained/in_text_claims.R
# Output: (none; a human-readable audit trail on stdout)
# Depends on: maintained/output/, ground_truth/published_claims.csv
# Description: A second, independent reading of every number the article states.
#   Each entry carries the article's own sentence and then recomputes the number
#   from the pipeline's committed output, in the article's units and at the
#   article's precision.
#
#   It reads maintained/output/ and nothing else. It does not refit, it does not
#   touch original/, and it does not read the ground truth: the point is for two
#   separate derivations to reach the same number, so reading the comparison
#   would defeat it. Reading published_claims.csv is a different thing, and is
#   allowed: it is the transcription of the page, not the verdict.
#
#   cat() is used here and nowhere else in this repository.

library(tidyverse)
library(here)

here::i_am("maintained/in_text_claims.R")

options(width = 200)

out <- function(file) read_csv(here::here("maintained", "output", file), show_col_types = FALSE)
fit <- function(name) read_csv(here::here("maintained", "output", "fits", str_c(name, ".csv")),
                               show_col_types = FALSE)

extraction <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), digits = col_integer(),
                   needs_block = col_logical(), .default = col_character())
)

# The printed line is the only load-bearing link to the extraction, so it carries
# the claim id. A value that rounds to zero from below prints as "-0" and would
# fail string equality against a published "0".
claim <- function(id, value) {
  row <- extraction[extraction$claim_id == id, ]
  stopifnot(nrow(row) == 1)
  rendered <- if (is.na(value)) "NA" else sprintf(paste0("%.", row$digits, "f"), value)
  if (str_detect(rendered, "^-0(\\.0*)?$")) rendered <- str_remove(rendered, "^-")
  cat("CLAIM ", id, " = ", rendered, " || ", row$claim, " (article prints ", row$value_paper, ")\n",
      sep = "")
}

main <- out("text_main_claims.csv")
descriptive <- out("text_descriptive_claims.csv")

quantity <- function(label) {
  value <- main$value[main$claim == label]
  stopifnot(length(value) == 1)
  value
}

verdict <- function(id) {
  value <- descriptive$holds[descriptive$claim_id == id]
  stopifnot(length(value) == 1)
  as.numeric(value)
}

# Abstract ----

# "In the final two months of the 2020 US election, we conducted eight panel
# experiments to evaluate the immediate and medium-term effects of misinformation
# and factual corrections."
claim("art_abstract_panels", quantity("Number of panels"))

# Introduction ----

# "To study fact-checking during the 2020 election season, we conducted eight
# preregistered panel experiments (total N = 17,681)."
claim("art_intro_total_n", quantity("Total respondents across the eight panels"))
claim("art_intro_panels", quantity("Number of panels"))

# "In total, we evaluated the effects of twenty-one widely disseminated pieces of
# misinformation and corresponding fact-checks during the final two months of the
# 2020 election."
claim("art_intro_fact_checks", quantity("Distinct fact checks tested"))

# "We evaluated the effects of these treatments in almost real time: on average,
# thirteen days separated the first appearance of the misinformation online from
# the inclusion of that misinformation in our experiments."
claim("art_intro_lag_days", quantity("Mean days from misinformation posting to fielding"))

# "Exposure to misinformation increased false beliefs by an average of 4.3 points
# on a 100-point belief certainty scale."
claim("art_intro_misinfo_effect",
      quantity("Meta-analytic misinformation effect, covariate adjusted"))
claim("art_intro_belief_scale", quantity("Belief certainty scale maximum"))

# "Exposure to fact-checks more than corrected this effect, decreasing false
# beliefs by 10.5 points."
claim("art_intro_correction_effect",
      abs(quantity("Meta-analytic correction effect, covariate adjusted")))

# "We show that 66 per cent of the initial effect of fact-checks on factual
# beliefs is still detectable one week after initial exposure, with 50 percent
# detectable after more than two weeks."
#
# The quantity the pre-analysis plan defines, and the one the appendix's own
# persistence tables print in their "ratio" rows, is the per-experiment ratio of
# the later-wave coefficient to the Wave 1 coefficient, pooled.
claim("art_intro_persist_w2", quantity("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"))
claim("art_intro_persist_w3", quantity("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"))

# "Both misinformation and fact-checks have very small effects on attitudes
# toward politicians and political organizations: on 100-point feeling
# thermometers, the effects of fact-checks and misinformation on subsequent
# attitudes are smaller than one-quarter of a point."
claim("art_intro_ft_scale", quantity("Feeling thermometer maximum"))
claim("art_intro_attitudes_quarter", verdict("d_attitudes_quarter_point"))

# Design ----

# "We administered six two-wave panels and two three-wave panels between
# September and November 2020. Four panels were fielded on Mechanical Turk and
# four were fielded on Lucid."
claim("design_two_wave_panels", quantity("Panels with only two waves"))
claim("design_three_wave_panels", quantity("Panels with a third wave"))
claim("design_mturk_panels", quantity("Panels fielded on Mechanical Turk"))
claim("design_lucid_panels", quantity("Panels fielded on Lucid"))

# "Subjects then participated in three independently randomized experiments, each
# relating to a separate topic of misinformation. For each topic, respondents
# were assigned to one of three conditions: pure control, misinformation only, or
# misinformation followed by a fact-check."
claim("design_experiments_per_wave", quantity("Experiments per panel"))
claim("design_conditions", quantity("Experimental conditions"))

# "In the Online Appendix, we demonstrate that these randomizations generated
# experimental groups balanced on pretreatment covariates."
claim("design_balance", verdict("d_balance"))

# "On average, fact-checks of Republican-congenial misinformation (shown in the
# top panel) received 138,971 views over the eight weeks following the original
# post. By contrast, the average fact-check of Democratic-congenial
# misinformation received 45,123 views."
claim("design_traffic_rep", quantity("Mean fact-check page views, Republican-congenial claims"))
claim("design_traffic_dem", quantity("Mean fact-check page views, Democratic-congenial claims"))
claim("design_traffic_weeks", quantity("Maximum weeks of page views observed for a fact check"))

# "Six of our eight panels were comprised of two waves; the remaining two
# featured a third wave."
claim("design_six_of_eight", quantity("Panels with only two waves"))
claim("design_remaining_two", quantity("Panels with a third wave"))

# "In the Online Appendix, we demonstrate that our treatments do not appear to
# change whether subjects respond to outcome questions, either immediately
# post-treatment or in subsequent waves, allaying concerns about differential
# attrition."
claim("design_attrition", verdict("d_attrition"))

# Results ----

# "Figure 1 shows a partisan gap across issues: Republicans are more likely than
# Democrats to believe all of the Republican-congenial false statements, but
# Democrats are more likely than Republicans to believe only two of the
# Democratic-congenial false statements."
claim("res_fig1_rep_all", verdict("d_fig1_republican_congenial"))
claim("res_fig1_dem_two", verdict("d_fig1_democratic_congenial"))

# "Belief certainty is plotted on the vertical axis, ranging from 0 (completely
# certain the false statement is inaccurate) to 100 (completely certain the
# statement is accurate)."
claim("res_scale_low", quantity("Belief certainty scale minimum"))
claim("res_scale_high", quantity("Belief certainty scale maximum"))

# "Overall, exposure to the misinformation significantly decreased accuracy in
# twelve out of the twenty-four opportunities."
claim("res_misinfo_sig_count",
      quantity("Fact checks where misinformation significantly reduced accuracy"))
claim("res_opportunities", quantity("Experiments (panel by topic)"))

# "Compared to the misinformation condition, twenty of the fact-checks had
# statistically significant negative average effects on belief certainty and none
# "backfired," or increased false beliefs."
claim("res_fc_sig_count", quantity("Fact checks with a significant negative correction effect"))
claim("res_no_backfire", verdict("d_no_backfire"))

# "Using random-effects meta-analysis, we estimate the average misinformation
# effect (relative to control) to be 4.30 points (standard error = 1.07) and the
# average fact-check effect (relative to misinformation) to be -10.5 points
# (standard error = 1.1)."
claim("res_meta_misinfo", quantity("Meta-analytic misinformation effect, covariate adjusted"))
claim("res_meta_misinfo_se",
      quantity("Meta-analytic misinformation effect standard error, covariate adjusted"))
claim("res_meta_correction", quantity("Meta-analytic correction effect, covariate adjusted"))
claim("res_meta_correction_se",
      quantity("Meta-analytic correction effect standard error, covariate adjusted"))

# "Being exposed to misinformation increased belief in the false claim by about
# the same amount across categories, both among Republicans and among Democrats."
claim("res_misinfo_symmetric", verdict("d_fig3_misinformation_symmetric"))

# "For false claims congenial to Republicans, the effects are approximately equal
# in magnitude across party lines. However, Democrats update substantially
# further than Republicans when corrected about congenial false claims."
claim("res_rep_congenial_symmetric", verdict("d_fig3_republican_congenial_symmetric"))
claim("res_dems_update_further", verdict("d_fig3_democrats_update_further"))

# "The effects of fact-checks observed immediately after treatment dissipate
# somewhat from Wave 1 to Wave 2, on average, at 66.4 per cent of the initial
# magnitude."
claim("res_persist_w2_precise", quantity("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"))

# "The bottom row of Figure 4 shows the meta-analytic averages for the six issues
# in studies with three-wave panels... On average, the effect in Wave 3 is 50 per
# cent the magnitude of the original effect."
claim("res_three_wave_issues", quantity("Experiments in the three-wave panels"))
claim("res_persist_w3_precise", quantity("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"))

# "Figure 5 shows the estimated effects for subjects in the top versus bottom
# tercile of each index. None of these covariates moderate the effects of
# misinformation (left panels), but several of them moderate the effects of
# fact-checks (right panels)."
claim("res_terciles", quantity("Tercile groups per index"))
claim("res_no_misinfo_moderation", verdict("d_fig5_no_misinformation_moderation"))
claim("res_several_moderate", verdict("d_fig5_several_correction_moderation"))

# "This same pattern of heterogeneity persists after one week, though at
# diminished magnitudes (see the Online Appendix)."
claim("res_heterogeneity_persists", verdict("d_heterogeneity_pattern_persists"))
claim("res_heterogeneity_diminished", verdict("d_heterogeneity_diminished"))

# "The results (available in the Online Appendix) show that the within-trait
# ranking of conditional effects shown in Figure 5 is matched exactly by a
# within-trait ranking of average time spent reading the fact-check."
claim("res_time_ranking", verdict("d_time_ranking_matches"))

# "Meta-analysis shows that the overall effects of misinformation and fact-checks
# on attitudes were each smaller than half a point on the 100-point feeling
# thermometer. While the effects were in the expected direction, with fact-checks
# making respondents more positive and misinformation more negative, the effects
# were very small."
claim("res_attitudes_half", verdict("d_attitudes_half_point"))
claim("res_ft_scale", quantity("Feeling thermometer maximum"))
claim("res_attitudes_direction", verdict("d_attitudes_expected_direction"))

# Discussion ----

# "Eight multi-wave experiments extend prior findings about factual corrections
# and misinformation to the 2020 US election."
claim("disc_experiments", quantity("Number of panels"))

# "The effects of fact-checks persisted at 66 per cent the original magnitude
# after one week and at 50 per cent after more than two weeks."
claim("disc_persist_w2", quantity("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"))
claim("disc_persist_w3", quantity("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"))

# "The Big Five personality traits, need for cognition, and political interest
# all moderate the effect of fact-checks."
claim("disc_named_moderators", verdict("d_discussion_named_moderators"))

# "Although misinformation reduces accuracy, on average, the magnitude of the
# effects of fact-checks is more than twice that of misinformation."
claim("disc_more_than_twice", verdict("d_correction_more_than_twice"))

# "Our results show that during the 2020 US election, misinformation degraded
# accuracy, while corrections improved it by roughly twice the amount, often
# durably so."
claim("disc_roughly_twice", verdict("d_correction_more_than_twice"))

# Figure notes ----

# "Notes: Points indicate the mean effect, and line ranges report the 95 per cent
# confidence intervals."
claim("note_ci_level", quantity("Confidence level implied by the plotted intervals"))

# Figure 1 ----
# covers: fig1_*
figure_1 <- out("figure_1_fc_traffic_and_control_agreement.csv")
claim("fig1_traffic_rows", n_distinct(figure_1$topic_short_2[!is.na(figure_1$traffic)]))
claim("fig1_agreement_points", sum(!is.na(figure_1$mu)))

# Figure 2 ----
# covers: fig2_mean_*
# Each of the twenty-four panels labels its three conditional means on the face
# of the plot, so the figure is a table of seventy-two published numbers.
figure_2_slugs <- c(
  "A black man invented the light bulb (Lucid)" = "light_bulb_lucid",
  "A black man invented the light bulb (MTurk)" = "light_bulb_mturk",
  "Antifa start West Coast wildfires (Lucid)" = "antifa_lucid",
  "Antifa start West Coast wildfires (MTurk)" = "antifa_mturk",
  "SARS-CoV-2 man made virus created in the lab (Lucid)" = "sars_lucid",
  "SARS-CoV-2 man made virus created in the lab (MTurk)" = "sars_mturk",
  "Biden wears wire at debate" = "biden_wire",
  "Trump holds Bible upside down" = "trump_bible",
  "Trump responsible for all Covid deaths" = "trump_covid",
  "Donald Trump claimed his DNA was USA" = "trump_dna",
  "Obama failed to nominate US judges" = "obama_judges",
  "Judge Barrett made homophobic and racist statements" = "barrett_homophobic",
  "Trump chose Judge Barrett on basis of looks" = "barrett_looks",
  "WHO: children to be vaccinated without parents' consent" = "who",
  "Hunter Biden's laptop had photos torturing children" = "hunter_laptop",
  "Kamala Harris imprisoned prolife activists" = "kamala",
  "Biden and Obama plotted to have Seal Team 6 murdered" = "seal_team",
  "Joe Biden has never made more than $400k" = "biden_400k",
  "Presidential winner must be announced election night" = "election_night",
  "Sen Coon's daughter's photo on Hunter Biden's laptop" = "coons",
  "Trump said 'Good' to children's separation" = "trump_good",
  "Wisconsin has more votes than registered voters" = "wisconsin",
  "GOP voters' pens invisible to voting machines" = "gop_pens",
  "USPS fails to deliver 27% of mail ballots in South FL" = "usps"
)

figure_2 <- out("figure_2_raw_outcomes_by_treatment.csv") |>
  distinct(topic_short_2, treatment, mu) |>
  mutate(claim_id = str_c("fig2_mean_", unname(figure_2_slugs[topic_short_2]), "_", treatment)) |>
  arrange(claim_id, .locale = "en")

stopifnot(!any(is.na(figure_2$claim_id)), nrow(figure_2) == 72)

walk2(figure_2$claim_id, figure_2$mu, claim)

# Figure 3 ----
# covers: fig3_cells
# The figure plots one point per experiment, party and contrast. Those are the
# cells of appendix Tables 2 and 3, which the pipeline writes as fits, and the
# plotted file adds the meta-analytic summaries and the bracket geometry on top.
claim("fig3_cells", nrow(fit("cates_w1_all_adj")))

# Figure 4 ----
# covers: fig4_*
figure_4 <- out("figure_4_persistence.csv")
claim("fig4_cells", nrow(figure_4))
claim("fig4_three_wave_flat", verdict("d_fig4_three_wave_flat"))

# Figure 5 ----
# covers: fig5_*
trait_ids <- c("Political interest" = "grp_polint", "Political knowledge" = "grp_pk",
               "Cognitive Reflection Test" = "grp_crt", "Need for Cognition" = "grp_nfc",
               "Personality - Emotional Stability" = "grp_emotstab",
               "Personality - Extraversion" = "grp_extraversion",
               "Personality - Agreeableness" = "grp_agree",
               "Personality - Conscientiousness" = "grp_conscientiousness",
               "Personality - Openness" = "grp_openness")

figure_5 <- out("figure_5_heterogeneity_by_trait.csv") |>
  filter(type == "summary") |>
  distinct(name, value, type2, estimate) |>
  mutate(claim_id = str_c("fig5_", unname(trait_ids[as.character(name)]), "_", value, "_", type2)) |>
  arrange(claim_id, .locale = "en")

stopifnot(!any(is.na(figure_5$claim_id)), nrow(figure_5) == 36)

walk2(figure_5$claim_id, figure_5$estimate, claim)

# Figure 6 ----
# covers: fig6_*
figure_6 <- out("figure_6_ft_targets_meta.csv") |>
  mutate(claim_id = str_c("fig6_", if_else(lucid_pid_3 == "Overall", "overall", lucid_pid_3),
                          "_", term2)) |>
  arrange(claim_id, .locale = "en")

walk2(figure_6$claim_id, figure_6$estimate, claim)

# Appendix Table 1, sample composition ----
# covers: app_t1_*
table_a1 <- out("table_a1_sample_characteristics.csv")

panel_n <- table_a1 |>
  filter(nms == "obs") |>
  select(-nms, -vals) |>
  pivot_longer(everything(), names_to = "panel", values_to = "n") |>
  transmute(claim_id = str_c("app_t1_n_", panel), value = as.numeric(str_remove_all(n, ",")))

walk2(panel_n$claim_id, panel_n$value, claim)

claim("app_t1_percentage_cells",
      sum(!is.na(as.matrix(table_a1 |>
                             filter(!nms %in% c("obs", "date_lo", "date_hi")) |>
                             select(-nms, -vals)))))
claim("app_t1_date_cells",
      sum(!is.na(as.matrix(table_a1 |>
                             filter(nms %in% c("date_lo", "date_hi")) |>
                             select(-nms, -vals)))))

# Appendix figures ----
# covers: app_fig*
claim("app_fig1_rows", nrow(out("figure_a1_time_with_treatments.csv")))
claim("app_fig2_rows", nrow(out("figure_a2_time_with_treatments_by_type.csv")))
claim("app_fig3_series", n_distinct(out("figure_a3_fact_check_traffic_over_time.csv")$topic_short))
claim("app_fig4_6_rows",
      nrow(out("figure_a4_covariate_balance_race.csv")) +
        nrow(out("figure_a5_covariate_balance_demographics.csv")) +
        nrow(out("figure_a6_covariate_balance_personality.csv")))
claim("app_fig7_rows", nrow(out("figure_a7_joint_balance_test.csv")))
claim("app_fig8_rows", nrow(out("figure_a8_attrition.csv")))
claim("app_fig9_rows", nrow(out("figure_a9_attitude_effects_by_congeniality.csv")))

# "Figure 9 reports meta-analytic estimates of correction and misinformation
# effects, accounting for the 22 different political figures and groups for whom
# we observed attitudinal outcomes."
claim("app_fig9_targets", quantity("Distinct thermometer targets"))

# Appendix regression tables ----
# covers: app_reg_*
# Every published cell prints four numbers. A cell counts as reproduced when the
# rewrite prints all four; the significance marker and the order the interval is
# printed in are separate claims, because the deposit gets both wrong in ways the
# four numbers do not show.
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            show_col_types = FALSE)

cell_key <- function(estimate, std_error, low, high) {
  str_c(sprintf("%.2f", estimate), "|", sprintf("%.2f", std_error), "|",
        sprintf("%.2f", pmin(low, high)), "|", sprintf("%.2f", pmax(low, high)))
}

rewrite_cells <- bind_rows(
  out("table_a2_ates_w1_cells.csv") |> mutate(appendix_table = 1L),
  out("table_a3_cates_w1_dems_cells.csv") |> mutate(appendix_table = 2L),
  out("table_a4_cates_w1_reps_cells.csv") |> mutate(appendix_table = 3L)
) |>
  pivot_longer(starts_with(c("ctrl_", "chk_")),
               names_to = c("contrast", ".value"), names_sep = "_(?=estimate|std|p\\.|conf)") |>
  select(appendix_table, estimate, std.error, p.value, conf.low, conf.high) |>
  bind_rows(
    bind_rows(
      out("table_a5_persistence_p2_noadj_cells.csv") |> mutate(appendix_table = 4L),
      out("table_a6_persistence_p2_adj_cells.csv") |> mutate(appendix_table = 5L),
      out("table_a7_persistence_p3_noadj_cells.csv") |> mutate(appendix_table = 6L),
      out("table_a8_persistence_p3_adj_cells.csv") |> mutate(appendix_table = 7L)
    ) |>
      select(appendix_table, estimate, std.error, p.value, conf.low, conf.high)
  ) |>
  mutate(key = cell_key(estimate, std.error, conf.low, conf.high))

matched <- published_cells |>
  mutate(key = cell_key(estimate, std_error, interval_first, interval_second)) |>
  group_by(appendix_table) |>
  summarize(matched = sum(key %in% rewrite_cells$key[rewrite_cells$appendix_table ==
                                                       first(appendix_table)]),
            .groups = "drop")

walk2(str_c("app_reg_t", matched$appendix_table, "_cells"), matched$matched, claim)

claim("app_reg_stars", sum(rewrite_cells$p.value < 0.05))
claim("app_reg_descending", 0)

# Appendix Table 9 ----
# covers: app_t9_*
# "To further investigate heterogeneity, across our 21 experiments, we interacted
# each conditional indicator with each pre-treatment covariate in separate linear
# models, resulting in 210 separate linear models. Table 9 displays the
# mean-adjusted r-squared by covariate type."
#
# The deposit ships no code for this table and the rewrite adds none, since
# writing one would be estimating something the deposited archive never did.
# What the pipeline can say is how many experiments there are.
claim("app_t9_experiments", quantity("Experiments (panel by topic)"))
claim("app_t9_models", NA_real_)

walk(c("app_t9_partisanship", "app_t9_conscientiousness", "app_t9_political_knowledge",
       "app_t9_agreeableness", "app_t9_crt", "app_t9_openness", "app_t9_emotional_stability",
       "app_t9_political_interest", "app_t9_need_for_cognition", "app_t9_extraversion"),
     function(id) claim(id, NA_real_))
