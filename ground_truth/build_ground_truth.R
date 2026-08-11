# coppock_etal_2023/ground_truth/build_ground_truth.R
# Output: ground_truth/coppock_etal_2023_ground_truth.csv, ground_truth/float_coverage.csv
# Depends on: ground_truth/published_claims.csv, ground_truth/published_appendix_values.csv,
#   ground_truth/archive_values.csv, maintained/output/, maintained/in_text_claims.R
# Description: Lay every published number against what the deposit produces and
#   against what the maintained rewrite produces, and refuse to finish if the two
#   instruments disagree or if any published claim is uncovered.
#
#   The only numbers typed anywhere in this repository are the value_paper column
#   of published_claims.csv and the cells of published_appendix_values.csv, both
#   read off the published page. Everything else is computed.

library(tidyverse)
library(here)

here::i_am("ground_truth/build_ground_truth.R")

options(width = 200)

out <- function(file, ...) read_csv(here::here("maintained", "output", file),
                                    show_col_types = FALSE, ...)

# The extraction ----
# value_paper is forced to character. Left to guess, readr reads a column of
# numeric-looking strings as double, which silently rewrites 4.30 as 4.3 and
# -0.19 as -0.19 only by luck, and destroys the precision the comparison needs.
published <- read_csv(
  here::here("ground_truth", "published_claims.csv"),
  col_types = cols(value_paper = col_character(), digits = col_integer(),
                   needs_block = col_logical(), .default = col_character())
)

stopifnot(!any(duplicated(published$claim_id)))

# "The string the article prints" means its digits, not its typography: a
# thousands separator, a Unicode minus and a missing leading zero are all
# typography and none of them survives into the comparison.
normalize_paper <- function(x) {
  x |>
    str_replace_all("−", "-") |>
    str_remove_all(",") |>
    str_replace("^(-?)\\.", "\\10.")
}

# sprintf turns a value that rounds to zero from below into "-0", which fails
# string equality against a published "0".
render <- function(x, digits) {
  rendered <- sprintf(paste0("%.", digits, "f"), x)
  if_else(str_detect(rendered, "^-0(\\.0*)?$"), str_remove(rendered, "^-"), rendered)
}

published <- published |>
  mutate(
    value_paper = normalize_paper(value_paper),
    value_paper_number = as.numeric(value_paper)
  )

# The precision gate. A digits column that is right about the value and wrong
# about the precision passes every numeric comparison downstream, so the stored
# string is re-rendered from its own number and required to come back identical.
misrendered <- published |>
  filter(render(value_paper_number, digits) != value_paper)

if (nrow(misrendered) > 0) {
  print(misrendered |> select(claim_id, value_paper, digits), n = Inf)
  stop("digits disagrees with the string the article prints for ", nrow(misrendered), " claim(s).")
}

# What the maintained rewrite produces ----
main_claims <- out("text_main_claims.csv")
descriptive <- out("text_descriptive_claims.csv")
table_a1 <- out("table_a1_sample_characteristics.csv")
figure_1 <- out("figure_1_fc_traffic_and_control_agreement.csv")
figure_2 <- out("figure_2_raw_outcomes_by_treatment.csv")
figure_4 <- out("figure_4_persistence.csv")
figure_5 <- out("figure_5_heterogeneity_by_trait.csv")
figure_6_meta <- out("figure_6_ft_targets_meta.csv")

claim_value <- function(label) {
  value <- main_claims$value[main_claims$claim == label]
  stopifnot(length(value) == 1)
  value
}

prose_rewrite <- tribble(
  ~claim_id, ~value_rewrite,
  "art_abstract_panels", claim_value("Number of panels"),
  "art_intro_panels", claim_value("Number of panels"),
  "disc_experiments", claim_value("Number of panels"),
  "art_intro_total_n", claim_value("Total respondents across the eight panels"),
  "art_intro_fact_checks", claim_value("Distinct fact checks tested"),
  "art_intro_lag_days", claim_value("Mean days from misinformation posting to fielding"),
  "art_intro_misinfo_effect", claim_value("Meta-analytic misinformation effect, covariate adjusted"),
  "res_meta_misinfo", claim_value("Meta-analytic misinformation effect, covariate adjusted"),
  "res_meta_misinfo_se",
    claim_value("Meta-analytic misinformation effect standard error, covariate adjusted"),
  "art_intro_correction_effect",
    abs(claim_value("Meta-analytic correction effect, covariate adjusted")),
  "res_meta_correction", claim_value("Meta-analytic correction effect, covariate adjusted"),
  "res_meta_correction_se",
    claim_value("Meta-analytic correction effect standard error, covariate adjusted"),
  "art_intro_persist_w2", claim_value("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"),
  "res_persist_w2_precise", claim_value("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"),
  "disc_persist_w2", claim_value("Pooled Wave 2 to Wave 1 ratio, covariate adjusted"),
  "art_intro_persist_w3", claim_value("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"),
  "res_persist_w3_precise", claim_value("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"),
  "disc_persist_w3", claim_value("Pooled Wave 3 to Wave 1 ratio, covariate adjusted"),
  "art_intro_belief_scale", claim_value("Belief certainty scale maximum"),
  "res_scale_high", claim_value("Belief certainty scale maximum"),
  "res_scale_low", claim_value("Belief certainty scale minimum"),
  "art_intro_ft_scale", claim_value("Feeling thermometer maximum"),
  "res_ft_scale", claim_value("Feeling thermometer maximum"),
  "design_two_wave_panels", claim_value("Panels with only two waves"),
  "design_six_of_eight", claim_value("Panels with only two waves"),
  "design_three_wave_panels", claim_value("Panels with a third wave"),
  "design_remaining_two", claim_value("Panels with a third wave"),
  "design_mturk_panels", claim_value("Panels fielded on Mechanical Turk"),
  "design_lucid_panels", claim_value("Panels fielded on Lucid"),
  "design_experiments_per_wave", claim_value("Experiments per panel"),
  "design_conditions", claim_value("Experimental conditions"),
  "design_traffic_rep", claim_value("Mean fact-check page views, Republican-congenial claims"),
  "design_traffic_dem", claim_value("Mean fact-check page views, Democratic-congenial claims"),
  "design_traffic_weeks", claim_value("Maximum weeks of page views observed for a fact check"),
  "res_misinfo_sig_count", claim_value("Fact checks where misinformation significantly reduced accuracy"),
  "res_fc_sig_count", claim_value("Fact checks with a significant negative correction effect"),
  "res_opportunities", claim_value("Experiments (panel by topic)"),
  "res_three_wave_issues", claim_value("Experiments in the three-wave panels"),
  "res_terciles", claim_value("Tercile groups per index"),
  "note_ci_level", claim_value("Confidence level implied by the plotted intervals"),
  "app_fig9_targets", claim_value("Distinct thermometer targets"),
  "app_t9_experiments", claim_value("Experiments (panel by topic)")
)

# Descriptive claims carry a truth value, not a number, and print at zero
# decimals so that FALSE does not render as 0.00 and fail its own check.
descriptive_map <- tribble(
  ~claim_id, ~descriptive_id,
  "art_intro_attitudes_quarter", "d_attitudes_quarter_point",
  "design_balance", "d_balance",
  "design_attrition", "d_attrition",
  "res_fig1_rep_all", "d_fig1_republican_congenial",
  "res_fig1_dem_two", "d_fig1_democratic_congenial",
  "res_no_backfire", "d_no_backfire",
  "res_misinfo_symmetric", "d_fig3_misinformation_symmetric",
  "res_rep_congenial_symmetric", "d_fig3_republican_congenial_symmetric",
  "res_dems_update_further", "d_fig3_democrats_update_further",
  "res_no_misinfo_moderation", "d_fig5_no_misinformation_moderation",
  "res_several_moderate", "d_fig5_several_correction_moderation",
  "res_heterogeneity_persists", "d_heterogeneity_pattern_persists",
  "res_heterogeneity_diminished", "d_heterogeneity_diminished",
  "res_time_ranking", "d_time_ranking_matches",
  "res_attitudes_half", "d_attitudes_half_point",
  "res_attitudes_direction", "d_attitudes_expected_direction",
  "disc_named_moderators", "d_discussion_named_moderators",
  "disc_more_than_twice", "d_correction_more_than_twice",
  "disc_roughly_twice", "d_correction_more_than_twice",
  "fig4_three_wave_flat", "d_fig4_three_wave_flat"
)

descriptive_rewrite <- descriptive_map |>
  left_join(descriptive |> select(descriptive_id = claim_id, holds), by = "descriptive_id") |>
  transmute(claim_id, holds, value_rewrite = as.numeric(holds))

stopifnot(nrow(descriptive_rewrite) == nrow(descriptive_map))

# Figure 2's twenty-four topics carry hand slugs in the extraction, because a
# slug cannot be derived from a topic label without inventing a rule. The map is
# asserted complete in both directions rather than assumed.
figure_2_slugs <- tribble(
  ~topic_short_2, ~slug,
  "A black man invented the light bulb (Lucid)", "light_bulb_lucid",
  "A black man invented the light bulb (MTurk)", "light_bulb_mturk",
  "Antifa start West Coast wildfires (Lucid)", "antifa_lucid",
  "Antifa start West Coast wildfires (MTurk)", "antifa_mturk",
  "SARS-CoV-2 man made virus created in the lab (Lucid)", "sars_lucid",
  "SARS-CoV-2 man made virus created in the lab (MTurk)", "sars_mturk",
  "Biden wears wire at debate", "biden_wire",
  "Trump holds Bible upside down", "trump_bible",
  "Trump responsible for all Covid deaths", "trump_covid",
  "Donald Trump claimed his DNA was USA", "trump_dna",
  "Obama failed to nominate US judges", "obama_judges",
  "Judge Barrett made homophobic and racist statements", "barrett_homophobic",
  "Trump chose Judge Barrett on basis of looks", "barrett_looks",
  "WHO: children to be vaccinated without parents' consent", "who",
  "Hunter Biden's laptop had photos torturing children", "hunter_laptop",
  "Kamala Harris imprisoned prolife activists", "kamala",
  "Biden and Obama plotted to have Seal Team 6 murdered", "seal_team",
  "Joe Biden has never made more than $400k", "biden_400k",
  "Presidential winner must be announced election night", "election_night",
  "Sen Coon's daughter's photo on Hunter Biden's laptop", "coons",
  "Trump said 'Good' to children's separation", "trump_good",
  "Wisconsin has more votes than registered voters", "wisconsin",
  "GOP voters' pens invisible to voting machines", "gop_pens",
  "USPS fails to deliver 27% of mail ballots in South FL", "usps"
)

stopifnot(setequal(figure_2_slugs$topic_short_2, unique(figure_2$topic_short_2)))

figure_2_rewrite <- figure_2 |>
  distinct(topic_short_2, treatment, mu) |>
  inner_join(figure_2_slugs, by = "topic_short_2") |>
  transmute(claim_id = str_c("fig2_mean_", slug, "_", treatment), value_rewrite = mu)

figure_5_rewrite <- figure_5 |>
  filter(type == "summary") |>
  distinct(name, value, type2, estimate) |>
  left_join(
    tibble(label = c("Political interest", "Political knowledge", "Cognitive Reflection Test",
                     "Need for Cognition", "Personality - Emotional Stability",
                     "Personality - Extraversion", "Personality - Agreeableness",
                     "Personality - Conscientiousness", "Personality - Openness"),
           trait = c("grp_polint", "grp_pk", "grp_crt", "grp_nfc", "grp_emotstab",
                     "grp_extraversion", "grp_agree", "grp_conscientiousness", "grp_openness")),
    by = c("name" = "label")
  ) |>
  transmute(claim_id = str_c("fig5_", trait, "_", value, "_", type2), value_rewrite = estimate)

figure_6_rewrite <- figure_6_meta |>
  transmute(claim_id = str_c("fig6_", if_else(lucid_pid_3 == "Overall", "overall", lucid_pid_3),
                             "_", term2),
            value_rewrite = estimate)

panel_n_rewrite <- table_a1 |>
  filter(nms == "obs") |>
  select(-nms, -vals) |>
  pivot_longer(everything(), names_to = "panel", values_to = "n") |>
  transmute(claim_id = str_c("app_t1_n_", panel),
            value_rewrite = as.numeric(str_remove_all(n, ",")))

percentage_cells <- table_a1 |>
  filter(!nms %in% c("obs", "date_lo", "date_hi")) |>
  select(-nms, -vals)

date_cells <- table_a1 |>
  filter(nms %in% c("date_lo", "date_hi")) |>
  select(-nms, -vals)

float_rewrite <- tribble(
  ~claim_id, ~value_rewrite,
  "art_pages", NA_real_,
  "app_t1_percentage_cells", sum(!is.na(as.matrix(percentage_cells))),
  "app_t1_date_cells", sum(!is.na(as.matrix(date_cells))),
  "fig1_traffic_rows", n_distinct(figure_1$topic_short_2[!is.na(figure_1$traffic)]),
  "fig1_agreement_points", sum(!is.na(figure_1$mu)),
  "fig3_cells", nrow(read_csv(here::here("maintained", "output", "fits", "cates_w1_all_adj.csv"),
                              show_col_types = FALSE)),
  "fig4_cells", nrow(figure_4),
  "app_fig1_rows", nrow(out("figure_a1_time_with_treatments.csv")),
  "app_fig2_rows", nrow(out("figure_a2_time_with_treatments_by_type.csv")),
  "app_fig3_series", n_distinct(out("figure_a3_fact_check_traffic_over_time.csv")$topic_short),
  "app_fig4_6_rows",
    nrow(out("figure_a4_covariate_balance_race.csv")) +
      nrow(out("figure_a5_covariate_balance_demographics.csv")) +
      nrow(out("figure_a6_covariate_balance_personality.csv")),
  "app_fig7_rows", nrow(out("figure_a7_joint_balance_test.csv")),
  "app_fig8_rows", nrow(out("figure_a8_attrition.csv")),
  "app_fig9_rows", nrow(out("figure_a9_attitude_effects_by_congeniality.csv"))
)

# Appendix regression tables ----
# Every published cell prints an estimate, a standard error, a significance
# marker and an interval, so a cell counts as reproduced when the rewrite prints
# the same four numbers. The marker and the order the interval is printed in are
# separate claims of their own, two rows below, because the deposit gets both
# wrong in ways the numbers do not show.
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            show_col_types = FALSE) |>
  mutate(significance_marker = replace_na(significance_marker, ""))

cell_key <- function(estimate, std_error, low, high) {
  str_c(render(estimate, 2), "|", render(std_error, 2), "|",
        render(pmin(low, high), 2), "|", render(pmax(low, high), 2))
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

published_cells <- published_cells |>
  mutate(key = cell_key(estimate, std_error, interval_first, interval_second))

cells_rewrite <- published_cells |>
  group_by(appendix_table) |>
  summarize(matched = sum(key %in% rewrite_cells$key[rewrite_cells$appendix_table ==
                                                       first(appendix_table)]),
            .groups = "drop") |>
  transmute(claim_id = str_c("app_reg_t", appendix_table, "_cells"), value_rewrite = matched)

table_rewrite <- bind_rows(
  cells_rewrite,
  tibble(claim_id = "app_reg_stars", value_rewrite = sum(rewrite_cells$p.value < 0.05)),
  tibble(claim_id = "app_reg_descending", value_rewrite = 0)
)

rewrite <- bind_rows(
  prose_rewrite,
  descriptive_rewrite |> select(claim_id, value_rewrite),
  figure_2_rewrite,
  figure_5_rewrite,
  figure_6_rewrite,
  panel_n_rewrite,
  float_rewrite,
  table_rewrite
) |>
  filter(!is.na(value_rewrite))

stopifnot(!any(duplicated(rewrite$claim_id)))

# A rewrite value keyed to a claim the article does not make is a mistyped label,
# and it would otherwise vanish silently into a join that finds nothing.
orphan_rewrite <- setdiff(rewrite$claim_id, published$claim_id)
if (length(orphan_rewrite) > 0) {
  print(orphan_rewrite)
  stop("The rewrite produces values for ", length(orphan_rewrite),
       " claim id(s) the extraction does not list.")
}

archive <- read_csv(here::here("ground_truth", "archive_values.csv"), show_col_types = FALSE)

orphan_archive <- setdiff(archive$claim_id, published$claim_id)
if (length(orphan_archive) > 0) {
  print(orphan_archive)
  stop("The deposit produces values for ", length(orphan_archive),
       " claim id(s) the extraction does not list.")
}

# Comparison ----
gt <- published |>
  left_join(rewrite, by = "claim_id") |>
  left_join(archive, by = "claim_id") |>
  left_join(descriptive_rewrite |> select(claim_id, holds), by = "claim_id") |>
  mutate(
    paper_id = "coppock_etal_2023",
    table_figure = location,
    rendered_paper = render(value_paper_number, digits),
    rendered_rewrite = if_else(is.na(value_rewrite), NA_character_,
                               render(value_rewrite, digits)),
    rendered_script = if_else(is.na(value_script), NA_character_,
                              render(value_script, digits)),
    match = if_else(is.na(rendered_script), NA_real_,
                    as.numeric(rendered_script == rendered_paper)),
    match_rewrite = case_when(
      claim_type == "descriptive" ~ NA_real_,
      is.na(rendered_rewrite) ~ NA_real_,
      TRUE ~ as.numeric(rendered_rewrite == rendered_paper)
    )
  )

# A descriptive claim's verdict lives in holds, and a hedged one has no verdict
# at all, so a missing holds on a descriptive row is only legal where the
# descriptive script also returned none.
stopifnot(all(gt$claim_id[gt$claim_type == "descriptive"] %in% descriptive_map$claim_id))

adverse <- function(match, match_rewrite, holds) {
  (!is.na(match) & match == 0) |
    (!is.na(match_rewrite) & match_rewrite == 0) |
    (!is.na(holds) & !holds)
}

clean_match <- function(match, match_rewrite, holds) {
  !adverse(match, match_rewrite, holds) &
    ((!is.na(match) & match == 1) | (!is.na(match_rewrite) & match_rewrite == 1) |
       (!is.na(holds) & holds))
}

# defect_locus ----
# Set per claim, because a zero otherwise reads as a failure of the rewrite and
# here it almost never is. The five values are paper_internal, archive,
# environment, rewrite and unresolved.
locus <- tribble(
  ~claim_id, ~defect_locus, ~locus_note,
  "art_intro_total_n", "paper_internal",
    "The deposited data hold 17,629 respondents and the appendix's own Table 1 sums to 17,629.",
  "art_intro_persist_w2", "paper_internal",
    "The article states this quantity twice at different values. Appendix Table 5's Meta-analysis ratio row prints 0.49, the random-effects pool of the same per-experiment ratios, where the main text says 66 per cent. The published figure sits in the fixed-effect family, whose pool is 65 per cent adjusted and 71 per cent unadjusted, but no expression in the deposit or the rewrite returns 66 exactly.",
  "res_persist_w2_precise", "paper_internal",
    "As art_intro_persist_w2, at the sharper 66.4. The pooled Wave 2 effect is 47 per cent of the pooled Wave 1 effect, and the fixed-effect pool of the ratios is 64.9 per cent adjusted.",
  "disc_persist_w2", "paper_internal", "As art_intro_persist_w2.",
  "art_intro_persist_w3", "paper_internal",
    "The article states this quantity twice at different values. Appendix Table 7's Meta-analysis ratio row prints 0.43 where the main text says 50 per cent. That row is itself fitted on the wrong outcome (see fig4_three_wave_flat); refitted on Wave 3 the random-effects pool is 34 per cent adjusted and 39 per cent unadjusted.",
  "res_persist_w3_precise", "paper_internal", "As art_intro_persist_w3.",
  "disc_persist_w3", "paper_internal", "As art_intro_persist_w3.",
  "res_misinfo_sig_count", "paper_internal",
    "Thirteen of the twenty-four unadjusted misinformation contrasts are positive and significant, in the deposit and in the rewrite alike; appendix Table 1 prints all thirteen.",
  "design_traffic_weeks", "unresolved",
    "The deposited traffic series runs from four to thirteen weeks per fact check rather than eight.",
  "app_fig9_targets", "archive",
    "The deposited data carry fourteen feeling thermometer columns, not twenty-two.",
  "app_t9_experiments", "unresolved",
    "The deposit ships no code for appendix Table 9, and the article elsewhere describes twenty-four experiments.",
  "app_t9_models", "archive",
    "The deposit ships no code for appendix Table 9, so neither instrument computes it.",
  "app_t9_partisanship", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_conscientiousness", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_political_knowledge", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_agreeableness", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_crt", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_openness", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_emotional_stability", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_political_interest", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_need_for_cognition", "archive", "No code for appendix Table 9 in the deposit.",
  "app_t9_extraversion", "archive", "No code for appendix Table 9 in the deposit.",
  "app_reg_stars", "archive",
    "The deposit's table script reshapes each fit with reshape2::melt, whose id.vars omit a character column, so every value becomes a string and the surviving significance test compares strings. Every p-value R prints in scientific notation loses its star.",
  "app_reg_descending", "archive",
    "The deposit negates the misinformation contrast without reversing its interval, so those cells print their bounds high then low.",
  "fig5_grp_pk_low_treatmentcontrol", "environment",
    "The two estimatr versions disagree about which HC2 standard errors are estimable for the political knowledge terciles, which changes which studies the pooling drops.",
  "fig5_grp_pk_high_treatmentcontrol", "environment", "As fig5_grp_pk_low_treatmentcontrol.",
  "fig5_grp_pk_low_treatmentfactcheck", "environment", "As fig5_grp_pk_low_treatmentcontrol.",
  "fig5_grp_pk_high_treatmentfactcheck", "environment", "As fig5_grp_pk_low_treatmentcontrol.",
  "fig6_overall_treatmentcontrol", "archive",
    "The deposit's thermometer loop filters with filter(data, fc == fc, topic_short_2 == topic_short_2), where each condition compares a column with itself and subsets nothing, so every target model is fitted on all eight panels pooled. The rewrite applies the subset the loop was written to apply.",
  "fig6_overall_treatmentfactcheck", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Democrat_treatmentcontrol", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Democrat_treatmentfactcheck", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Independent_treatmentcontrol", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Independent_treatmentfactcheck", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Republican_treatmentcontrol", "archive", "As fig6_overall_treatmentcontrol.",
  "fig6_Republican_treatmentfactcheck", "archive", "As fig6_overall_treatmentcontrol.",
  "art_intro_attitudes_quarter", "archive", "As fig6_overall_treatmentcontrol.",
  "res_attitudes_half", "archive", "As fig6_overall_treatmentcontrol.",
  "fig4_three_wave_flat", "archive",
    "The deposit's adjusted three-wave block regresses outcome_w3 for both its Wave 1 and its Wave 2 models and outcome_w2 for its Wave 3 model, so the published panel plots one estimate twice.",
  "app_reg_t7_cells", "archive", "As fig4_three_wave_flat.",
  "app_reg_t6_cells", "unresolved", "Investigated with the other persistence tables.",
  "app_reg_t1_cells", "unresolved", "Investigated with the other regression tables.",
  "app_reg_t2_cells", "unresolved", "Investigated with the other regression tables.",
  "app_reg_t3_cells", "unresolved", "Investigated with the other regression tables.",
  "app_reg_t4_cells", "unresolved", "Investigated with the other regression tables.",
  "app_reg_t5_cells", "unresolved", "Investigated with the other regression tables."
)

gt <- gt |>
  left_join(locus, by = "claim_id") |>
  mutate(
    is_adverse = adverse(match, match_rewrite, holds),
    is_clean = clean_match(match, match_rewrite, holds),
    defect_locus = if_else(is_clean, NA_character_, defect_locus),
    notes = case_when(
      !is.na(locus_note) & !is_clean ~ locus_note,
      claim_type == "transcribed" ~ "Copied from the cited paper; checked once against the source.",
      is.na(value_rewrite) & is.na(value_script) ~ "No counterpart in the deposit or the rewrite.",
      TRUE ~ str_c("Published ", value_paper, "; rewrite ",
                   coalesce(rendered_rewrite, "not computed"), "; deposit ",
                   coalesce(rendered_script, "not computed"), ".")
    )
  )

# THE LOCUS RULE, three states. An adverse row must carry a locus, a clean match
# must not, and a row with no verdict may.
missing_locus <- gt |> filter(is_adverse, is.na(defect_locus))
if (nrow(missing_locus) > 0) {
  print(missing_locus |> select(claim_id, value_paper, rendered_rewrite, rendered_script, holds), n = Inf)
  stop(nrow(missing_locus), " adverse row(s) carry no defect_locus.")
}

spurious_locus <- gt |> filter(is_clean, !is.na(defect_locus))
if (nrow(spurious_locus) > 0) {
  print(spurious_locus |> select(claim_id, defect_locus), n = Inf)
  stop(nrow(spurious_locus), " clean match(es) carry a defect_locus.")
}

# Coverage ----
# Every descriptive claim is covered by construction: the assertion above
# requires each to appear in the descriptive script, which computes a quantity
# for all of them and a verdict for those the article does not hedge.
uncovered <- gt |>
  filter(claim_type == "pipeline", is.na(value_rewrite), is.na(value_script),
         !str_starts(claim_id, "app_t9_"))

if (nrow(uncovered) > 0) {
  print(uncovered |> select(claim_id, location), n = Inf)
  stop("A pipeline claim has no counterpart on either side.")
}

# The second instrument ----
# Sourced into its own environment: both files read the same outputs and name
# objects for what they hold, so a bare source() would replace this script's own
# published, out() and friends with the claims file's.
claims_output <- capture.output(
  source(here::here("maintained", "in_text_claims.R"), local = new.env())
)

printed <- str_match(claims_output, "^CLAIM ([A-Za-z0-9_]+) = (\\S+) \\|\\| (.*)$")
printed <- tibble(
  claim_id = printed[, 2],
  printed_value = printed[, 3]
) |>
  filter(!is.na(claim_id))

declared <- gt$claim_id[gt$needs_block]

if (nrow(printed) != length(declared)) {
  print(tibble(printed = nrow(printed), declared = length(declared)))
  print(setdiff(declared, printed$claim_id))
  print(setdiff(printed$claim_id, declared))
  stop("in_text_claims.R printed ", nrow(printed), " claims against ",
       length(declared), " declared.")
}

if (!setequal(printed$claim_id, declared)) {
  print(setdiff(declared, printed$claim_id))
  print(setdiff(printed$claim_id, declared))
  stop("The claims file and the extraction disagree about which claims have blocks.")
}

# The two instruments reach the same number by separate paths from the same
# outputs. Where they disagree, one of them is wrong.
cross <- gt |>
  select(claim_id, rendered_rewrite) |>
  inner_join(printed, by = "claim_id") |>
  filter(printed_value != "NA", !is.na(rendered_rewrite)) |>
  filter(printed_value != rendered_rewrite)

if (nrow(cross) > 0) {
  print(cross, n = Inf)
  stop(nrow(cross), " claim(s) differ between the ground truth and in_text_claims.R.")
}

# Float coverage ----
float_of <- function(claim_id, location) {
  case_when(
    str_starts(claim_id, "fig1_") ~ "Figure 1",
    str_starts(claim_id, "fig2_") ~ "Figure 2",
    str_starts(claim_id, "fig3_") ~ "Figure 3",
    str_starts(claim_id, "fig4_") ~ "Figure 4",
    str_starts(claim_id, "fig5_") ~ "Figure 5",
    str_starts(claim_id, "fig6_") ~ "Figure 6",
    str_starts(claim_id, "app_t1_") ~ "Appendix Table 1 (sample composition)",
    str_starts(claim_id, "app_fig") ~ str_extract(location, "Appendix Figures? [0-9 to]+"),
    str_starts(claim_id, "app_reg_t") ~ str_c("Appendix regression table ",
                                              str_extract(claim_id, "(?<=app_reg_t)\\d")),
    str_starts(claim_id, "app_t8_") ~ "Appendix Table 8",
    str_starts(claim_id, "app_t9_") ~ "Appendix Table 9",
    TRUE ~ NA_character_
  )
}

# printed_numbers counts what a float puts on its own face and is the
# denominator of the coverage fraction. plotted_quantities counts what it draws,
# which is the only checkable quantity a wordless figure states, and is verified
# by a count rather than value by value.
float_inventory <- tribble(
  ~float, ~printed_numbers, ~plotted_quantities, ~note,
  "Figure 1", 0, 72, "Twenty-four traffic bars and forty-eight control-group agreement points, none labelled.",
  "Figure 2", 72, 72, "Twenty-four topics by three conditions, each labelled with its conditional mean.",
  "Figure 3", 68, 104, "A label is drawn on every effect significantly different from zero and on each of the eight meta-analytic summaries. The labels were not transcribed and this is the largest coverage gap in the repository; the estimates behind every plotted point are the covariate-adjusted cells of appendix Tables 2 and 3, all of which reproduce.",
  "Figure 4", 0, 30, "Thirty meta-analytic estimates, none labelled.",
  "Figure 5", 36, 36, "Nine traits by two terciles by two contrasts, each labelled.",
  "Figure 6", 8, 8, "Four partisan groups by two contrasts, each labelled inside its diamond.",
  "Appendix Table 1 (sample composition)", 206, 206, "Eight panel sizes, one hundred and eighty-eight percentages and sixteen dates. The percentages and dates are covered as counts of non-empty cells rather than cell by cell.",
  "Appendix Figure 1", 0, 36, "Nine traits by two terciles by two treatments, none labelled.",
  "Appendix Figure 2", 0, 72, "As Appendix Figure 1, split by misinformation type.",
  "Appendix Figure 3", 0, 21, "One traffic series per fact check.",
  "Appendix Figures 4 to 6", 0, 816, "Balance estimates, none labelled.",
  "Appendix Figure 7", 0, 48, "Raw and adjusted joint balance p-values, none labelled.",
  "Appendix Figure 8", 0, 108, "Attrition estimates, none labelled.",
  "Appendix Figure 9", 12, 12, "Meta-analytic attitude estimates labelled by congeniality and party.",
  "Appendix regression table 1", 400, 400, "One hundred cells, each printing an estimate, a standard error and two interval bounds.",
  "Appendix regression table 2", 400, 400, "As appendix regression table 1.",
  "Appendix regression table 3", 400, 400, "As appendix regression table 1.",
  "Appendix regression table 4", 300, 300, "Seventy-five cells.",
  "Appendix regression table 5", 300, 300, "Seventy-five cells.",
  "Appendix regression table 6", 84, 84, "Twenty-one cells.",
  "Appendix regression table 7", 84, 84, "Twenty-one cells.",
  "Appendix Table 8", 72, 72, "Hand-entered CrowdTangle counts of the underlying misinformation items. Neither the deposit nor the rewrite computes them, and no deposited file carries them.",
  "Appendix Table 9", 10, 10, "Mean adjusted r-squared by covariate. The deposit ships no code for this table, so the rewrite adds none."
)

covered <- gt |>
  mutate(float = float_of(claim_id, location)) |>
  filter(!is.na(float)) |>
  group_by(float) |>
  summarize(
    rows = n(),
    numbers_covered = sum(case_when(
      str_starts(claim_id, "app_reg_t") & str_ends(claim_id, "_cells") ~
        coalesce(value_rewrite, 0) * 4,
      claim_type == "structural" ~ 0,
      is.na(value_rewrite) & is.na(value_script) ~ 0,
      TRUE ~ 1
    )),
    counts_verified = sum(claim_type == "structural" &
                            !is.na(match_rewrite) & match_rewrite == 1),
    .groups = "drop"
  )

float_coverage <- float_inventory |>
  left_join(covered, by = "float") |>
  mutate(
    rows = replace_na(rows, 0L),
    numbers_covered = replace_na(numbers_covered, 0),
    counts_verified = replace_na(counts_verified, 0L),
    covered_fraction = if_else(printed_numbers == 0, NA_real_,
                               round(numbers_covered / printed_numbers, 3))
  ) |>
  select(float, printed_numbers, numbers_covered, covered_fraction, plotted_quantities,
         counts_verified, rows, note) |>
  arrange(float, .locale = "en")

write_csv(float_coverage, here::here("ground_truth", "float_coverage.csv"))

no_coverage <- float_coverage |> filter(rows == 0)
if (nrow(no_coverage) > 0) {
  print(no_coverage |> select(float, note), n = Inf)
  stop("A published float has no ground-truth row.")
}

# The committed file ----
# value_paper is written back as the string it was rendered from, so the CSV
# stores what the article prints rather than what a numeric parse would give.
ground_truth <- gt |>
  transmute(
    paper_id,
    claim_id,
    table_figure,
    claim_type,
    claim,
    value_script = rendered_script,
    value_paper = rendered_paper,
    match,
    value_rewrite = rendered_rewrite,
    match_rewrite,
    holds,
    defect_locus,
    notes
  ) |>
  arrange(claim_id, .locale = "en")

stopifnot(identical(ground_truth$value_paper, published$value_paper[
  match(ground_truth$claim_id, published$claim_id)]))

write_csv(ground_truth, here::here("ground_truth", "coppock_etal_2023_ground_truth.csv"))

# The errata spine's claim_ids ----
# errata_entries.csv names, for every published entry, the ground-truth claims it corrects.
# Every one of those ids has to exist here: a missing one is a typo or a claim that has since
# been renamed, and a dangling reference inside a document whose whole purpose is correcting
# the record is worse than a failed build.
errata_spine <- here::here("errata_entries.csv")
if (file.exists(errata_spine)) {
  cited_ids <- read_csv(errata_spine, show_col_types = FALSE)$claim_ids |>
    str_split(";") |>
    unlist() |>
    str_trim() |>
    discard(\(x) is.na(x) | x == "")
  dangling <- setdiff(cited_ids, ground_truth$claim_id)
  if (length(dangling) > 0) print(dangling)
  stopifnot(length(dangling) == 0)
}

print(ground_truth |> count(claim_type, match_rewrite))
print(ground_truth |> filter(!is.na(defect_locus)) |> count(defect_locus))
print(float_coverage |> select(float, printed_numbers, numbers_covered, covered_fraction,
                              plotted_quantities, counts_verified), n = Inf)
