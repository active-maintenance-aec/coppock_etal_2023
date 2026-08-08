# coppock_etal_2023/maintained/text_descriptive_claims.R
# Output: output/text_descriptive_claims.csv
# Depends on: helpers.R, fit_all_models.R output, fit_balance_and_attrition.R output,
#   figure_1 and appendix figure 1 output
# Description: A truth value for every claim the article makes about shape, sign,
#   count or ordering rather than about a number. Each row records the claim, the
#   quantity the claim is evaluated against, the threshold the sentence states,
#   and whether the claim holds.
#
#   The published thresholds appear here as comparison targets and nowhere else.
#   "More than twice" cannot be evaluated without the number 2 in the code, and a
#   threshold that decides a verdict about an estimate is not an input to that
#   estimate. No script that fits a model contains a published number.

source(here::here("maintained", "helpers.R"))

figure_1 <- read_csv(file.path(out_dir, "figure_1_fc_traffic_and_control_agreement.csv"),
                     show_col_types = FALSE)
time_spent <- read_csv(file.path(out_dir, "figure_a1_time_with_treatments.csv"),
                       show_col_types = FALSE)
ranking_check <- read_csv(file.path(out_dir, "text_time_ranking_check.csv"),
                          show_col_types = FALSE)

ates_w1_all_noadj <- read_fit("ates_w1_all_noadj")
cates_w1_all_adj <- read_fit("cates_w1_all_adj")
meta_trait_dic <- read_fit("meta_trait_dic_w1_all_adj")
meta_trait_w1 <- read_fit("meta_trait_cates_w1_all_adj")
meta_trait_w2 <- read_fit("meta_trait_cates_w2_p2_adj")
ft_targets_meta <- read_fit("ft_targets_meta_w1_all_adj")
ate_meta_adj <- read_fit("ates_w1_all_adj") |>
  mutate(contrast = term) |>
  group_by(contrast) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything()))))
balance <- read_fit("balance_estimates")
attrition <- read_fit("attrition_estimates")

# Control-group partisan gap, one row per experiment ----
# A mean is higher or lower in every sample, so "more likely to believe" is read
# as a difference the data can distinguish from zero rather than as an ordering
# of two point estimates. Both readings are recorded: the article's "all" holds
# under the point-estimate reading, and its "only two" holds only under the
# significance reading.
control_gap <- figure_1 |>
  filter(!is.na(mu)) |>
  select(congeniality, topic_short_2, lucid_pid_3, mu, se) |>
  pivot_wider(names_from = lucid_pid_3, values_from = c(mu, se)) |>
  mutate(
    difference = mu_Republican - mu_Democrat,
    se_difference = sqrt(se_Republican^2 + se_Democrat^2),
    republicans_higher = difference > 0,
    republicans_higher_significantly = difference / se_difference > 1.96,
    democrats_higher_significantly = difference / se_difference < -1.96,
    republican_congenial = str_detect(congeniality, "Republicans")
  )

# Effects by party at Wave 1, one row per experiment ----
party_effects <- cates_w1_all_adj |>
  left_join(read_rds(file.path(data_dir, "treatments_df.rds")) |>
              select(panel, fc, congeniality),
            by = c("panel", "fc")) |>
  filter(lucid_pid_3 %in% c("Democrat", "Republican"))

# tidy() on an rma object returns its own term column, which would overwrite the
# grouping column of the same name and leave every row labelled "overall", so the
# contrast is carried through under a different name.
party_pool <- party_effects |>
  mutate(contrast = term) |>
  group_by(contrast, lucid_pid_3, congeniality) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything()))))

pool_value <- function(contrast_name, party, congeniality_pattern) {
  value <- party_pool$estimate[party_pool$contrast == contrast_name &
                                 party_pool$lucid_pid_3 == party &
                                 str_detect(party_pool$congeniality, congeniality_pattern)]
  stopifnot(length(value) == 1)
  value
}

# Trait moderation ----
# A covariate moderates an effect when the high and low terciles differ, which is
# the meta-regression on the tercile indicator, not the two conditional effects.
moderation <- meta_trait_dic |>
  transmute(name, contrast = type2, moderates = p.value < 0.05)

personality_and_named <- c("grp_openness", "grp_conscientiousness", "grp_extraversion",
                           "grp_agree", "grp_emotstab", "grp_nfc", "grp_polint")

trait_gap <- function(meta) {
  meta |>
    select(name, value, type2, estimate) |>
    pivot_wider(names_from = value, values_from = estimate) |>
    mutate(gap = abs(high - low))
}

gap_w1 <- trait_gap(meta_trait_w1) |> filter(type2 == "treatmentfactcheck")
gap_w2 <- trait_gap(meta_trait_w2) |> filter(type2 == "treatmentfactcheck")

# "The same pattern at diminished magnitudes" is two claims: the tercile that
# responds more strongly is the same one a week later, and every conditional
# effect is smaller. The first is about ordering, the second about magnitude, so
# they are counted separately over the nine traits and eighteen tercile cells.
persistence_pattern <- inner_join(
  gap_w1 |> transmute(name, stronger_w1 = if_else(high < low, "high", "low")),
  gap_w2 |> transmute(name, stronger_w2 = if_else(high < low, "high", "low")),
  by = "name"
)

magnitudes <- inner_join(
  meta_trait_w1 |> filter(type2 == "treatmentfactcheck") |>
    select(name, value, estimate_w1 = estimate),
  meta_trait_w2 |> filter(type2 == "treatmentfactcheck") |>
    select(name, value, estimate_w2 = estimate),
  by = c("name", "value")
)

three_wave_flat <- read_csv(file.path(out_dir, "figure_4_persistence.csv"),
                            show_col_types = FALSE) |>
  filter(issues == "Issues across three waves", lucid_pid_3 == "All Subjects", wave %in% 1:2) |>
  select(congeniality, wave, estimate) |>
  pivot_wider(names_from = wave, values_from = estimate,
              names_prefix = "estimate_w")

misinformation_meta <- ate_meta_adj$estimate[ate_meta_adj$contrast == "treatmentcontrol"]
correction_meta <- ate_meta_adj$estimate[ate_meta_adj$contrast == "treatmentfactcheck"]

thermometer_misinformation <- ft_targets_meta$estimate[ft_targets_meta$term == "treatmentcontrol"]
thermometer_correction <- ft_targets_meta$estimate[ft_targets_meta$term == "treatmentfactcheck"]

claims <- tribble(
  ~claim_id, ~claim, ~quantity, ~threshold, ~holds,

  "d_fig1_republican_congenial",
  "Republicans are more likely than Democrats to believe all of the Republican-congenial false statements",
  sum(control_gap$republicans_higher[control_gap$republican_congenial]),
  sum(control_gap$republican_congenial),
  sum(control_gap$republicans_higher[control_gap$republican_congenial]) ==
    sum(control_gap$republican_congenial),

  "d_fig1_democratic_congenial",
  "Democrats are more likely than Republicans to believe only two of the Democratic-congenial false statements",
  sum(control_gap$democrats_higher_significantly[!control_gap$republican_congenial]),
  2,
  sum(control_gap$democrats_higher_significantly[!control_gap$republican_congenial]) == 2,

  "d_no_backfire",
  "none backfired, or increased false beliefs",
  sum(ates_w1_all_noadj$term == "treatmentfactcheck" &
        ates_w1_all_noadj$p.value < 0.05 & ates_w1_all_noadj$estimate > 0),
  0,
  sum(ates_w1_all_noadj$term == "treatmentfactcheck" &
        ates_w1_all_noadj$p.value < 0.05 & ates_w1_all_noadj$estimate > 0) == 0,

  # "About the same amount" and "approximately equal" are hedges, so these two
  # rows record the magnitude the sentence is about and return no verdict. A
  # threshold invented here would decide the claim rather than test it.
  "d_fig3_misinformation_symmetric",
  "Being exposed to misinformation increased belief in the false claim by about the same amount across categories, both among Republicans and among Democrats",
  max(abs(pool_value("treatmentcontrol", "Democrat", "Democrats") -
            pool_value("treatmentcontrol", "Democrat", "Republicans")),
      abs(pool_value("treatmentcontrol", "Republican", "Democrats") -
            pool_value("treatmentcontrol", "Republican", "Republicans"))),
  NA,
  NA,

  "d_fig3_republican_congenial_symmetric",
  "For false claims congenial to Republicans, the effects are approximately equal in magnitude across party lines",
  abs(pool_value("treatmentfactcheck", "Democrat", "Republicans") -
        pool_value("treatmentfactcheck", "Republican", "Republicans")),
  NA,
  NA,

  "d_fig3_democrats_update_further",
  "Democrats update substantially further than Republicans when corrected about congenial false claims",
  pool_value("treatmentfactcheck", "Republican", "Democrats") -
    pool_value("treatmentfactcheck", "Democrat", "Democrats"),
  0,
  pool_value("treatmentfactcheck", "Democrat", "Democrats") <
    pool_value("treatmentfactcheck", "Republican", "Democrats"),

  "d_fig5_no_misinformation_moderation",
  "None of these covariates moderate the effects of misinformation",
  sum(moderation$moderates[moderation$contrast == "treatmentcontrol"]),
  0,
  sum(moderation$moderates[moderation$contrast == "treatmentcontrol"]) == 0,

  "d_fig5_several_correction_moderation",
  "Several of them moderate the effects of fact-checks",
  sum(moderation$moderates[moderation$contrast == "treatmentfactcheck"]),
  1,
  sum(moderation$moderates[moderation$contrast == "treatmentfactcheck"]) > 1,

  "d_discussion_named_moderators",
  "The Big Five personality traits, need for cognition, and political interest all moderate the effect of fact-checks",
  sum(moderation$moderates[moderation$contrast == "treatmentfactcheck" &
                             moderation$name %in% personality_and_named]),
  length(personality_and_named),
  all(moderation$moderates[moderation$contrast == "treatmentfactcheck" &
                             moderation$name %in% personality_and_named]),

  "d_heterogeneity_pattern_persists",
  "The same pattern of heterogeneity persists after one week",
  sum(persistence_pattern$stronger_w1 == persistence_pattern$stronger_w2),
  nrow(persistence_pattern),
  all(persistence_pattern$stronger_w1 == persistence_pattern$stronger_w2),

  "d_heterogeneity_diminished",
  "The pattern of heterogeneity after one week is at diminished magnitudes",
  sum(abs(magnitudes$estimate_w2) < abs(magnitudes$estimate_w1)),
  nrow(magnitudes),
  all(abs(magnitudes$estimate_w2) < abs(magnitudes$estimate_w1)),

  "d_time_ranking_matches",
  "The within-trait ranking of conditional effects in Figure 5 is matched exactly by the within-trait ranking of average time spent reading the fact-check",
  sum(ranking_check$agrees),
  nrow(ranking_check),
  all(ranking_check$agrees),

  "d_attitudes_expected_direction",
  "Fact-checks made respondents more positive and misinformation more negative",
  thermometer_correction,
  0,
  thermometer_correction > 0 & thermometer_misinformation < 0,

  "d_attitudes_quarter_point",
  "The effects of fact-checks and misinformation on subsequent attitudes are smaller than one-quarter of a point",
  max(abs(thermometer_misinformation), abs(thermometer_correction)),
  0.25,
  max(abs(thermometer_misinformation), abs(thermometer_correction)) < 0.25,

  "d_attitudes_half_point",
  "The overall effects of misinformation and fact-checks on attitudes were each smaller than half a point",
  max(abs(thermometer_misinformation), abs(thermometer_correction)),
  0.5,
  max(abs(thermometer_misinformation), abs(thermometer_correction)) < 0.5,

  "d_correction_more_than_twice",
  "The magnitude of the effects of fact-checks is more than twice that of misinformation",
  abs(correction_meta) / misinformation_meta,
  2,
  abs(correction_meta) / misinformation_meta > 2,

  "d_balance",
  "These randomizations generated experimental groups balanced on pretreatment covariates",
  sum(p.adjust(balance$p.value, method = "BH") < 0.05),
  0,
  sum(p.adjust(balance$p.value, method = "BH") < 0.05) == 0,

  # Figure 4's bottom-left panel prints its Wave 1 and Wave 2 points at the same
  # height. That is what the deposited estimates give, because the deposit fits
  # both waves on the Wave 3 outcome; the corrected fits separate them.
  "d_fig4_three_wave_flat",
  "In the All Subjects panel for issues across three waves, the Wave 1 and Wave 2 estimates are identical",
  max(abs(three_wave_flat$estimate_w1 - three_wave_flat$estimate_w2)),
  0,
  all(three_wave_flat$estimate_w1 == three_wave_flat$estimate_w2),

  "d_attrition",
  "Our treatments do not appear to change whether subjects respond to outcome questions",
  sum(p.adjust(attrition$p.value, method = "BH") < 0.05),
  0,
  sum(p.adjust(attrition$p.value, method = "BH") < 0.05) == 0
)

claims <- claims |>
  mutate(quantity = as.numeric(quantity), threshold = as.numeric(threshold),
         holds = as.logical(holds))

# A missing verdict is only allowed where the sentence itself hedges, and those
# rows are the ones with no threshold to compare against.
stopifnot(is.na(claims$holds) == is.na(claims$threshold))

write_csv(claims, file.path(out_dir, "text_descriptive_claims.csv"))
print(claims, n = nrow(claims))
