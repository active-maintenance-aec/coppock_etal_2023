# coppock_etal_2023/maintained/text_main_claims.R
# Output: output/text_main_claims.csv
# Depends on: helpers.R, fit_all_models.R output, original/data/all_panels_long.rds,
#   original/data/treatments_df.rds
# Description: Every quantity the main text states in prose rather than in a
#   figure or a table: sample sizes, fact-check traffic, the pooled treatment
#   effects, how much of the correction effect survives to later waves, and how
#   many contrasts reach significance.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))

respondents <- all_panels_long |> filter(fc == 1)

panel_sizes <- respondents |> count(panel, name = "n_respondents")

# Fact checks, not panel-by-topic cells. The three topics carried by both arms
# of Panel 1 appear twice in treatments_df, distinguished in topic_short_2 by a
# platform suffix, so there are 24 experiments run on 21 distinct fact checks.
# Anything the paper says about a fact check (its traffic, how long after the
# misinformation appeared it was fielded) is a statement about the 21, and
# averaging over the 24 double-counts the three shared topics.
fact_checks <- treatments_df |>
  distinct(topic_short, congeniality, traffic, date_posted, date_fielded)

traffic <- fact_checks |>
  group_by(congeniality) |>
  summarize(mean_traffic = mean(traffic), .groups = "drop")

lag_days <- fact_checks |>
  summarize(mean_days = mean(as.numeric(difftime(date_fielded, date_posted, units = "days")),
                             na.rm = TRUE)) |>
  pull(mean_days)

# Pooled Wave 1 effects ----
# tidy() on an rma object returns a column called term of its own, so the
# contrast is carried through the pooling under a different name.
pool_all <- function(name) {
  read_fit(name) |>
    mutate(contrast = term) |>
    group_by(contrast) |>
    reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
                 conf.int = TRUE)) |>
    select(-any_of(c("term", "type"))) |>
    rename(term = contrast)
}

ate_meta_adj <- pool_all("ates_w1_all_adj")
ate_meta_noadj <- pool_all("ates_w1_all_noadj")

pick_meta <- function(meta, term_name, column) {
  meta[[column]][meta$term == term_name]
}

# How much of the Wave 1 effect survives ----
# The ratio compares the pooled correction effect at a later wave to the pooled
# effect at Wave 1 within the same set of panels and the same set of respondents.
# The persistence models drop the misinformation-only arm, so treatmentfactcheck
# is the only contrast these fits carry; naming it keeps the ratio a statement
# about the correction rather than about whichever term sorts first.
survival <- function(later, first) {
  100 *
    pick_meta(pool_all(later), "treatmentfactcheck", "estimate") /
    pick_meta(pool_all(first), "treatmentfactcheck", "estimate")
}

# The pre-analysis plan's own persistence quantity is a different thing: the
# per-experiment ratio of the later-wave coefficient to the Wave 1 coefficient,
# which is what an instrumental-variables regression of the later outcome on the
# Wave 1 outcome, instrumented by assignment, returns. The appendix prints its
# pooled value in the "ratio" row of each persistence table. Both derivations are
# reported here because the main text's persistence percentages match neither.
ratio_meta <- function(name) {
  d <- read_fit(name)
  100 * as.numeric(coef(rma.uni(yi = d$estimate, sei = d$std.error)))
}

ates_w1_all_adj <- read_fit("ates_w1_all_adj")
ates_w1_all_noadj <- read_fit("ates_w1_all_noadj")

ft_targets_meta <- read_fit("ft_targets_meta_w1_all_adj")

cume_fc_views <- read_rds(file.path(data_dir, "cume_fc_views.rds"))

three_wave <- all_panels_long |> filter(!is.na(outcome_w3))

thermometer_max <- all_panels_long |>
  select(starts_with("w1_ft")) |>
  as.matrix() |>
  max(na.rm = TRUE)

# The confidence level is a property of every interval the pipeline draws, so it
# is read back out of one rather than asserted: an interval half width of z
# standard errors is a level of 2 * pnorm(z) - 1.
confidence_level <- ates_w1_all_adj |>
  slice(1) |>
  summarize(level = 100 * (2 * pnorm((conf.high - estimate) / std.error) - 1)) |>
  pull(level)

claims <- tribble(
  ~claim, ~value,
  "Total respondents across the eight panels", nrow(respondents),
  "Distinct respondent identifiers", n_distinct(respondents$admin_id),
  "Number of panels", n_distinct(respondents$panel),
  "Experiments (panel by topic)", nrow(treatments_df),
  "Distinct fact checks tested", n_distinct(treatments_df$topic_short),
  "Mean days from misinformation posting to fielding", lag_days,
  "Mean fact-check page views, Republican-congenial claims",
    traffic$mean_traffic[traffic$congeniality == "False claim congenial to Republicans"],
  "Mean fact-check page views, Democratic-congenial claims",
    traffic$mean_traffic[traffic$congeniality == "False claim congenial to Democrats"],
  "Meta-analytic misinformation effect, covariate adjusted",
    pick_meta(ate_meta_adj, "treatmentcontrol", "estimate"),
  "Meta-analytic misinformation effect standard error, covariate adjusted",
    pick_meta(ate_meta_adj, "treatmentcontrol", "std.error"),
  "Meta-analytic correction effect, covariate adjusted",
    pick_meta(ate_meta_adj, "treatmentfactcheck", "estimate"),
  "Meta-analytic correction effect standard error, covariate adjusted",
    pick_meta(ate_meta_adj, "treatmentfactcheck", "std.error"),
  "Meta-analytic misinformation effect, unadjusted",
    pick_meta(ate_meta_noadj, "treatmentcontrol", "estimate"),
  "Meta-analytic correction effect, unadjusted",
    pick_meta(ate_meta_noadj, "treatmentfactcheck", "estimate"),
  "Fact checks where misinformation significantly reduced accuracy",
    sum(ates_w1_all_noadj$term == "treatmentcontrol" &
          ates_w1_all_noadj$p.value < 0.05 & ates_w1_all_noadj$estimate > 0),
  "Fact checks with a significant negative correction effect",
    sum(ates_w1_all_noadj$term == "treatmentfactcheck" &
          ates_w1_all_noadj$p.value < 0.05 & ates_w1_all_noadj$estimate < 0),
  "Fact checks where the correction increased false belief",
    sum(ates_w1_all_noadj$term == "treatmentfactcheck" &
          ates_w1_all_noadj$p.value < 0.05 & ates_w1_all_noadj$estimate > 0),
  "Wave 2 correction effect as a percentage of Wave 1, covariate adjusted",
    survival("ates_w2_p2_adj", "ates_w1_p2_adj"),
  "Wave 2 correction effect as a percentage of Wave 1, unadjusted",
    survival("ates_w2_p2_noadj", "ates_w1_p2_noadj"),
  "Wave 3 correction effect as a percentage of Wave 1, covariate adjusted",
    survival("ates_w3_p3_adj", "ates_w1_p3_adj"),
  "Wave 3 correction effect as a percentage of Wave 1, unadjusted",
    survival("ates_w3_p3_noadj", "ates_w1_p3_noadj"),
  "Pooled Wave 2 to Wave 1 ratio, covariate adjusted", ratio_meta("iv_p2_adj"),
  "Pooled Wave 2 to Wave 1 ratio, unadjusted", ratio_meta("iv_p2_noadj"),
  "Pooled Wave 3 to Wave 1 ratio, covariate adjusted", ratio_meta("iv_p3_adj"),
  "Pooled Wave 3 to Wave 1 ratio, unadjusted", ratio_meta("iv_p3_noadj"),
  "Largest absolute meta-analytic thermometer effect on the claim's target",
    max(abs(ft_targets_meta$estimate)),
  "Ratio of the pooled correction effect to the pooled misinformation effect",
    abs(pick_meta(ate_meta_adj, "treatmentfactcheck", "estimate")) /
      pick_meta(ate_meta_adj, "treatmentcontrol", "estimate"),
  "Belief certainty scale minimum", min(all_panels_long$outcome_w1, na.rm = TRUE),
  "Belief certainty scale maximum", max(all_panels_long$outcome_w1, na.rm = TRUE),
  "Feeling thermometer maximum", thermometer_max,
  "Distinct thermometer targets", n_distinct(read_fit("ft_ates_w1_all_adj")$target),
  "Experimental conditions", n_distinct(all_panels_long$treatment),
  "Experiments per panel", nrow(treatments_df) / n_distinct(treatments_df$panel),
  "Panels fielded on Mechanical Turk", sum(str_detect(unique(treatments_df$panel), "mturk")),
  "Panels fielded on Lucid", sum(str_detect(unique(treatments_df$panel), "lucid")),
  "Panels with a third wave", n_distinct(three_wave$panel),
  "Panels with only two waves",
    n_distinct(respondents$panel) - n_distinct(three_wave$panel),
  "Experiments in the three-wave panels", nrow(distinct(three_wave, panel, fc)),
  "Tercile groups per index", n_distinct(all_panels_long$grp_crt[!is.na(all_panels_long$grp_crt)]),
  "Maximum weeks of page views observed for a fact check", max(cume_fc_views$week_index),
  "Confidence level implied by the plotted intervals", confidence_level
)

claims <- bind_rows(
  claims,
  panel_sizes |> transmute(claim = str_c("Respondents, ", panel), value = n_respondents)
)

write_csv(claims, file.path(out_dir, "text_main_claims.csv"))
print(claims, n = nrow(claims))
