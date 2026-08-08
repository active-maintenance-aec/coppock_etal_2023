# coppock_etal_2023/maintained/fit_all_models.R
# Output: output/fits/*.csv (one tidy CSV per family of model fits)
# Depends on: helpers.R, original/data/all_panels_long.rds,
#   original/data/treatments_df.rds, original/data/ft_targets_long.csv
# Description: Fit every model the figures and tables need, starting from the
#   deposited respondent-level data. The deposit also ships these fits as
#   fitted_models/*.rds; nothing downstream of this file reads them, so every
#   published number in output/ traces back to the survey data rather than to a
#   saved object whose provenance cannot be checked.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))
ft_targets_long <- read_csv(file.path(data_dir, "ft_targets_long.csv"), show_col_types = FALSE)

persistence_p2_df <- all_panels_long |>
  filter(!is.na(outcome_w1), !is.na(outcome_w2), treatment != "misinformation")

persistence_p3_df <- all_panels_long |>
  filter(!is.na(outcome_w1), !is.na(outcome_w3), treatment != "misinformation")

# One model per fact check, in a named group ----
fit_by_group <- function(data, formula_text, groups) {
  data |>
    group_by(across(all_of(groups))) |>
    reframe(tidy(lm_robust(formula(formula_text), data = pick(everything()))))
}

fit_iv_by_group <- function(data, formula_text, groups) {
  data |>
    group_by(across(all_of(groups))) |>
    reframe(tidy(iv_robust(formula(formula_text), data = pick(everything())))) |>
    filter(term == "outcome_w1")
}

pool <- function(data, groups) {
  data |>
    group_by(across(all_of(groups))) |>
    reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
                 conf.int = TRUE))
}

ate_groups <- c("fc", "panel", "panel_factor", "rating")
cate_groups <- c("fc", "panel", "panel_factor", "lucid_pid_3")
persistence_groups <- c("fc", "panel", "panel_factor")

# Average treatment effects at Wave 1 ----
ates_w1_all_noadj <- all_panels_long |>
  fit_by_group("outcome_w1 ~ treatment", ate_groups) |>
  filter_and_flip()

ates_w1_all_adj <- all_panels_long |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_full)), ate_groups) |>
  filter_and_flip()

write_fit(ates_w1_all_noadj, "ates_w1_all_noadj")
write_fit(ates_w1_all_adj, "ates_w1_all_adj")

# Conditional average treatment effects by party at Wave 1 ----
cates_w1_all_noadj <- all_panels_long |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group("outcome_w1 ~ treatment", cate_groups) |>
  filter_and_flip()

cates_w1_all_adj <- all_panels_long |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_full)), cate_groups) |>
  filter_and_flip()

# The Democrat/Republican difference in each effect, from a fully interacted model.
diff_in_cates_w1_all_adj <- all_panels_long |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(
    paste0("outcome_w1 ~ treatment + lucid_pid_3 + lucid_pid_3 * treatment + lucid_pid_3 * (",
           rhs(covariates_full), ")"),
    c("fc", "panel", "topic_short_2")
  ) |>
  filter(term %in% c("treatmentcontrol:lucid_pid_3Republican",
                     "treatmentfactcheck:lucid_pid_3Republican"))

write_fit(cates_w1_all_noadj, "cates_w1_all_noadj")
write_fit(cates_w1_all_adj, "cates_w1_all_adj")
write_fit(diff_in_cates_w1_all_adj, "diff_in_cates_w1_all_adj")

# Effects within the top and bottom tercile of each trait ----
trait_columns <- c("grp_crt", "grp_polint", "grp_nfc", "grp_openness",
                   "grp_conscientiousness", "grp_extraversion", "grp_agree",
                   "grp_emotstab", "grp_pk")

by_trait <- function(data) {
  data |>
    pivot_longer(cols = all_of(trait_columns)) |>
    filter(!is.na(value), value != "med")
}

trait_cates_w1_all_adj <- all_panels_long |>
  by_trait() |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_full)),
               c("name", "value", "fc", "panel", "panel_factor")) |>
  filter_and_flip() |>
  mutate(type2 = term)

meta_trait_cates_w1_all_adj <- pool(trait_cates_w1_all_adj, c("name", "value", "type2"))

# Whether the high and low groups differ, as a meta-regression on the tercile.
meta_trait_dic_w1_all_adj <- trait_cates_w1_all_adj |>
  group_by(name, type2) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, mods = value,
                       data = pick(everything())), conf.int = TRUE)) |>
  filter(term == "mods")

trait_cates_w2_p2_adj <- persistence_p2_df |>
  by_trait() |>
  fit_by_group(paste("outcome_w2 ~ treatment +", rhs(covariates_full)),
               c("name", "value", "fc", "panel", "panel_factor")) |>
  filter_and_flip() |>
  mutate(type2 = term)

meta_trait_cates_w2_p2_adj <- pool(trait_cates_w2_p2_adj, c("name", "value", "type2"))

write_fit(trait_cates_w1_all_adj, "trait_cates_w1_all_adj")
write_fit(meta_trait_cates_w1_all_adj, "meta_trait_cates_w1_all_adj")
write_fit(meta_trait_dic_w1_all_adj, "meta_trait_dic_w1_all_adj")
write_fit(trait_cates_w2_p2_adj, "trait_cates_w2_p2_adj")
write_fit(meta_trait_cates_w2_p2_adj, "meta_trait_cates_w2_p2_adj")

# Persistence from Wave 1 to Wave 2 ----
ates_w1_p2_noadj <- persistence_p2_df |>
  fit_by_group("outcome_w1 ~ treatment", persistence_groups) |>
  filter_and_flip()

ates_w2_p2_noadj <- persistence_p2_df |>
  fit_by_group("outcome_w2 ~ treatment", persistence_groups) |>
  filter_and_flip()

ates_w1_p2_adj <- persistence_p2_df |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_full)), persistence_groups) |>
  filter_and_flip()

ates_w2_p2_adj <- persistence_p2_df |>
  fit_by_group(paste("outcome_w2 ~ treatment +", rhs(covariates_full)), persistence_groups) |>
  filter_and_flip()

iv_p2_noadj <- fit_iv_by_group(persistence_p2_df,
                               "outcome_w2 ~ outcome_w1 | treatment", persistence_groups)

iv_p2_adj <- fit_iv_by_group(
  persistence_p2_df,
  paste0("outcome_w2 ~ outcome_w1 + ", rhs(covariates_full),
         " | treatment + ", rhs(covariates_full)),
  persistence_groups
)

# Persistence from Wave 1 to Wave 3 ----
# The deposited script fits the adjusted models on the wrong outcomes: its
# ates_w1_p3_adj and ates_w2_p3_adj both regress outcome_w3, and its
# ates_w3_p3_adj regresses outcome_w2, while iv_p3_adj instruments outcome_w2
# rather than outcome_w3. The unadjusted block immediately above it in the same
# file pairs each wave with its own outcome, which is what the names say and
# what the covariate-adjusted CATE block further down also does, so this is a
# copy-and-paste slip rather than a modelling decision. It is corrected here and
# the effect on the published tables is reported in the repository README.
ates_w1_p3_noadj <- persistence_p3_df |>
  fit_by_group("outcome_w1 ~ treatment", persistence_groups) |>
  filter_and_flip()

ates_w2_p3_noadj <- persistence_p3_df |>
  fit_by_group("outcome_w2 ~ treatment", persistence_groups) |>
  filter_and_flip()

ates_w3_p3_noadj <- persistence_p3_df |>
  fit_by_group("outcome_w3 ~ treatment", persistence_groups) |>
  filter_and_flip()

ates_w1_p3_adj <- persistence_p3_df |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_full)), persistence_groups) |>
  filter_and_flip()

ates_w2_p3_adj <- persistence_p3_df |>
  fit_by_group(paste("outcome_w2 ~ treatment +", rhs(covariates_full)), persistence_groups) |>
  filter_and_flip()

ates_w3_p3_adj <- persistence_p3_df |>
  fit_by_group(paste("outcome_w3 ~ treatment +", rhs(covariates_full)), persistence_groups) |>
  filter_and_flip()

iv_p3_noadj <- fit_iv_by_group(persistence_p3_df,
                               "outcome_w3 ~ outcome_w1 | treatment", persistence_groups)

iv_p3_adj <- fit_iv_by_group(
  persistence_p3_df,
  paste0("outcome_w3 ~ outcome_w1 + ", rhs(covariates_full),
         " | treatment + ", rhs(covariates_full)),
  persistence_groups
)

walk2(
  list(ates_w1_p2_noadj, ates_w2_p2_noadj, ates_w1_p2_adj, ates_w2_p2_adj,
       iv_p2_noadj, iv_p2_adj,
       ates_w1_p3_noadj, ates_w2_p3_noadj, ates_w3_p3_noadj,
       ates_w1_p3_adj, ates_w2_p3_adj, ates_w3_p3_adj,
       iv_p3_noadj, iv_p3_adj),
  c("ates_w1_p2_noadj", "ates_w2_p2_noadj", "ates_w1_p2_adj", "ates_w2_p2_adj",
    "iv_p2_noadj", "iv_p2_adj",
    "ates_w1_p3_noadj", "ates_w2_p3_noadj", "ates_w3_p3_noadj",
    "ates_w1_p3_adj", "ates_w2_p3_adj", "ates_w3_p3_adj",
    "iv_p3_noadj", "iv_p3_adj"),
  write_fit
)

# Persistence by party ----
# Everything from here on uses the reduced covariate set; see helpers.R.
cates_w1_p2_adj <- persistence_p2_df |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_no_race)), cate_groups) |>
  filter_and_flip()

cates_w2_p2_adj <- persistence_p2_df |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w2 ~ treatment +", rhs(covariates_no_race)), cate_groups) |>
  filter_and_flip()

cates_w1_p3_adj <- persistence_p3_df |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w1 ~ treatment +", rhs(covariates_no_race)), cate_groups) |>
  filter_and_flip()

cates_w2_p3_adj <- persistence_p3_df |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w2 ~ treatment +", rhs(covariates_no_race)), cate_groups) |>
  filter_and_flip()

cates_w3_p3_adj <- persistence_p3_df |>
  filter(lucid_pid_3 != "Independent") |>
  fit_by_group(paste("outcome_w3 ~ treatment +", rhs(covariates_no_race)), cate_groups) |>
  filter_and_flip()

walk2(
  list(cates_w1_p2_adj, cates_w2_p2_adj, cates_w1_p3_adj, cates_w2_p3_adj, cates_w3_p3_adj),
  c("cates_w1_p2_adj", "cates_w2_p2_adj", "cates_w1_p3_adj", "cates_w2_p3_adj", "cates_w3_p3_adj"),
  write_fit
)

# Meta-analytic persistence surface plotted in Figure 4 ----
pool_persistence <- function(data, groups, wave, issues, subjects = NULL) {
  out <- data |>
    left_join(treatments_df, by = c("fc", "panel")) |>
    pool(groups) |>
    mutate(wave = wave, issues = issues)
  if (!is.null(subjects)) out$lucid_pid_3 <- subjects
  out
}

persistence_meta <- bind_rows(
  pool_persistence(ates_w1_p2_adj, "congeniality", 1, "All Issues", "All Subjects"),
  pool_persistence(ates_w2_p2_adj, "congeniality", 2, "All Issues", "All Subjects"),
  pool_persistence(ates_w1_p3_adj, "congeniality", 1, "Issues across three waves", "All Subjects"),
  pool_persistence(ates_w2_p3_adj, "congeniality", 2, "Issues across three waves", "All Subjects"),
  pool_persistence(ates_w3_p3_adj, "congeniality", 3, "Issues across three waves", "All Subjects"),
  pool_persistence(cates_w1_p2_adj, c("lucid_pid_3", "congeniality"), 1, "All Issues"),
  pool_persistence(cates_w2_p2_adj, c("lucid_pid_3", "congeniality"), 2, "All Issues"),
  pool_persistence(cates_w1_p3_adj, c("lucid_pid_3", "congeniality"), 1, "Issues across three waves"),
  pool_persistence(cates_w2_p3_adj, c("lucid_pid_3", "congeniality"), 2, "Issues across three waves"),
  pool_persistence(cates_w3_p3_adj, c("lucid_pid_3", "congeniality"), 3, "Issues across three waves")
)

write_fit(persistence_meta, "persistence_meta")

# Feeling thermometers, all targets asked in each panel ----
ft_long <- all_panels_long |>
  pivot_longer(cols = starts_with("w1_ft"), names_to = "target", values_to = "thermometer") |>
  filter(!is.na(thermometer))

ft_ates_w1_all_adj <- ft_long |>
  filter(!is.na(lucid_pid_3)) |>
  fit_by_group(paste("thermometer ~ treatment +", rhs(covariates_no_pid)),
               c("fc", "panel", "panel_factor", "target", "lucid_pid_3")) |>
  filter_and_flip()

meta_ft_ates <- ft_ates_w1_all_adj |>
  rename(ty2 = term) |>
  pool(c("lucid_pid_3", "target", "ty2"))

write_fit(ft_ates_w1_all_adj, "ft_ates_w1_all_adj")
write_fit(meta_ft_ates, "meta_ft_ates_w1_all_adj")

# Feeling thermometers, restricted to the target of each false claim ----
# The deposited loop writes filter(all_panels_long, fc == fc, topic_short_2 ==
# topic_short_2). Inside filter(), the bare names resolve to the columns rather
# than to the loop's local variables, so both conditions are a column compared
# with itself and neither one subsets anything: every fit runs on all eight
# panels pooled instead of on the panel that carried the false claim. The loop
# extracts fc and topic_short_2 one row at a time precisely in order to subset,
# so the intent is unambiguous. The subset is applied here.
#
# Applying it costs one of the twenty-three target rows. Panel 3 (Lucid) carried
# the claim that Obama failed to nominate US judges and names Barack Obama as one
# of its two targets, but that panel never asked the Barack Obama thermometer, so
# the correctly subset regression has no observations. Pooled over all eight
# panels the same regression returns an estimate, computed entirely from
# respondents who were never shown the claim. The row is dropped here and
# recorded in output/fits/ft_targets_unfittable.csv.
ft_target_rows <- ft_targets_long |>
  filter(!is.na(target_of_misinformation)) |>
  mutate(n_observed = pmap_int(
    list(fc, topic_short_2, target_of_misinformation),
    function(fc_i, topic_i, dv)
      sum(!is.na(all_panels_long[[dv]][all_panels_long$fc == fc_i &
                                         all_panels_long$topic_short_2 == topic_i]))
  ))

ft_target_rows |>
  filter(n_observed == 0) |>
  select(fc, panel, topic_short_2, target_of_misinformation, n_observed) |>
  write_fit("ft_targets_unfittable")

ft_target_rows <- ft_target_rows |> filter(n_observed > 0)

fit_ft_target <- function(dv, fc_i, topic_i, covariates, pid = NULL) {
  d <- all_panels_long |> filter(fc == fc_i, topic_short_2 == topic_i)
  if (!is.null(pid)) d <- d |> filter(lucid_pid_3 == pid)
  lm_robust(formula(paste0("`", dv, "` ~ treatment + ", rhs(covariates))), data = d) |>
    tidy() |>
    mutate(dv = dv, fc = fc_i, topic_short_2 = topic_i) |>
    filter_and_flip()
}

ft_targets_ates_w1_all_adj <- ft_target_rows |>
  pmap(function(fc, topic_short_2, target_of_misinformation, ...)
    fit_ft_target(target_of_misinformation, fc, topic_short_2, covariates_no_race)) |>
  list_rbind()

ft_targets_meta_w1_all_adj <- ft_targets_ates_w1_all_adj |>
  mutate(term2 = term) |>
  pool("term2") |>
  mutate(term = term2, outcome = "META")

ft_targets_ates_w1_PID_adj <- c("Democrat", "Republican", "Independent") |>
  map(function(pid)
    ft_target_rows |>
      pmap(function(fc, topic_short_2, target_of_misinformation, ...)
        fit_ft_target(target_of_misinformation, fc, topic_short_2, covariates_no_pid, pid)) |>
      list_rbind() |>
      mutate(lucid_pid_3 = pid)) |>
  list_rbind()

ft_targets_meta_w1_PID_adj <- ft_targets_ates_w1_PID_adj |>
  mutate(term2 = term) |>
  pool(c("term2", "lucid_pid_3")) |>
  mutate(term = term2, outcome = "META")

write_fit(ft_targets_ates_w1_all_adj, "ft_targets_ates_w1_all_adj")
write_fit(ft_targets_meta_w1_all_adj, "ft_targets_meta_w1_all_adj")
write_fit(ft_targets_ates_w1_PID_adj, "ft_targets_ates_w1_PID_adj")
write_fit(ft_targets_meta_w1_PID_adj, "ft_targets_meta_w1_PID_adj")

print(tibble(fits_written = length(list.files(fits_dir, pattern = "\\.csv$"))))
