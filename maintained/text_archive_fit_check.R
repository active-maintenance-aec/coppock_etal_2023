# coppock_etal_2023/maintained/text_archive_fit_check.R
# Output: output/text_archive_fit_check.csv
# Depends on: helpers.R, fit_all_models.R output, original/fitted_models/
# Description: Compare every model fit rebuilt from the survey data against the
#   matching object the deposit ships in fitted_models/. The deposit's figure and
#   table scripts read those objects rather than the data, so this is the only
#   check that the deposited estimates follow from the deposited respondents.
#   Five objects are expected to disagree: they are the four persistence models
#   and the thermometer-target models whose specifications fit_all_models.R
#   corrects, and each is refitted here under the deposit's own specification so
#   that the disagreement can be attributed to the correction rather than left
#   ambiguous.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
ft_targets_long <- read_csv(file.path(data_dir, "ft_targets_long.csv"), show_col_types = FALSE)

persistence_p3_df <- all_panels_long |>
  filter(!is.na(outcome_w1), !is.na(outcome_w3), treatment != "misinformation")

persistence_groups <- c("fc", "panel", "panel_factor")

numeric_columns <- c("estimate", "std.error", "statistic", "p.value", "conf.low", "conf.high")

# Line up two tidy fits on whatever labelling columns they share, then take the
# largest absolute difference over every numeric column.
# key_exclude names labelling columns that must not be joined on. The corrected
# fits carry a different outcome variable by construction, so joining on the
# outcome name would match nothing and report a vacuous zero-row comparison.
compare <- function(rewrite, deposited, label, key_exclude = character()) {
  rewrite <- as_tibble(rewrite)
  deposited <- as_tibble(deposited)
  shared <- setdiff(intersect(names(rewrite), names(deposited)), key_exclude)
  keys <- setdiff(shared, numeric_columns)
  values <- intersect(shared, numeric_columns)
  joined <- inner_join(
    rewrite |> select(all_of(shared)) |> mutate(across(all_of(keys), as.character)),
    deposited |> select(all_of(shared)) |> mutate(across(all_of(keys), as.character)),
    by = keys, suffix = c("_rewrite", "_deposit")
  )
  # Reported per column, not as one maximum over all six. The rewrite reverses
  # the confidence interval the deposit prints backwards for every negated
  # contrast, so a single maximum is the width of the widest interval in the
  # object and says nothing about whether the two agree. Separating the columns
  # is what makes the estimate column readable as agreement.
  difference <- function(v) {
    a <- joined[[paste0(v, "_rewrite")]]
    b <- joined[[paste0(v, "_deposit")]]
    both <- !is.na(a) & !is.na(b)
    if (!any(both)) NA_real_ else max(abs(a[both] - b[both]))
  }
  # A cell one side computes and the other returns as missing is invisible to a
  # difference, and it is how the political knowledge estimates move: the two
  # estimatr versions disagree about which HC2 standard errors are estimable.
  na_disagreements <- map_int(values, function(v)
    sum(xor(is.na(joined[[paste0(v, "_rewrite")]]), is.na(joined[[paste0(v, "_deposit")]]))))
  tibble(
    object = label,
    rows_deposited = nrow(deposited),
    rows_rewrite = nrow(rewrite),
    rows_matched = nrow(joined),
    max_abs_difference_estimate = difference("estimate"),
    max_abs_difference_std_error = difference("std.error"),
    max_abs_difference_p_value = difference("p.value"),
    max_abs_difference_interval = max(difference("conf.low"), difference("conf.high"),
                                      na.rm = TRUE),
    cells_missing_on_one_side = sum(na_disagreements)
  )
}

deposited_objects <- c(
  "ates_w1_all_noadj", "ates_w1_all_adj", "cates_w1_all_noadj", "cates_w1_all_adj",
  "diff_in_cates_w1_all_adj", "trait_cates_w1_all_adj", "meta_trait_cates_w1_all_adj",
  "trait_cates_w2_p2_adj", "ates_w1_p2_noadj", "ates_w1_p2_adj", "ates_w2_p2_noadj",
  "ates_w2_p2_adj", "iv_p2_noadj", "iv_p2_adj", "ates_w1_p3_noadj", "ates_w3_p3_noadj",
  "iv_p3_noadj", "cates_w1_p2_adj", "cates_w2_p2_adj", "cates_w1_p3_adj",
  "cates_w2_p3_adj", "cates_w3_p3_adj", "persistence_meta", "ft_ates_w1_all_adj",
  "meta_ft_ates_w1_all_adj", "ft_targets_ates_w1_all_adj", "ft_targets_meta_w1_all_adj",
  "ft_targets_ates_w1_PID_adj", "ft_targets_meta_w1_PID_adj",
  "ates_w1_p3_adj", "ates_w3_p3_adj", "iv_p3_adj"
)

as_deposited <- function(name) read_rds(file.path(models_dir, paste0(name, ".rds")))

corrected <- c("ates_w1_p3_adj", "ates_w3_p3_adj", "iv_p3_adj",
               "ft_targets_ates_w1_all_adj", "ft_targets_ates_w1_PID_adj")

check <- map(deposited_objects, function(name)
  compare(read_fit(name), as_deposited(name), name,
          key_exclude = if (name %in% corrected) c("outcome", "df") else character())) |>
  list_rbind()

# Two deposited objects carry no file extension and are the ones the deposit's
# own scripts read, while the .rds files the deposit's code writes under those
# names are absent from the archive.
check <- bind_rows(
  check,
  compare(read_fit("meta_trait_dic_w1_all_adj"),
          read_rds(file.path(models_dir, "meta_trait_dic_w1_all_adj")),
          "meta_trait_dic_w1_all_adj"),
  compare(read_fit("meta_trait_cates_w2_p2_adj"),
          read_rds(file.path(models_dir, "meta_trait_cates_w2_p2_adj")),
          "meta_trait_cates_w2_p2_adj")
)

# The deposit's own specifications for the objects the rewrite corrects ----
archive_ates_w1_p3_adj <- persistence_p3_df |>
  group_by(across(all_of(persistence_groups))) |>
  reframe(tidy(lm_robust(formula(paste("outcome_w3 ~ treatment +", rhs(covariates_full))),
                         data = pick(everything())))) |>
  filter_and_flip()

archive_ates_w3_p3_adj <- persistence_p3_df |>
  group_by(across(all_of(persistence_groups))) |>
  reframe(tidy(lm_robust(formula(paste("outcome_w2 ~ treatment +", rhs(covariates_full))),
                         data = pick(everything())))) |>
  filter_and_flip()

archive_iv_p3_adj <- persistence_p3_df |>
  group_by(across(all_of(persistence_groups))) |>
  reframe(tidy(iv_robust(formula(paste0("outcome_w2 ~ outcome_w1 + ", rhs(covariates_full),
                                        " | treatment + ", rhs(covariates_full))),
                         data = pick(everything())))) |>
  filter(term == "outcome_w1")

archive_ft_target <- function(dv, covariates, pid = NULL) {
  d <- all_panels_long
  if (!is.null(pid)) d <- d |> filter(lucid_pid_3 == pid)
  lm_robust(formula(paste0("`", dv, "` ~ treatment + ", rhs(covariates))), data = d) |>
    tidy() |>
    mutate(dv = dv) |>
    filter_and_flip()
}

ft_target_rows <- ft_targets_long |> filter(!is.na(target_of_misinformation))

archive_ft_targets_all <- ft_target_rows |>
  pmap(function(fc, topic_short_2, target_of_misinformation, ...)
    archive_ft_target(target_of_misinformation, covariates_no_race) |>
      mutate(fc = fc, topic_short_2 = topic_short_2)) |>
  list_rbind()

archive_ft_targets_pid <- c("Democrat", "Republican", "Independent") |>
  map(function(pid)
    ft_target_rows |>
      pmap(function(fc, topic_short_2, target_of_misinformation, ...)
        archive_ft_target(target_of_misinformation, covariates_no_pid, pid) |>
          mutate(fc = fc, topic_short_2 = topic_short_2)) |>
      list_rbind() |>
      mutate(lucid_pid_3 = pid)) |>
  list_rbind()

archive_check <- bind_rows(
  compare(archive_ates_w1_p3_adj, as_deposited("ates_w1_p3_adj"), "ates_w1_p3_adj"),
  compare(archive_ates_w3_p3_adj, as_deposited("ates_w3_p3_adj"), "ates_w3_p3_adj"),
  compare(archive_iv_p3_adj, as_deposited("iv_p3_adj"), "iv_p3_adj"),
  compare(archive_ft_targets_all, as_deposited("ft_targets_ates_w1_all_adj"),
          "ft_targets_ates_w1_all_adj"),
  compare(archive_ft_targets_pid, as_deposited("ft_targets_ates_w1_PID_adj"),
          "ft_targets_ates_w1_PID_adj")
) |>
  select(object,
         max_abs_difference_estimate_archive_spec = max_abs_difference_estimate)

check <- check |>
  left_join(archive_check, by = "object") |>
  arrange(desc(max_abs_difference_estimate), object, .locale = "en")

write_csv(check, file.path(out_dir, "text_archive_fit_check.csv"))
print(check, n = nrow(check))
