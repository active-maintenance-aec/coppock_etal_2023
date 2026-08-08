# coppock_etal_2023/maintained/tables_a2_a8_ate_cate_persistence.R
# Output: output/table_a2_ates_w1.csv, output/table_a3_cates_w1_dems.csv,
#   output/table_a4_cates_w1_reps.csv, output/table_a5_persistence_p2_noadj.csv,
#   output/table_a6_persistence_p2_adj.csv, output/table_a7_persistence_p3_noadj.csv,
#   output/table_a8_persistence_p3_adj.csv, and a *_cells.csv beside each one
# Depends on: helpers.R, fit_all_models.R output, original/data/treatments_df.rds
# Description: The seven regression tables in section 6 of the appendix. Each
#   cell is an estimate, its standard error, a significance marker and a 95 per
#   cent interval, with a meta-analytic row appended to every table. Every table
#   also writes its cells unrounded, so that a check against the published table
#   reads the number the model produced rather than the string the table prints.

source(here::here("maintained", "helpers.R"))

treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))

# Both effect columns side by side, one row per fact check ----
# The two contrasts arrive as separate rows of a tidy fit; the table wants them
# in two columns, joined on everything that identifies the fact check.
side_by_side_cells <- function(data, extra_keys = character()) {
  keys <- c("fc", "panel", "panel_factor", "outcome", extra_keys)
  cell <- function(term_name, prefix) {
    data |>
      filter(term == term_name) |>
      select(all_of(keys), estimate, std.error, p.value, conf.low, conf.high) |>
      rename_with(~ paste0(prefix, "_", .x),
                  c(estimate, std.error, p.value, conf.low, conf.high))
  }
  cell("treatmentcontrol", "ctrl") |>
    left_join(cell("treatmentfactcheck", "chk"), by = keys) |>
    arrange(panel_factor, fc) |>
    left_join(treatments_df |> select(topic_short, panel, fc), by = c("panel", "fc")) |>
    select(topic_short, panel_factor, all_of(extra_keys), starts_with("ctrl_"), starts_with("chk_"))
}

side_by_side <- function(cells, extra_keys = character()) {
  cells |>
    mutate(
      `Average Effect of Misinformation versus Control` = paste0(
        make_se_entry(ctrl_estimate, ctrl_std.error),
        if_else(ctrl_p.value < 0.05, "* ", " "),
        make_interval_entry(ctrl_conf.low, ctrl_conf.high)
      ),
      `Average Effect of Correction versus Misinformation` = paste0(
        make_se_entry(chk_estimate, chk_std.error),
        if_else(chk_p.value < 0.05, "* ", " "),
        make_interval_entry(chk_conf.low, chk_conf.high)
      )
    ) |>
    select(topic_short, panel_factor, all_of(extra_keys), covariates,
           `Average Effect of Misinformation versus Control`,
           `Average Effect of Correction versus Misinformation`)
}

# The persistence tables have one effect column, since the misinformation-only
# arm is excluded from the follow-up waves by design.
single_column_cells <- function(data) {
  data |>
    filter(str_detect(term, "treatment|outcome")) |>
    left_join(treatments_df |> select(topic_short, panel, fc), by = c("panel", "fc")) |>
    select(topic_short, panel_factor, estimate, std.error, p.value, conf.low, conf.high)
}

single_column <- function(cells) {
  cells |>
    mutate(
      `Average Effect of Correction versus Control` = paste0(
        make_se_entry(estimate, std.error),
        if_else(p.value < 0.05, "* ", " "),
        make_interval_entry(conf.low, conf.high)
      )
    ) |>
    select(topic_short, panel_factor, wave, covariates,
           `Average Effect of Correction versus Control`)
}

# A meta-analytic row, formatted to slot into the table body.
meta_row <- function(data, groups, outcome_label = "outcome_w1") {
  data |>
    mutate(term2 = term) |>
    group_by(across(all_of(groups))) |>
    reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
                 conf.int = TRUE)) |>
    mutate(
      term = term2,
      panel_factor = "Meta-analysis",
      fc = "Meta-analysis",
      panel = "Meta-analysis",
      outcome = outcome_label
    ) |>
    select(-any_of(c("term2", "type")))
}

meta_first <- function(data, ...) {
  data |>
    mutate(
      panel_factor = factor(panel_factor) |> relevel(ref = "Meta-analysis"),
      panel_factor = factor(panel_factor, levels = rev(levels(panel_factor)))
    ) |>
    arrange(panel_factor, topic_short, ...)
}

# Every table is written twice: the formatted body a reader compares against the
# published page, and the same rows unrounded so that a downstream check reads
# the model's number rather than a string already rounded to two decimals.
write_table <- function(cells, formatted, name) {
  write_csv(formatted, file.path(out_dir, paste0(name, ".csv")))
  write_csv(cells, file.path(out_dir, paste0(name, "_cells.csv")))
}

# Appendix section 6, Table 1: average effects at Wave 1 ----
ates_w1_all_noadj <- read_fit("ates_w1_all_noadj")
ates_w1_all_adj <- read_fit("ates_w1_all_adj")

ate_w1_cells <- bind_rows(
  ates_w1_all_noadj |> side_by_side_cells() |> mutate(covariates = "no"),
  meta_row(ates_w1_all_noadj, "term2") |> side_by_side_cells() |>
    mutate(covariates = "no", topic_short = "Meta-analysis"),
  ates_w1_all_adj |> side_by_side_cells() |> mutate(covariates = "yes"),
  meta_row(ates_w1_all_adj, "term2") |> side_by_side_cells() |>
    mutate(covariates = "yes", topic_short = "Meta-analysis")
) |>
  meta_first(covariates)

write_table(ate_w1_cells, side_by_side(ate_w1_cells), "table_a2_ates_w1")

# Appendix section 6, Tables 2 and 3: effects at Wave 1 by party ----
cates_w1_all_noadj <- read_fit("cates_w1_all_noadj")
cates_w1_all_adj <- read_fit("cates_w1_all_adj")

cate_w1_cells <- bind_rows(
  cates_w1_all_noadj |> side_by_side_cells("lucid_pid_3") |> mutate(covariates = "no"),
  meta_row(cates_w1_all_noadj, c("lucid_pid_3", "term2")) |> side_by_side_cells("lucid_pid_3") |>
    mutate(covariates = "no", topic_short = "Meta-analysis"),
  cates_w1_all_adj |> side_by_side_cells("lucid_pid_3") |> mutate(covariates = "yes"),
  meta_row(cates_w1_all_adj, c("lucid_pid_3", "term2")) |> side_by_side_cells("lucid_pid_3") |>
    mutate(covariates = "yes", topic_short = "Meta-analysis")
) |>
  meta_first(covariates, lucid_pid_3)

cate_w1_table <- side_by_side(cate_w1_cells, "lucid_pid_3")

write_table(cate_w1_cells |> filter(lucid_pid_3 == "Democrat"),
            cate_w1_table |> filter(lucid_pid_3 == "Democrat"),
            "table_a3_cates_w1_dems")

write_table(cate_w1_cells |> filter(lucid_pid_3 == "Republican"),
            cate_w1_table |> filter(lucid_pid_3 == "Republican"),
            "table_a4_cates_w1_reps")

persistence_cells <- function(pieces, wave_levels) {
  pieces |>
    list_rbind() |>
    mutate(
      wave = factor(wave, wave_levels),
      panel_factor = factor(panel_factor) |> relevel(ref = "Meta-analysis"),
      panel_factor = factor(panel_factor, levels = rev(levels(panel_factor)))
    ) |>
    select(topic_short, panel_factor, wave, covariates,
           estimate, std.error, p.value, conf.low, conf.high) |>
    arrange(panel_factor, topic_short, covariates, wave)
}

# Appendix section 6, Tables 4 and 5: persistence to Wave 2 ----
p2_pieces <- list(
  read_fit("ates_w1_p2_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "w1"),
  read_fit("ates_w1_p2_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "w1"),
  read_fit("ates_w2_p2_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "w2"),
  read_fit("ates_w2_p2_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "w2"),
  read_fit("iv_p2_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "ratio"),
  read_fit("iv_p2_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "ratio"),
  meta_row(read_fit("ates_w1_p2_noadj"), "term2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "w1"),
  meta_row(read_fit("ates_w1_p2_adj"), "term2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "w1"),
  meta_row(read_fit("ates_w2_p2_noadj"), "term2", "outcome_w2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "w2"),
  meta_row(read_fit("ates_w2_p2_adj"), "term2", "outcome_w2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "w2"),
  meta_row(read_fit("iv_p2_noadj"), "term2", "outcome_w2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "ratio"),
  meta_row(read_fit("iv_p2_adj"), "term2", "outcome_w2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "ratio")
)

persistence_p2_cells <- persistence_cells(p2_pieces, c("w1", "w2", "ratio"))

walk2(
  c("no", "yes"),
  c("table_a5_persistence_p2_noadj", "table_a6_persistence_p2_adj"),
  function(adjustment, name) {
    cells <- persistence_p2_cells |> filter(covariates == adjustment)
    write_table(cells, single_column(cells), name)
  }
)

# Appendix section 6, Tables 6 and 7: persistence to Wave 3 ----
p3_pieces <- list(
  read_fit("ates_w1_p3_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "w1"),
  read_fit("ates_w1_p3_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "w1"),
  read_fit("ates_w3_p3_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "w3"),
  read_fit("ates_w3_p3_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "w3"),
  read_fit("iv_p3_noadj") |> single_column_cells() |> mutate(covariates = "no", wave = "ratio"),
  read_fit("iv_p3_adj") |> single_column_cells() |> mutate(covariates = "yes", wave = "ratio"),
  meta_row(read_fit("ates_w1_p3_noadj"), "term2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "w1"),
  meta_row(read_fit("ates_w1_p3_adj"), "term2") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "w1"),
  meta_row(read_fit("ates_w3_p3_noadj"), "term2", "outcome_w3") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "w3"),
  meta_row(read_fit("ates_w3_p3_adj"), "term2", "outcome_w3") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "w3"),
  meta_row(read_fit("iv_p3_noadj"), "term2", "outcome_w3") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "no", wave = "ratio"),
  meta_row(read_fit("iv_p3_adj"), "term2", "outcome_w3") |> single_column_cells() |>
    mutate(topic_short = "Meta-analysis", covariates = "yes", wave = "ratio")
)

persistence_p3_cells <- persistence_cells(p3_pieces, c("w1", "w3", "ratio"))

walk2(
  c("no", "yes"),
  c("table_a7_persistence_p3_noadj", "table_a8_persistence_p3_adj"),
  function(adjustment, name) {
    cells <- persistence_p3_cells |> filter(covariates == adjustment)
    write_table(cells, single_column(cells), name)
  }
)

print(ate_w1_cells |> filter(topic_short == "Meta-analysis") |>
        select(covariates, ctrl_estimate, ctrl_std.error, chk_estimate, chk_std.error))
