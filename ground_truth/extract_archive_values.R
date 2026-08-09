# coppock_etal_2023/ground_truth/extract_archive_values.R
# Output: ground_truth/archive_values.csv
# Depends on: original/ (fetched by download_original.R)
# Description: Read every published quantity the deposit can answer out of the
#   objects the deposit ships, so that value_script in the ground truth is
#   generated rather than typed.
#
#   The deposit's figure and table scripts write nothing: their only uncommented
#   file-touching calls are the thirty-four write_rds() lines in fit_all_models.R.
#   So there is no deposited artifact to diff, and the deposit's answer to a
#   published claim is whatever its saved model objects plus its own formatting
#   code produce. Both are read here; nothing is re-fitted.
#
#   Claims with no deposited counterpart are simply absent from the output, and
#   the build records value_script as missing for them.

library(tidyverse)
library(here)
library(metafor)
library(broom)

here::i_am("ground_truth/extract_archive_values.R")

models_dir <- here::here("original", "fitted_models")
data_dir <- here::here("original", "data")
stopifnot(dir.exists(models_dir), dir.exists(data_dir))

deposited <- function(name) as_tibble(read_rds(file.path(models_dir, paste0(name, ".rds"))))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))

# Pooled Wave 1 effects ----
# tidy() on an rma object returns a term column of its own, so the contrast is
# carried through the pooling under a different name.
pool_ates <- function(name) {
  deposited(name) |>
    mutate(contrast = term) |>
    group_by(contrast) |>
    reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything()))))
}

meta_adj <- pool_ates("ates_w1_all_adj")
meta_noadj <- pool_ates("ates_w1_all_noadj")

pick_meta <- function(meta, contrast_name, column) {
  value <- meta[[column]][meta$contrast == contrast_name]
  stopifnot(length(value) == 1)
  value
}

ates_noadj <- deposited("ates_w1_all_noadj")

# The pre-analysis plan's persistence quantity, which the appendix prints in the
# "ratio" row of each persistence table. The pooling method is named rather than
# left to the default, because which family the pool belongs to is the question
# these rows exist to answer.
ratio_meta <- function(name, method) {
  d <- deposited(name)
  100 * as.numeric(coef(rma.uni(yi = d$estimate, sei = d$std.error, method = method)))
}

prose <- tribble(
  ~claim_id, ~value_script,
  "art_intro_total_n", sum(all_panels_long$fc == 1),
  "art_intro_fact_checks", n_distinct(treatments_df$topic_short),
  "res_opportunities", nrow(treatments_df),
  "art_intro_lag_days", treatments_df |>
    distinct(topic_short, date_posted, date_fielded) |>
    summarize(m = mean(as.numeric(difftime(date_fielded, date_posted, units = "days")))) |>
    pull(m),
  "design_traffic_rep", treatments_df |>
    distinct(topic_short, congeniality, traffic) |>
    filter(str_detect(congeniality, "Republicans")) |>
    summarize(m = mean(traffic)) |> pull(m),
  "design_traffic_dem", treatments_df |>
    distinct(topic_short, congeniality, traffic) |>
    filter(str_detect(congeniality, "Democrats")) |>
    summarize(m = mean(traffic)) |> pull(m),
  "art_intro_misinfo_effect", pick_meta(meta_adj, "treatmentcontrol", "estimate"),
  "art_intro_correction_effect", abs(pick_meta(meta_adj, "treatmentfactcheck", "estimate")),
  "res_meta_misinfo", pick_meta(meta_adj, "treatmentcontrol", "estimate"),
  "res_meta_misinfo_se", pick_meta(meta_adj, "treatmentcontrol", "std.error"),
  "res_meta_correction", pick_meta(meta_adj, "treatmentfactcheck", "estimate"),
  "res_meta_correction_se", pick_meta(meta_adj, "treatmentfactcheck", "std.error"),
  "res_misinfo_sig_count", sum(ates_noadj$term == "treatmentcontrol" &
                                 ates_noadj$p.value < 0.05 & ates_noadj$estimate > 0),
  "res_fc_sig_count", sum(ates_noadj$term == "treatmentfactcheck" &
                            ates_noadj$p.value < 0.05 & ates_noadj$estimate < 0),
  "art_intro_persist_w2", ratio_meta("iv_p2_adj", "REML"),
  "res_persist_w2_precise", ratio_meta("iv_p2_adj", "REML"),
  "disc_persist_w2", ratio_meta("iv_p2_adj", "REML"),
  "art_intro_persist_w3", ratio_meta("iv_p3_adj", "REML"),
  "res_persist_w3_precise", ratio_meta("iv_p3_adj", "REML"),
  "disc_persist_w3", ratio_meta("iv_p3_adj", "REML")
)

# Panel sizes ----
panel_slug <- all_panels_long |>
  filter(fc == 1) |>
  count(panel, name = "n_respondents") |>
  transmute(claim_id = str_c("app_t1_n_", panel), value_script = n_respondents)

# Figure 5 trait labels ----
figure_5 <- deposited("meta_trait_cates_w1_all_adj") |>
  transmute(claim_id = str_c("fig5_", name, "_", value, "_", type2), value_script = estimate)

# Figure 6 thermometer labels ----
figure_6 <- bind_rows(
  deposited("ft_targets_meta_w1_all_adj") |> mutate(lucid_pid_3 = "overall"),
  deposited("ft_targets_meta_w1_PID_adj")
) |>
  transmute(claim_id = str_c("fig6_", lucid_pid_3, "_", term2), value_script = estimate)

# Appendix regression table cells ----
# The deposit's own table script is run in a scratch copy and its printed LaTeX
# is parsed, rather than the cells being rebuilt here from the deposited fits.
# Rebuilding them cannot be made faithful: the deposit's significance marker is
# not a comparison of numbers in every table. Its reshape passes each fit through
# reshape2::melt, and where a fit carries a character column outside id.vars
# (rating, in the Wave 1 average-effect table) every melted value becomes a
# string, so the surviving if_else(p.value < 0.05, ...) compares two strings:
# "0.0144" sorts before "0.05" and earns a star while "1.28e-10" sorts after it
# and does not. Tables built from fits with no such column keep the numeric
# comparison. Running the script is the only way to get all of that right.
archive_run_dir <- Sys.getenv("ARCHIVE_RUN_DIR",
                              unset = file.path(tempdir(), "coppock_etal_2023_archive"))
table_dir <- file.path(archive_run_dir, "appendix_tables")
unlink(table_dir, recursive = TRUE)
dir.create(table_dir, recursive = TRUE, showWarnings = FALSE)
file.copy(list.files(here::here("original"), full.names = TRUE), table_dir, recursive = TRUE)

table_script <- "code/appendix_tables_2_3_4_5_6_and_7.R"
old_wd <- setwd(table_dir)
table_output <- system2("Rscript", c("--vanilla", shQuote(table_script)),
                        stdout = TRUE, stderr = TRUE)
setwd(old_wd)
stopifnot(is.null(attr(table_output, "status")))

cell_pattern <- "(-?\\d+\\.\\d+) \\((\\d+\\.\\d+)\\)(\\*?) \\[(-?\\d+\\.\\d+), (-?\\d+\\.\\d+)\\]"

# The script prints the seven table bodies in the order the appendix numbers
# them, one \\\\-terminated row at a time, so a table boundary is a line that
# opens a new body. The bodies are separated by xtable's own comment header.
body_starts <- which(str_detect(table_output, "^\\s*% latex table generated"))
stopifnot(length(body_starts) == 7)
body_end <- c(body_starts[-1], length(table_output) + 1) - 1

archive_cells <- map(seq_along(body_starts), function(i) {
  block <- table_output[body_starts[i]:body_end[i]]
  tibble(appendix_table = i,
         cell = str_extract_all(block, cell_pattern) |> unlist())
}) |>
  list_rbind()

# An unstarred cell is an empty significance_marker, which read_csv parses as
# missing, and a missing marker propagates through str_c and silently turns every
# unstarred published cell into NA.
published_cells <- read_csv(here::here("ground_truth", "published_appendix_values.csv"),
                            show_col_types = FALSE) |>
  mutate(significance_marker = replace_na(significance_marker, ""),
         published_cell = str_c(sprintf("%.2f", estimate), " (", sprintf("%.2f", std_error), ")",
                                significance_marker, " [", sprintf("%.2f", interval_first), ", ",
                                sprintf("%.2f", interval_second), "]"))

# A published cell counts as matched when the deposit prints a cell with the same
# string somewhere in the same table. Matching on the string rather than on a row
# label is what keeps this insensitive to the row order the appendix chose, which
# the deposit's output does not carry in a form the published page preserves.
cells_matched <- published_cells |>
  group_by(appendix_table) |>
  summarize(matched = sum(published_cell %in%
                            archive_cells$cell[archive_cells$appendix_table ==
                                                 first(appendix_table)]),
            .groups = "drop") |>
  transmute(claim_id = str_c("app_reg_t", appendix_table, "_cells"), value_script = matched)

archive_values <- bind_rows(
  prose,
  panel_slug,
  figure_5,
  figure_6,
  cells_matched
) |>
  arrange(claim_id, .locale = "en")

stopifnot(!any(duplicated(archive_values$claim_id)))

write_csv(archive_values, here::here("ground_truth", "archive_values.csv"))

print(tibble(claims_answered_by_the_deposit = nrow(archive_values)))
