# coppock_etal_2023/maintained/helpers.R
# Output: (none; sourced by every other script)
# Depends on: original/ (fetched by download_original.R)
# Description: Packages, paths, covariate sets and shared formatting helpers.

library(here)
library(tidyverse)
library(estimatr)
library(metafor)
library(broom)
library(emmeans)
library(ggbeeswarm)
library(nnet)
library(lmtest)
library(scales)
library(xtable)

here::i_am("maintained/helpers.R")

data_dir <- here::here("original", "data")
models_dir <- here::here("original", "fitted_models")
out_dir <- here::here("maintained", "output")
fits_dir <- here::here("maintained", "output", "fits")

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
dir.create(fits_dir, showWarnings = FALSE, recursive = TRUE)

# Covariate sets ----
# Exactly as the deposited fit_all_models.R defines them. The paper's design
# section describes adjusting for demographics, political knowledge, political
# interest, cognitive reflection, need for cognition and the Big Five.
covariates_full <- c(
  "lucid_pid_7n", "lucid_age", "lucid_race", "lucid_hhin",
  "political_knowledge_pre", "political_interest_pre",
  "cognitive_reflection_pre", "need_for_cognition_pre",
  "extraversion_pre", "agreeableness_pre", "conscientiousness_pre",
  "emotional_stability_pre", "openness_to_experience_pre"
)

# The deposit drops lucid_race from the adjustment set partway through
# fit_all_models.R, under the comment "removing race?". Every model fitted after
# that point uses the reduced set: the persistence CATEs and the two families of
# thermometer models. That is a modelling choice rather than a coding slip, so it
# is preserved here and given a name instead of being hidden inside a covariate
# string that quietly changes value halfway down the file.
covariates_no_race <- setdiff(covariates_full, "lucid_race")
covariates_no_pid <- setdiff(covariates_full, "lucid_pid_7n")

rhs <- function(covariates) paste(covariates, collapse = " + ")

# The three-arm treatment is regressed with misinformation as the reference
# level, so the control coefficient is the negative of the misinformation
# effect. Flipping its sign puts both columns on the scale the paper reports.
#
# Negating an interval reverses it, so the bounds are swapped back into
# ascending order afterwards. The deposit's version of this function negates
# without swapping, which is why every misinformation cell of the published
# regression tables prints its confidence interval as [upper, lower]. The
# interval itself is right; only the order it is printed in is wrong.
filter_and_flip <- function(data) {
  data |>
    filter(term %in% c("treatmentcontrol", "treatmentfactcheck")) |>
    mutate(
      flip = term == "treatmentcontrol",
      estimate = if_else(flip, -1 * estimate, estimate),
      new_low = if_else(flip, -1 * conf.high, conf.low),
      new_high = if_else(flip, -1 * conf.low, conf.high),
      conf.low = new_low,
      conf.high = new_high
    ) |>
    select(-flip, -new_low, -new_high)
}

# Formatting, carried over from the deposit's helpers.R ----
add_parens <- function(x, digits = 3) {
  paste0("(", sprintf(paste0("%.", digits, "f"), as.numeric(x)), ")")
}

format_num <- function(x, digits = 3) {
  sprintf(paste0("%.", digits, "f"), as.numeric(x))
}

make_se_entry <- function(est, se, digits = 2) {
  paste0(format_num(est, digits = digits), " ", add_parens(se, digits = digits))
}

make_interval_entry <- function(conf.low, conf.high, digits = 2) {
  paste0("[", format_num(conf.low, digits = digits), ", ",
         format_num(conf.high, digits = digits), "]")
}

stars <- function(p) {
  c("***", "**", "*", "")[findInterval(p, c(-Inf, .001, .01, .05, Inf))]
}

# Standard error of the mean, replacing plotrix::std.error ----
std_error <- function(x, na.rm = TRUE) {
  if (na.rm) x <- x[!is.na(x)]
  sd(x) / sqrt(length(x))
}

# Model fits are written to output/fits/ as tidy CSVs so that every number a
# figure or table prints can be traced back to a file a reader can open.
# fc labels fact checks 1, 2 and 3 within a panel and is a character column in
# the deposited data, so it is read back as one rather than as a number.
read_fit <- function(name) {
  path <- file.path(fits_dir, paste0(name, ".csv"))
  header <- str_split_1(read_lines(path, n_max = 1), ",")
  read_csv(path, show_col_types = FALSE,
           col_types = if ("fc" %in% header) cols(fc = col_character()) else cols())
}

write_fit <- function(data, name) {
  write_csv(data, file.path(fits_dir, paste0(name, ".csv")))
}

# Shared ggplot2 theme, carried over from the deposit's helpers.R ----
theme_tw <- theme(
  panel.background = element_rect(color = "grey95", fill = "grey95"),
  panel.grid = element_blank(),
  strip.text.y = element_text(angle = 0),
  strip.background = element_rect(color = "grey95", fill = "grey95"),
  legend.background = element_rect(color = "white", fill = "white"),
  legend.margin = margin(-.5, 0, 0, 0, "cm"),
  panel.grid.minor = element_blank(),
  plot.title = element_text(face = "bold", hjust = 0),
  plot.subtitle = element_text(size = 10),
  plot.caption = element_text(face = "italic", size = 7),
  legend.position = "bottom"
)

# Every figure writes a CSV of the values it plots alongside the PDF and PNG:
# a PDF differs on its timestamp at every run, so it cannot be diffed.
save_figure <- function(plot, gg_df, name, width, height) {
  ggsave(file.path(out_dir, paste0(name, ".pdf")), plot = plot, width = width, height = height)
  ggsave(file.path(out_dir, paste0(name, ".png")), plot = plot, width = width, height = height, dpi = 300)
  write_csv(gg_df, file.path(out_dir, paste0(name, ".csv")))
}

# Blank a figure PDF's embedded timestamps ----
# R's pdf() device stamps /CreationDate and /ModDate with the wall clock, so an
# otherwise deterministic pipeline writes a different file on every run. The epoch
# string is the same width as what it replaces, which keeps the cross-reference byte
# offsets valid, and a file with no timestamp is left alone.
blank_pdf_timestamps <- function(path) {
  epoch <- charToRaw("D:19700101000000")
  raw_pdf <- readBin(path, "raw", file.size(path))
  hits <- grepRaw("D:[0-9]{14}", raw_pdf, all = TRUE)
  if (length(hits) == 0) return(invisible(path))
  for (h in hits) raw_pdf[h:(h + length(epoch) - 1L)] <- epoch
  writeBin(raw_pdf, path)
  invisible(path)
}
