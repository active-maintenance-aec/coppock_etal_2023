# coppock_etal_2023/maintained/table_a1_sample_characteristics.R
# Output: output/table_a1_sample_characteristics.csv
# Depends on: helpers.R, original/data/all_panels_long.rds
# Description: Appendix Table 1. Respondent counts, fielding dates and
#   demographic composition of each of the eight panels.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))

# One row per respondent: the long file repeats each respondent once per fact
# check, so a single fact check identifies the panel's respondents exactly once.
respondents <- all_panels_long |> filter(fc == 1)

counts_and_dates <- respondents |>
  group_by(panel) |>
  summarize(
    obs = n(),
    date_lo = min(as.Date(admin_StartDate), na.rm = TRUE),
    date_hi = max(as.Date(admin_StartDate), na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    date_lo = str_c(ordinal(mday(date_lo)), " ", month(date_lo, label = TRUE, abbr = TRUE)),
    date_hi = str_c(ordinal(mday(date_hi)), " ", month(date_hi, label = TRUE, abbr = TRUE)),
    obs = prettyNum(obs, big.mark = ",")
  ) |>
  pivot_longer(-panel, names_to = "cols", values_to = "vals") |>
  pivot_wider(values_from = vals, names_from = panel) |>
  slice(c(2:3, 1))

# Household income arrives as a bracket label; the midpoint of the two dollar
# figures in the label is what the published bands are cut from.
income_midpoint <- function(x) {
  str_extract_all(x, "\\d{1,3},\\d{3}") |>
    map_dbl(function(v) mean(as.numeric(str_remove_all(v, ","))))
}

demographics <- respondents |>
  transmute(
    panel,
    Gender = if_else(lucid_female == 1, "Female", "Male") |> fct_infreq(),
    Race = fct_infreq(lucid_race),
    Age = if_else(lucid_age > 100, 2020 - lucid_age, lucid_age) |>
      round() |>
      cut(c(-Inf, 34, 50, 65, Inf), labels = c("18-34", "35-50", "51-65", ">65")),
    Education = fct_collapse(
      lucid_education,
      "HSD or less" = c("3rd Grade or less", "Middle School - Grades 4 - 8",
                        "Completed some high school", "High school graduate",
                        "Other post high school vocational training"),
      "Some college" = c("Completed some college, but no degree", "Associate's degree"),
      "BA degree" = "Bachelor's degree",
      "Graduate education" = c("Completed some graduate, but no degree",
                               "Masters degree", "Doctorate degree")
    ) |>
      factor(c("HSD or less", "Some college", "BA degree", "Graduate education")),
    Income = income_midpoint(lucid_hhi) |>
      cut(c(-Inf, 40000, 80000, 120000, Inf),
          labels = c("<$40k", "$40-80k", "$80-120k", ">$120k")),
    Partisanship = factor(lucid_pid_3, c("Democrat", "Independent", "Republican")),
    Region = fct_inorder(lucid_region)
  )

composition <- demographics |>
  pivot_longer(-panel, names_to = "nms", values_to = "vals") |>
  mutate(
    vals = factor(vals, demographics |> select(-panel) |> map(levels) |> unlist() |> as.character()),
    nms = factor(nms, c("Gender", "Race", "Age", "Education", "Income",
                        "Partisanship", "Region"))
  ) |>
  group_by(panel, nms, vals) |>
  tally() |>
  drop_na() |>
  mutate(perc = as.character(round(n / sum(n) * 100))) |>
  select(-n) |>
  pivot_wider(names_from = panel, values_from = perc)

# The row label prints only once per block, as the published table does.
tab <- counts_and_dates |>
  rename(nms = cols) |>
  bind_rows(composition) |>
  select(nms, vals, everything()) |>
  group_by(nms) |>
  mutate(nms = c(nms[[1]], rep("", n() - 1))) |>
  ungroup()

write_csv(tab, file.path(out_dir, "table_a1_sample_characteristics.csv"))
print(tab, n = nrow(tab))
