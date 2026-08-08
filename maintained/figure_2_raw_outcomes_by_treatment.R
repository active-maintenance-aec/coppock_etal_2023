# coppock_etal_2023/maintained/figure_2_raw_outcomes_by_treatment.R
# Output: output/figure_2_raw_outcomes_by_treatment.pdf/.png/.csv
# Depends on: helpers.R, fit_all_models.R output, original/data/all_panels_long.rds,
#   original/data/treatments_df.rds
# Description: Figure 2. Belief-certainty distributions by topic and condition,
#   with the conditional means and the significance of each pairwise contrast.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))
ates_w1_all_noadj <- read_fit("ates_w1_all_noadj")

topic_order <- treatments_df |> arrange(date_fielded, panel) |> pull(topic_short_2)

all_panels_long <- all_panels_long |>
  mutate(
    treatment = factor(treatment, c("control", "misinformation", "factcheck")),
    topic_short_2 = factor(topic_short_2, topic_order)
  )

# Conditional means and their 95 per cent intervals, one point per condition.
means_df <- all_panels_long |>
  group_by(treatment, topic_short_2) |>
  summarize(
    mu = mean(outcome_w1, na.rm = TRUE),
    se = std_error(outcome_w1),
    .groups = "drop"
  ) |>
  mutate(lo = mu - se * 1.96, hi = mu + se * 1.96)

# Significance stars sit between the two conditions being compared: the
# control/misinformation contrast at x = 1.5 and misinformation/fact-check at 2.5.
stars_df <- ates_w1_all_noadj |>
  left_join(treatments_df |> select(panel, fc, topic_short_2), by = c("panel", "fc")) |>
  filter(p.value < .05) |>
  mutate(
    topic_short_2 = factor(topic_short_2, topic_order),
    lab = stars(p.value),
    xnum = if_else(str_detect(term, "control"), 1.5, 2.5)
  )

gg_df <- means_df |>
  full_join(stars_df |> select(topic_short_2, term, xnum, star = lab, p.value),
            by = "topic_short_2")

g <- all_panels_long |>
  ggplot() +
  geom_quasirandom(
    aes(x = treatment, y = outcome_w1),
    shape = 21, fill = "grey20", alpha = .075, width = .3
  ) +
  geom_rect(
    aes(xmin = as.numeric(treatment) - .01, xmax = as.numeric(treatment) + .01,
        ymin = lo - .3, ymax = hi + .3),
    fill = "white", color = "white", data = means_df
  ) +
  geom_linerange(aes(treatment, ymin = lo, ymax = hi), data = means_df) +
  geom_point(aes(treatment, mu), shape = 21, size = 7, fill = "white", data = means_df) +
  geom_text(aes(treatment, mu, label = round(mu, 0)), data = means_df, size = 2.65) +
  geom_text(aes(xnum, 83, label = lab), data = stars_df, size = 3) +
  facet_wrap(~topic_short_2, ncol = 3, labeller = label_wrap_gen(30)) +
  labs(
    x = "Experimental condition",
    y = "Certainty of agreement\n(larger values indicate certain agreement, smaller value certain disagreement)"
  ) +
  scale_x_discrete(
    breaks = levels(means_df$treatment),
    labels = c("Control", "Misinformation", "Fact-check")
  ) +
  theme_bw() +
  theme_tw

save_figure(g, gg_df, "figure_2_raw_outcomes_by_treatment", width = 10, height = 12)
