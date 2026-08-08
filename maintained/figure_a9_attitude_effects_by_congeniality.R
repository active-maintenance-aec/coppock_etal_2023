# coppock_etal_2023/maintained/figure_a9_attitude_effects_by_congeniality.R
# Output: output/figure_a9_attitude_effects_by_congeniality.pdf/.png/.csv
# Depends on: helpers.R, original/data/all_panels_long.rds
# Description: Appendix Figure 9. Meta-analytic misinformation and correction
#   effects on all twenty-two feeling thermometers, grouped by respondent party
#   and by the partisan congeniality of the false claim. The deposited script for
#   this figure reads data/clean/all_panels_long.rds, a path the archive does not
#   contain, so it stops on its first line; the data it wants is at
#   data/all_panels_long.rds and everything below it runs once that is fixed.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))

thermometers <- all_panels_long |>
  select(admin_id, treatment, lucid_pid_3, topic_short, congeniality,
         contains("_ft_")) |>
  pivot_longer(contains("_ft_"), names_to = "target", values_to = "thermometer",
               values_drop_na = TRUE)

# One model per claim and thermometer, then the two consecutive contrasts within
# each party. The treatment factor runs misinformation, control, fact-check, so
# consecutive contrasts give control against misinformation and fact-check
# against control.
contrasts_df <- thermometers |>
  group_by(topic_short, congeniality, target) |>
  nest() |>
  mutate(estimates = map(data, function(d)
    emmeans(
      lm_robust(thermometer ~ treatment * lucid_pid_3, data = d),
      consec ~ treatment | lucid_pid_3,
      data = d,
      at = list(lucid_pid_3 = c("Democrat", "Republican", "Independent")),
      adjust = "none"
    )$contrasts |>
      tidy(conf.int = TRUE))) |>
  select(-data) |>
  unnest(estimates) |>
  ungroup() |>
  mutate(
    effect_type = if_else(str_detect(contrast, "control - "),
                          "Misinformation effect", "Correction effect") |>
      fct_rev()
  )

gg_df <- contrasts_df |>
  drop_na(estimate, std.error) |>
  group_by(congeniality, effect_type, lucid_pid_3) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything()),
                       control = list(maxiter = 10000)), conf.int = TRUE)) |>
  select(-any_of(c("term", "type"))) |>
  mutate(
    lab = str_c(
      round(estimate, 2) |>
        as.character() |>
        str_replace("0\\.", ".") |>
        str_pad(width = 3, side = "right", pad = "0"),
      stars(p.value)
    ),
    congeniality = str_replace(congeniality, "False claim congenial to", "...to")
  )

g <- gg_df |>
  ggplot() +
  geom_vline(xintercept = 0, linewidth = .3, linetype = "dashed") +
  geom_label(
    aes(x = estimate, y = congeniality, label = lab, color = lucid_pid_3),
    size = 3, fill = "grey95", label.size = 0, position = position_nudge(y = .25)
  ) +
  geom_pointrange(
    aes(xmin = conf.low, xmax = conf.high, x = estimate,
        y = congeniality, color = lucid_pid_3)
  ) +
  facet_grid(lucid_pid_3 ~ effect_type) +
  labs(x = "Difference on 100pt feeling thermometer scale",
       y = "False claim congenial to...") +
  theme_bw() +
  theme_tw +
  theme(legend.position = "none")

save_figure(g, gg_df, "figure_a9_attitude_effects_by_congeniality", width = 9, height = 6)
print(gg_df |> select(congeniality, effect_type, lucid_pid_3, estimate, conf.low, conf.high, p.value, lab))
