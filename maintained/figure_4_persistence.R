# coppock_etal_2023/maintained/figure_4_persistence.R
# Output: output/figure_4_persistence.pdf/.png/.csv
# Depends on: helpers.R, fit_all_models.R output
# Description: Figure 4. Meta-analytic correction effects at each wave, for the
#   six-panel two-wave set and the two-panel three-wave set, overall and by party.

source(here::here("maintained", "helpers.R"))

gg_df <- read_fit("persistence_meta") |>
  mutate(
    lucid_pid_3 = recode(lucid_pid_3,
                         Democrat = "Democratic Subjects",
                         Republican = "Republican Subjects") |>
      factor(c("All Subjects", "Democratic Subjects", "Republican Subjects"))
  )

congeniality_labels <- tibble(
  x = c(2, 2),
  y = c(-15, 5),
  lab = c("False claim congenial\nto Democrats", "False claim congenial\nto Republicans")
) |>
  mutate(
    lucid_pid_3 = factor("All Subjects",
                         c("All Subjects", "Democratic Subjects", "Republican Subjects")),
    issues = factor("All Issues", unique(gg_df$issues))
  )

g <- gg_df |>
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_blank(aes(wave, estimate)) +
  geom_line(
    aes(wave, estimate, group = congeniality),
    linewidth = .375, position = position_dodge(width = .2)
  ) +
  geom_pointrange(
    aes(wave, estimate, ymin = conf.low, ymax = conf.high, fill = congeniality),
    shape = 21, color = "black", position = position_dodge(width = .2), size = .5
  ) +
  geom_text(
    aes(x, y, label = lab), data = congeniality_labels,
    fontface = "italic", size = 3, hjust = .5, lineheight = .75
  ) +
  scale_x_continuous(breaks = 1:3, labels = str_c("Wave ", 1:3),
                     expand = expansion(add = .3)) +
  facet_grid(issues ~ lucid_pid_3, labeller = label_wrap_gen(15)) +
  labs(x = "", y = "Correction effect estimate", fill = "") +
  theme_bw() +
  theme_tw

save_figure(g, gg_df, "figure_4_persistence", width = 9, height = 6)
