# coppock_etal_2023/maintained/figure_6_ft_targets.R
# Output: output/figure_6_ft_targets.pdf/.png/.csv
# Depends on: helpers.R, fit_all_models.R output
# Description: Figure 6. Effects of misinformation and of corrections on feeling
#   thermometers toward the person or group each false claim targets, by party
#   and overall, with a meta-analytic average in each row.

source(here::here("maintained", "helpers.R"))

party_levels <- c("Democrat", "Independent", "Republican", "Overall")

label_effect <- function(x) {
  if_else(str_detect(x, "control"), "Misinformation effect", "Correction effect") |>
    factor(c("Misinformation effect", "Correction effect"))
}

gg_df <- bind_rows(
  read_fit("ft_targets_ates_w1_PID_adj"),
  read_fit("ft_targets_ates_w1_all_adj") |> mutate(lucid_pid_3 = "Overall")
) |>
  mutate(
    lucid_pid_3 = factor(lucid_pid_3, party_levels),
    term = label_effect(term),
    sig = if_else(p.value < .05, "p <.05", "p >= .05")
  )

meta_df <- bind_rows(
  read_fit("ft_targets_meta_w1_PID_adj"),
  read_fit("ft_targets_meta_w1_all_adj") |> mutate(lucid_pid_3 = "Overall")
) |>
  mutate(
    lucid_pid_3 = factor(lucid_pid_3, party_levels),
    term = label_effect(term)
  )

# The diamond labels print the estimate to two decimals without a leading zero,
# padded so that "-.2" and ".5" both read as two decimal places.
meta_df$lab <- round(meta_df$estimate, 2) |>
  as.character() |>
  str_replace(fixed("0."), fixed("."))
meta_df$lab <- if_else(
  str_detect(meta_df$lab, "-"),
  str_pad(meta_df$lab, width = 4, side = "right", pad = "0"),
  str_pad(meta_df$lab, width = 3, side = "right", pad = "0")
)
meta_df$lab <- str_c(meta_df$lab, stars(meta_df$p.value))

significant_targets <- gg_df |>
  filter(p.value <= .05) |>
  mutate(lab = str_to_title(str_replace_all(str_remove(dv, "^w1_ft_"), "_", " ")))

g <- gg_df |>
  ggplot() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_dotplot(
    aes(x = lucid_pid_3, y = estimate, fill = sig),
    binwidth = .25, binaxis = "y", stackdir = "center",
    binpositions = "all", dotsize = .8
  ) +
  geom_segment(
    aes(x = lucid_pid_3, xend = lucid_pid_3, y = conf.low, yend = conf.high),
    linewidth = 1, data = meta_df
  ) +
  geom_point(aes(x = lucid_pid_3, y = estimate),
             data = meta_df, shape = 23, size = 9.5, fill = "white") +
  geom_text(aes(lucid_pid_3, estimate, label = lab), size = 2.75, data = meta_df) +
  geom_text(
    aes(y = estimate, label = str_replace(lab, " ", "\n"), x = lucid_pid_3),
    data = significant_targets, position = position_nudge(x = .075),
    fontface = "italic", size = 2.5, lineheight = .7, vjust = 0
  ) +
  coord_flip() +
  labs(x = "", y = "Difference on 100 point feeling thermometer") +
  facet_wrap(~term, nrow = 1) +
  scale_fill_grey(start = .3, end = 1) +
  theme_bw() +
  theme_tw

save_figure(g, gg_df, "figure_6_ft_targets", width = 9, height = 5)
write_csv(meta_df, file.path(out_dir, "figure_6_ft_targets_meta.csv"))
