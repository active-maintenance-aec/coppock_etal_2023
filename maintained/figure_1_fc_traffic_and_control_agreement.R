# coppock_etal_2023/maintained/figure_1_fc_traffic_and_control_agreement.R
# Output: output/figure_1_fc_traffic_and_control_agreement.pdf/.png/.csv
# Depends on: helpers.R, original/data/all_panels_long.rds, original/data/treatments_df.rds
# Description: Figure 1. Cumulative page views of each fact-check alongside
#   agreement with the false claim among control-group Democrats and Republicans.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds"))
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))

gg_df <- all_panels_long |>
  group_by(congeniality, lucid_pid_3, treatment, topic_short_2) |>
  summarize(
    mu = mean(outcome_w1, na.rm = TRUE),
    se = std_error(outcome_w1),
    .groups = "drop"
  ) |>
  mutate(lo = mu - se * 1.96, hi = mu + se * 1.96) |>
  filter(treatment == "control", lucid_pid_3 %in% c("Democrat", "Republican")) |>
  mutate(cfac = "Agreement in control") |>
  bind_rows(
    treatments_df |>
      select(topic_short_2, congeniality, traffic) |>
      mutate(cfac = "Fact check website traffic")
  ) |>
  left_join(
    treatments_df |>
      transmute(
        topic_short_2,
        ts2 = str_c(topic_short_2, " (", mday(date_fielded), " ",
                    month(date_fielded, label = TRUE, abbr = TRUE), ")")
      ),
    by = "topic_short_2"
  ) |>
  mutate(
    cfac = factor(cfac, c("Fact check website traffic", "Agreement in control")) |> fct_rev(),
    congeniality = str_wrap(congeniality, width = 15) |>
      factor(c("False claim\ncongenial to\nRepublicans", "False claim\ncongenial to\nDemocrats"))
  )

# Fact-checks are ordered by the traffic their page received.
gg_df$ts2 <- factor(
  gg_df$ts2,
  gg_df |>
    filter(!is.na(traffic)) |>
    group_by(ts2) |>
    summarize(traf = max(traffic), .groups = "drop") |>
    arrange(traf) |>
    pull(ts2)
)

party_labels <- tibble(
  x = c(45, 20.5),
  y = levels(gg_df$ts2)[24],
  lab = c("Republicans", "Democrats")
) |>
  mutate(
    y = factor(y, levels(gg_df$ts2)),
    cfac = factor(levels(gg_df$cfac)[1], levels(gg_df$cfac)),
    congeniality = factor(levels(gg_df$congeniality)[1], levels(gg_df$congeniality))
  )

g <- ggplot() +
  geom_col(
    aes(traffic, ts2),
    data = gg_df |> filter(str_detect(cfac, "traffic")),
    fill = "grey98", color = "grey2", linewidth = .2, width = .55
  ) +
  geom_linerange(
    aes(xmin = lo, xmax = hi, y = ts2, group = lucid_pid_3),
    data = gg_df |> filter(!str_detect(cfac, "traffic")),
    position = position_dodge(width = .1), linewidth = .3
  ) +
  geom_point(
    aes(mu, ts2, fill = lucid_pid_3),
    data = gg_df |> filter(!str_detect(cfac, "traffic")),
    shape = 21, size = 2.5, position = position_dodge(width = .1)
  ) +
  geom_text(
    aes(x, y, label = lab), data = party_labels,
    hjust = 0, size = 2.7, lineheight = .7, fontface = "italic"
  ) +
  facet_grid(congeniality ~ cfac, scales = "free", space = "free_y", drop = TRUE) +
  theme_bw() +
  theme_tw +
  labs(x = "", y = "", fill = "")

save_figure(g, gg_df, "figure_1_fc_traffic_and_control_agreement", width = 9, height = 7)
