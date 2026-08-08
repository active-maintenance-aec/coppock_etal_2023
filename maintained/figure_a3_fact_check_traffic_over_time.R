# coppock_etal_2023/maintained/figure_a3_fact_check_traffic_over_time.R
# Output: output/figure_a3_fact_check_traffic_over_time.pdf/.png/.csv
# Depends on: helpers.R, original/data/cume_fc_views.rds,
#   original/data/final_fc_views.rds, original/data/int_dates_for_fc_views.rds
# Description: Appendix Figure 3. Cumulative page views of each fact-check over
#   the weeks after it was posted, with the fielding window of the panel that
#   used it shaded. The deposited script sets family = "Roboto" on three text
#   layers and stops on a font the archive does not ship; no font is requested
#   here, which is the only change the figure needs.

source(here::here("maintained", "helpers.R"))

cumulative_views <- read_rds(file.path(data_dir, "cume_fc_views.rds"))
final_views <- read_rds(file.path(data_dir, "final_fc_views.rds"))
fielding_windows <- read_rds(file.path(data_dir, "int_dates_for_fc_views.rds")) |>
  drop_na()

congeniality_labels <- tibble(
  x = as.Date(c("2020-09-20", "2020-09-10")),
  y = c(300e3, 205e3),
  lab = c("False claim\ncongenial\nto Reps.", "False claim\ncongenial\nto Dems.")
) |>
  mutate(topic_short = factor(
    c("Hunter Biden's laptop had photos torturing children",
      "Donald Trump claimed his DNA was USA"),
    levels(final_views$topic_short)
  ))

fielding_label <- tibble(
  x = as.Date("2020-10-20"),
  y = 400e3,
  lab = "Survey\nfielding\nperiod",
  topic_short = factor("GOP voters' pens invisible to voting machines",
                       levels(final_views$topic_short))
)

gg_df <- cumulative_views

g <- gg_df |>
  ggplot() +
  geom_rect(aes(xmin = start, xmax = end), ymin = -30000, ymax = 5.35e05,
            linewidth = .1, fill = "white", color = "grey2", data = fielding_windows) +
  geom_line(aes(rd2, cume_views, linetype = congeniality, group = fn), linewidth = .5) +
  geom_point(aes(rd2, cume_views), size = 9, shape = 21, fill = "white",
             data = final_views) +
  geom_text(aes(rd2, cume_views, label = lab), fontface = "italic", size = 2.25,
            data = final_views) +
  geom_text(aes(x, y, label = lab), fontface = "italic", size = 3, lineheight = .75,
            hjust = 0, data = congeniality_labels) +
  geom_text(aes(x, y, label = lab), fontface = "italic", size = 3, lineheight = .75,
            hjust = 0, data = fielding_label) +
  scale_y_continuous(breaks = seq(0, 500e3, 250e3), labels = c(0, 250, 500),
                     expand = expansion(add = c(45000, 45000))) +
  facet_wrap(~topic_short, nrow = 3, labeller = label_wrap_gen(25)) +
  labs(x = "", y = "Cumulative views of fact check (000s of visits)") +
  theme_bw() +
  theme_tw +
  theme(legend.position = "none")

save_figure(g, gg_df, "figure_a3_fact_check_traffic_over_time", width = 11, height = 8)

print(final_views |> select(topic_short, congeniality, cume_views) |> arrange(desc(cume_views)))
