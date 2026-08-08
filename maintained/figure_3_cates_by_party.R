# coppock_etal_2023/maintained/figure_3_cates_by_party.R
# Output: output/figure_3_cates_by_party.pdf/.png/.csv,
#   output/figure_3_partisan_difference_brackets.csv
# Depends on: helpers.R, fit_all_models.R output, original/data/treatments_df.rds
# Description: Figure 3. Covariate-adjusted effects of misinformation and of
#   corrections at Wave 1, separately for Democrats and Republicans, with a
#   meta-analytic summary per party and congeniality.

source(here::here("maintained", "helpers.R"))

cates_w1_all_adj <- read_fit("cates_w1_all_adj")
diff_in_cates_w1_all_adj <- read_fit("diff_in_cates_w1_all_adj")
treatments_df <- read_rds(file.path(data_dir, "treatments_df.rds"))

# The point labels drop the leading zero, as the published figure does.
drop_leading_zero <- function(x) {
  txt <- as.character(round(x, 1))
  if_else(str_extract(txt, "\\d") == "0", str_replace(txt, "\\d", ""), txt)
}

cates <- cates_w1_all_adj |>
  left_join(treatments_df |> select(fc, panel, topic_short_2, congeniality),
            by = c("fc", "panel")) |>
  mutate(
    congeniality = str_wrap(congeniality, 15) |> fct_inorder() |> fct_rev(),
    lab = drop_leading_zero(estimate),
    type = case_when(
      str_detect(term, "control") ~ "Misinformation effect",
      str_detect(term, "factcheck") ~ "Correction effect"
    ) |>
      factor(c("Misinformation effect", "Correction effect")),
    pg = if_else(p.value < 0.05, "sig", "insig")
  )

# Topics run from the largest to the smallest average correction effect within
# each congeniality block.
cates$topic_short_2 <- factor(
  cates$topic_short_2,
  cates |>
    filter(!str_detect(term, "control")) |>
    group_by(congeniality, topic_short_2) |>
    summarize(mu = mean(estimate), .groups = "drop") |>
    arrange(desc(congeniality), desc(mu)) |>
    pull(topic_short_2)
)

# tidy() on an rma object returns columns called term and type of its own, so
# the contrast is carried through the pooling under a different name.
meta_df <- cates |>
  mutate(contrast = term) |>
  group_by(congeniality, lucid_pid_3, contrast) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
               conf.int = TRUE)) |>
  select(-any_of(c("term", "type"))) |>
  mutate(
    term = contrast,
    topic_short_2 = factor("Overall", c("Overall", " ", levels(cates$topic_short_2))),
    type = if_else(str_detect(contrast, "control"), "Misinformation effect", "Correction effect") |>
      factor(levels(cates$type)),
    pg = if_else(p.value < 0.05, "sig", "insig"),
    lab = as.character(round(estimate, 1))
  )

gg_df <- bind_rows(
  meta_df,
  cates |> mutate(topic_short_2 = factor(topic_short_2, levels(meta_df$topic_short_2)))
)

# Brackets marking topics where the two parties' effects differ significantly.
brackets <- diff_in_cates_w1_all_adj |>
  filter(p.value < .05) |>
  mutate(type = if_else(str_detect(term, "control"),
                        "Misinformation effect", "Correction effect")) |>
  left_join(cates |> distinct(topic_short_2, congeniality), by = "topic_short_2") |>
  left_join(
    cates |>
      group_by(topic_short_2, type) |>
      summarize(lo = min(estimate), hi = max(estimate), .groups = "drop"),
    by = c("topic_short_2", "type")
  ) |>
  mutate(
    lab = stars(p.value),
    mid = (lo + hi) / 2,
    yint = if_else(congeniality == "False claim\ncongenial to\nRepublicans", -10, 0),
    topic_short_2 = factor(topic_short_2, levels(gg_df$topic_short_2)),
    type = factor(type, levels(gg_df$type))
  )

# A blank row per facet keeps the "Overall" summary separated from the topics.
spacer <- tibble(
  estimate = 1,
  topic_short_2 = " ",
  type = "Misinformation effect",
  congeniality = c("False claim\ncongenial to\nRepublicans", "False claim\ncongenial to\nDemocrats")
) |>
  mutate(
    topic_short_2 = factor(topic_short_2, levels(gg_df$topic_short_2)),
    type = factor(type, levels(gg_df$type)),
    congeniality = factor(congeniality, levels(gg_df$congeniality))
  )

party_labels <- tibble(x = c(-6, 18), lab = c("Democrats", "Republicans")) |>
  mutate(
    lucid_pid_3 = factor(c("Democrat", "Republican"), c("Democrat", "Republican")),
    type = factor("Misinformation effect", levels(gg_df$type)),
    congeniality = factor(levels(gg_df$congeniality)[1], levels(gg_df$congeniality))
  )

g <- gg_df |>
  ggplot() +
  geom_vline(aes(xintercept = 0), linewidth = .25, linetype = "dashed") +
  geom_blank(aes(estimate, y = topic_short_2), data = bind_rows(gg_df, spacer)) +
  geom_linerange(
    aes(xmin = conf.low + if_else(str_detect(type, "Correction"), -.075, .075),
        xmax = conf.high + if_else(str_detect(type, "Correction"), .075, -.075),
        group = lucid_pid_3, y = topic_short_2),
    color = "black", linewidth = .75, position = position_dodge(.65 / 4), data = gg_df
  ) +
  geom_linerange(
    aes(xmin = conf.low, xmax = conf.high, color = lucid_pid_3, y = topic_short_2),
    position = position_dodge(.65 / 4), data = gg_df
  ) +
  geom_segment(
    aes(x = lo, xend = hi,
        y = as.numeric(topic_short_2) + yint + .5,
        yend = as.numeric(topic_short_2) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_segment(
    aes(x = lo, xend = lo, y = as.numeric(topic_short_2) + yint,
        yend = as.numeric(topic_short_2) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_segment(
    aes(x = hi, xend = hi, y = as.numeric(topic_short_2) + yint,
        yend = as.numeric(topic_short_2) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_label(
    aes(x = mid, label = lab, y = as.numeric(topic_short_2) + .41 + yint),
    data = brackets, fill = "grey95", label.padding = unit(0.125, "lines"),
    linewidth = 0, fontface = "bold", size = 3
  ) +
  geom_label(
    aes(x, label = lab, fill = lucid_pid_3, group = lucid_pid_3, y = 16),
    data = party_labels, position = position_dodge(width = 2),
    size = 2.5, color = "black", fontface = "italic"
  ) +
  geom_point(
    aes(estimate, topic_short_2, fill = lucid_pid_3, group = lucid_pid_3, size = pg),
    shape = 21, color = "black", position = position_dodge(.65 / 4), data = gg_df
  ) +
  geom_text(
    aes(estimate, topic_short_2, label = lab, group = lucid_pid_3),
    size = 1.75, color = "black", position = position_nudge(y = .16 / 4),
    data = gg_df |> filter(p.value < .05, lucid_pid_3 == "Republican")
  ) +
  geom_text(
    aes(estimate, topic_short_2, label = lab, group = lucid_pid_3),
    size = 1.75, color = "black", position = position_nudge(y = -.16 / 4),
    data = gg_df |> filter(p.value < .05, lucid_pid_3 != "Republican")
  ) +
  facet_grid(congeniality ~ type, scales = "free", space = "free", drop = TRUE) +
  labs(x = "", y = "") +
  scale_size_manual(values = c(3, 6)) +
  scale_fill_manual(values = rev(c("grey99", "grey65"))) +
  scale_color_manual(values = rev(c("grey80", "grey65"))) +
  scale_y_discrete(breaks = levels(gg_df$topic_short_2), drop = TRUE) +
  theme_bw() +
  theme_tw

save_figure(g, gg_df, "figure_3_cates_by_party", width = 9, height = 10)
write_csv(brackets, file.path(out_dir, "figure_3_partisan_difference_brackets.csv"))
