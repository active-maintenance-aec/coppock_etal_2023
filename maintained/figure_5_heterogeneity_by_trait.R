# coppock_etal_2023/maintained/figure_5_heterogeneity_by_trait.R
# Output: output/figure_5_heterogeneity_by_trait.pdf/.png/.csv,
#   output/figure_5_trait_difference_brackets.csv
# Depends on: helpers.R, fit_all_models.R output
# Description: Figure 5. Effects of misinformation and of corrections for
#   subjects in the top and bottom tercile of each personality and cognitive
#   style measure, with meta-analytic summaries and tests of the difference.

source(here::here("maintained", "helpers.R"))

trait_levels <- c(
  grp_polint = "Political interest",
  grp_pk = "Political knowledge",
  grp_crt = "Cognitive Reflection Test",
  grp_nfc = "Need for Cognition",
  grp_emotstab = "Personality - Emotional Stability",
  grp_extraversion = "Personality - Extraversion",
  grp_agree = "Personality - Agreeableness",
  grp_conscientiousness = "Personality - Conscientiousness",
  grp_openness = "Personality - Openness"
)

label_trait <- function(x) factor(unname(trait_levels[x]), unname(trait_levels))

label_row <- function(x) {
  if_else(str_detect(x, "Personality"), "Personality\ntraits", "Other\nheterogeneity\ndimensions") |>
    factor(c("Personality\ntraits", "Other\nheterogeneity\ndimensions"))
}

label_column <- function(x) {
  if_else(str_detect(x, "control"), "Misinformation effects", "Correction effects") |>
    factor(c("Misinformation effects", "Correction effects"))
}

# Personality traits occupy the lower facet row, so their bracket offsets shift.
add_facets <- function(data, term_column) {
  data |>
    mutate(
      name = label_trait(name),
      rfac = label_row(as.character(label_trait(name))),
      cfac = label_column(.data[[term_column]]),
      yint = if_else(rfac == "Personality\ntraits", -4, 0)
    )
}

meta_df <- read_fit("meta_trait_cates_w1_all_adj") |> add_facets("type2")
studies_df <- read_fit("trait_cates_w1_all_adj") |> add_facets("term")

brackets <- read_fit("meta_trait_dic_w1_all_adj") |>
  add_facets("term") |>
  filter(p.value < .05) |>
  left_join(
    meta_df |>
      group_by(name, type2, rfac, cfac) |>
      summarize(xlo = min(estimate), xhi = max(estimate), .groups = "drop"),
    by = c("name", "type2", "rfac", "cfac")
  ) |>
  mutate(mid = (xhi + xlo) / 2, slab = stars(p.value))

scale_labels <- tibble(
  x = c(-6.6, -13.3), y = c(5.8, 5.8), lab = c("Low on\nscale", "High on\nscale")
) |>
  mutate(
    cfac = factor(levels(meta_df$cfac)[2], levels(meta_df$cfac)),
    rfac = factor("Personality\ntraits", levels(meta_df$rfac))
  )

g <- studies_df |>
  ggplot() +
  geom_vline(aes(xintercept = 0), linetype = "dashed", linewidth = .25) +
  geom_quasirandom(
    aes(estimate, name, fill = value, group = value),
    shape = 21, bandwidth = .0005, size = .65,
    orientation = "y", dodge.width = .6, data = studies_df
  ) +
  geom_quasirandom(
    aes(estimate, name, color = value, group = value),
    bandwidth = .0005, size = .4,
    orientation = "y", dodge.width = .6, data = studies_df
  ) +
  geom_segment(
    aes(x = xlo, xend = xhi,
        y = as.numeric(name) + yint + .5, yend = as.numeric(name) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_segment(
    aes(x = xlo, xend = xlo, y = as.numeric(name) + yint,
        yend = as.numeric(name) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_segment(
    aes(x = xhi, xend = xhi, y = as.numeric(name) + yint,
        yend = as.numeric(name) + yint + .5),
    data = brackets, linewidth = .25, linetype = "dotted"
  ) +
  geom_label(
    aes(x = mid, label = slab, y = as.numeric(name) + .465 + yint),
    data = brackets, fill = "grey98", linewidth = 0, fontface = "bold", size = 3,
    label.padding = unit(0.125, "lines")
  ) +
  geom_point(
    aes(x = estimate, y = name, fill = value),
    data = meta_df, shape = 21, size = 5.5, position = position_dodge(.65)
  ) +
  geom_linerange(
    aes(xmin = conf.low - .05, xmax = conf.high + .05, y = name, group = value),
    linewidth = .8, color = "black", data = meta_df, position = position_dodge(.65)
  ) +
  geom_linerange(
    aes(xmin = conf.low, xmax = conf.high, y = name, color = value),
    data = meta_df, position = position_dodge(.65)
  ) +
  geom_point(
    aes(x = estimate, y = name, fill = value),
    shape = 21, data = meta_df, size = 5.4, position = position_dodge(.65)
  ) +
  geom_text(
    aes(estimate, name, label = round(estimate, 1), group = value),
    size = 1.4, color = "black", position = position_dodge(.65), data = meta_df
  ) +
  geom_text(aes(x, y, label = lab), data = scale_labels,
            size = 2.5, lineheight = .7, fontface = "italic") +
  facet_grid(rfac ~ cfac, scales = "free", space = "free") +
  scale_fill_grey(start = .6, end = .99) +
  scale_color_grey(start = .6, end = .99) +
  scale_y_discrete(
    expand = expansion(add = c(.75, 1.2)),
    breaks = levels(meta_df$name),
    labels = levels(meta_df$name) |> str_remove("Personality - ")
  ) +
  scale_x_continuous(breaks = seq(-30, 30, 10)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme_tw

save_figure(g, meta_df, "figure_5_heterogeneity_by_trait", width = 9, height = 7)
write_csv(brackets, file.path(out_dir, "figure_5_trait_difference_brackets.csv"))
