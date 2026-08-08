# coppock_etal_2023/maintained/figures_a1_a2_time_with_treatments.R
# Output: output/figure_a1_time_with_treatments.pdf/.png/.csv,
#   output/figure_a2_time_with_treatments_by_type.pdf/.png/.csv,
#   output/text_time_ranking_check.csv
# Depends on: helpers.R, fit_all_models.R output, original/data/all_panels_long.rds
# Description: Appendix Figures 1 and 2. Average seconds spent with the
#   misinformation and the fact-check treatments, for the top and bottom tercile
#   of each trait and for each party, overall and split by whether the
#   misinformation arrived as a social media post or as an article. The two
#   figures come from one set of per-panel regressions pooled two ways, so they
#   share a script.
#
#   The script also tests the main text's claim that the within-trait ranking of
#   correction effects in Figure 5 is matched exactly by the within-trait ranking
#   of time spent with the fact check.

source(here::here("maintained", "helpers.R"))

all_panels_long <- read_rds(file.path(data_dir, "all_panels_long.rds")) |>
  mutate(timing = coalesce(correction_page_timer, misinfo_page_timer))

# The deposit names political knowledge in the labels but never pivots it into
# the long frame, so the published figure has nine rows and no political
# knowledge row. That omission is preserved: adding the trait would add a row
# the published figure does not have.
group_levels <- c(
  lucid_pid_3 = "Party Affiliation",
  grp_polint = "Political interest",
  grp_crt = "Cognitive Reflection Test",
  grp_nfc = "Need for Cognition",
  grp_extraversion = "Personality - Extraversion",
  grp_agree = "Personality - Agreeableness",
  grp_conscientiousness = "Personality - Conscientiousness",
  grp_emotstab = "Personality - Emotional Stability",
  grp_openness = "Personality - Openness"
)

timings <- all_panels_long |>
  pivot_longer(cols = all_of(names(group_levels))) |>
  filter(!is.na(value), value != "med", value != "Independent", treatment != "control") |>
  mutate(name = factor(unname(group_levels[name]), unname(group_levels))) |>
  group_by(name, value, treatment, fc, misinfo_type, panel, panel_factor) |>
  reframe(tidy(lm_robust(timing ~ 1, data = pick(everything()))))

gg_df <- timings |>
  group_by(name, value, treatment) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
               conf.int = TRUE))

# The tercile labels are written directly beside the points rather than into a
# legend, so only one facet carries them.
plot_timings <- function(data, label_data, label_x, facets) {
  ggplot(data, aes(estimate, name, group = value, shape = value)) +
    geom_point(position = position_dodge(width = .3)) +
    geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                   position = position_dodge(width = .3)) +
    geom_text(aes(label = value, x = label_x), hjust = 0, size = 2,
              position = position_dodge(width = .4), data = label_data) +
    facets +
    theme_bw() +
    theme(axis.title.y = element_blank(), legend.position = "none",
          strip.background = element_blank()) +
    labs(x = "Average time spent with treatments, in seconds")
}

g1 <- plot_timings(gg_df, gg_df |> filter(treatment == "misinformation"), 55,
                   facet_wrap(~treatment))

save_figure(g1, gg_df, "figure_a1_time_with_treatments", width = 6.5, height = 3.5)

by_type_df <- timings |>
  group_by(name, value, treatment, misinfo_type) |>
  reframe(tidy(rma.uni(yi = estimate, sei = std.error, data = pick(everything())),
               conf.int = TRUE))

g2 <- plot_timings(
  by_type_df,
  by_type_df |> filter(treatment == "misinformation",
                       misinfo_type == "Misinformation is a social media post"),
  65,
  facet_grid(misinfo_type ~ treatment)
)

save_figure(g2, by_type_df, "figure_a2_time_with_treatments_by_type", width = 6.5, height = 6.5)

# Does the ranking of correction effects match the ranking of time spent? ----
# For each trait the question is whether the tercile with the larger fact-check
# effect is also the tercile that spent longer with the fact check. The effect is
# negative, so "larger effect" means the more negative pooled estimate. Party
# affiliation appears in the time figure but not in Figure 5, and political
# knowledge appears in Figure 5 but not in the time figure, so the eight traits
# both figures carry are what the claim can be tested on.
trait_labels <- c(
  grp_polint = "Political interest",
  grp_crt = "Cognitive Reflection Test",
  grp_nfc = "Need for Cognition",
  grp_extraversion = "Personality - Extraversion",
  grp_agree = "Personality - Agreeableness",
  grp_conscientiousness = "Personality - Conscientiousness",
  grp_emotstab = "Personality - Emotional Stability",
  grp_openness = "Personality - Openness"
)

stronger_effect <- read_fit("meta_trait_cates_w1_all_adj") |>
  filter(type2 == "treatmentfactcheck", name %in% names(trait_labels)) |>
  group_by(name) |>
  summarize(stronger_effect = value[which.min(estimate)], .groups = "drop")

longer_reading <- gg_df |>
  filter(treatment == "factcheck", as.character(name) %in% unname(trait_labels)) |>
  mutate(name = names(trait_labels)[match(as.character(name), unname(trait_labels))]) |>
  group_by(name) |>
  summarize(longer_reading = value[which.max(estimate)], .groups = "drop")

ranking_check <- stronger_effect |>
  inner_join(longer_reading, by = "name") |>
  mutate(agrees = stronger_effect == longer_reading)

stopifnot(nrow(ranking_check) == length(trait_labels))

write_csv(ranking_check, file.path(out_dir, "text_time_ranking_check.csv"))
print(ranking_check)
