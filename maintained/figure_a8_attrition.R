# coppock_etal_2023/maintained/figure_a8_attrition.R
# Output: output/figure_a8_attrition.pdf/.png/.csv
# Depends on: helpers.R, fit_balance_and_attrition.R output
# Description: Appendix Figure 8. The estimated effect of each condition on
#   whether a respondent answered the outcome question, at each of the three
#   waves, one row of panels per panel and fact check.

source(here::here("maintained", "helpers.R"))

gg_df <- read_fit("attrition_estimates") |>
  mutate(significant = p.value <= 0.05)

g <- gg_df |>
  ggplot(aes(estimate, term, group = fc, shape = significant, color = significant)) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_point(position = position_dodge(width = 0.5)) +
  geom_linerange(aes(xmin = conf.low, xmax = conf.high),
                 position = position_dodge(width = 0.5)) +
  scale_color_manual(values = c(`FALSE` = gray(0.5), `TRUE` = gray(0.1))) +
  facet_grid(panel_factor ~ outcome) +
  theme_bw() +
  theme(strip.background = element_blank(), axis.title.y = element_blank(),
        legend.position = "none") +
  labs(x = "Estimated effect on response, relative to the misinformation condition")

save_figure(g, gg_df, "figure_a8_attrition", width = 6.5, height = 9)

print(gg_df |> count(significant))
