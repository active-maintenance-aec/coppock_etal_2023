# coppock_etal_2023/maintained/figure_a7_joint_balance_test.R
# Output: output/figure_a7_joint_balance_test.pdf/.png/.csv
# Depends on: helpers.R, fit_balance_and_attrition.R output
# Description: Appendix Figure 7. The distribution of p-values from a joint
#   multinomial test of whether the pre-treatment covariates together predict
#   condition, before and after a Benjamini-Hochberg correction.

source(here::here("maintained", "helpers.R"))

gg_df <- read_fit("joint_balance_tests") |>
  pivot_longer(cols = c(raw, adjusted)) |>
  mutate(name = factor(name, c("raw", "adjusted")))

g <- gg_df |>
  ggplot(aes(value)) +
  geom_histogram(binwidth = 0.05) +
  facet_wrap(~name) +
  theme_bw() +
  theme(strip.background = element_blank()) +
  labs(x = "p.value of joint test", y = "Count of fact checks")

save_figure(g, gg_df, "figure_a7_joint_balance_test", width = 6.5, height = 4)

print(gg_df |> group_by(name) |> summarize(below_05 = sum(value <= 0.05), n = n()))
