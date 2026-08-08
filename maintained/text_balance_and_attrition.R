# coppock_etal_2023/maintained/text_balance_and_attrition.R
# Output: output/text_balance_and_attrition.csv
# Depends on: helpers.R, fit_balance_and_attrition.R output
# Description: The counts appendix sections 4 and 5 report in prose: how many
#   balance and attrition tests were run, how many reached significance, and how
#   many survive a Benjamini-Hochberg correction. The deposited scripts for those
#   figures print these counts to the console and have every ggsave commented
#   out, so nothing they produce reaches a file.

source(here::here("maintained", "helpers.R"))

balance_regs <- read_fit("balance_estimates")
joint_balance <- read_fit("joint_balance_tests")
attrition_regs <- read_fit("attrition_estimates")

claims <- tribble(
  ~claim, ~value,
  "Balance regressions run", nrow(balance_regs),
  "Balance regressions with raw p < 0.05", sum(balance_regs$p.value <= 0.05),
  "Balance regressions with raw p < 0.05, per cent",
    100 * mean(balance_regs$p.value <= 0.05),
  "Balance regressions significant after Benjamini-Hochberg",
    sum(p.adjust(balance_regs$p.value, method = "BH") <= 0.05),
  "Joint balance tests run", nrow(joint_balance),
  "Joint balance tests with raw p < 0.05", sum(joint_balance$raw <= 0.05),
  "Joint balance tests significant after Benjamini-Hochberg",
    sum(joint_balance$adjusted <= 0.05),
  "Attrition tests run", nrow(attrition_regs),
  "Attrition tests with raw p < 0.05", sum(attrition_regs$p.value <= 0.05),
  "Attrition tests significant after Benjamini-Hochberg",
    sum(p.adjust(attrition_regs$p.value, method = "BH") <= 0.05)
)

write_csv(claims, file.path(out_dir, "text_balance_and_attrition.csv"))
print(claims, n = nrow(claims))
