library(tidyverse)
library(fixest)
library(modelsummary)
library(broom)
library(scales)
library(gt)

# -----------------------------
# 1. Load data
# -----------------------------
baseline  <- read_csv("baseline.csv", show_col_types = FALSE)
treatment <- read_csv("treatment.csv", show_col_types = FALSE)
endline   <- read_csv("endline.csv", show_col_types = FALSE)

# -----------------------------
# 2. Merge data
# -----------------------------
df_full <- baseline %>%
  left_join(treatment, by = "key")

df <- df_full %>%
  inner_join(endline, by = "key")

# -----------------------------
# 3. Create treatment variables
# -----------------------------
df <- df %>%
  mutate(
    Loan   = as.integer(treatment == 1),
    InKind = as.integer(treatment == 2),
    Cash   = as.integer(treatment == 3)
  )

df_full <- df_full %>%
  mutate(
    Loan   = as.integer(treatment == 1),
    InKind = as.integer(treatment == 2),
    Cash   = as.integer(treatment == 3),
    in_endline = as.integer(key %in% df$key)
  )

# -----------------------------
# 4. Basic sample checks
# -----------------------------
cat("Baseline sample size:", nrow(baseline), "\n")
cat("Treatment file size:", nrow(treatment), "\n")
cat("Endline file size:", nrow(endline), "\n")
cat("Merged analysis sample size:", nrow(df), "\n\n")

cat("Treatment counts in analysis sample:\n")
print(table(df$treatment))

cat("\nTreatment shares in analysis sample:\n")
print(prop.table(table(df$treatment)))

# -----------------------------
# 5. Attrition check
# -----------------------------
attrition_model <- feols(in_endline ~ Loan + InKind + Cash, data = df_full, vcov = "hetero")
summary(attrition_model)

attrition_summary <- df_full %>%
  group_by(treatment) %>%
  summarise(
    n_baseline = n(),
    n_endline = sum(in_endline, na.rm = TRUE),
    attrition_rate = 1 - mean(in_endline, na.rm = TRUE),
    .groups = "drop"
  )

print(attrition_summary)

# -----------------------------
# 6. Balance table setup
# -----------------------------
balance_vars <- c(
  "age",
  "female",
  "educ_college",
  "prior_work",
  "has_business",
  "low_income",
  "prior_borrowing",
  "family_pressure_index"
)

# Group means
balance_means <- df %>%
  mutate(
    treatment_label = case_when(
      treatment == 0 ~ "Control",
      treatment == 1 ~ "Loan",
      treatment == 2 ~ "In-kind grant",
      treatment == 3 ~ "Cash grant"
    )
  ) %>%
  group_by(treatment_label) %>%
  summarise(across(all_of(balance_vars), ~ mean(.x, na.rm = TRUE)), .groups = "drop") %>%
  pivot_longer(-treatment_label, names_to = "variable", values_to = "mean") %>%
  pivot_wider(names_from = treatment_label, values_from = mean)

# Optional balance regressions
balance_models <- lapply(balance_vars, function(v) {
  feols(as.formula(paste0(v, " ~ Loan + InKind + Cash")), data = df, vcov = "hetero")
})
names(balance_models) <- balance_vars

# Joint p-values using base lm + overall F-test
balance_pvals <- lapply(balance_vars, function(v) {
  dat <- df %>%
    select(all_of(v), treatment) %>%
    drop_na()
  
  mod <- lm(as.formula(paste0(v, " ~ factor(treatment)")), data = dat)
  pval <- anova(mod)[1, "Pr(>F)"]
  
  tibble(variable = v, p_value = pval)
}) %>%
  bind_rows()

balance_table_out <- balance_means %>%
  left_join(balance_pvals, by = "variable") %>%
  mutate(
    variable = recode(
      variable,
      age = "Age",
      female = "Female",
      educ_college = "Completed college",
      prior_work = "Worked before program",
      has_business = "Had business at baseline",
      low_income = "Low-income household",
      prior_borrowing = "Borrowed before",
      family_pressure_index = "Family pressure index"
    )
  ) %>%
  select(variable, Control, Loan, `In-kind grant`, `Cash grant`, p_value)

print(balance_table_out)

# -----------------------------
# 7. Main effects
# -----------------------------
m1 <- feols(labor_income ~ Loan + InKind + Cash | cohort, data = df, vcov = "hetero")
m2 <- feols(has_business_endline ~ Loan + InKind + Cash | cohort, data = df, vcov = "hetero")

etable(m1, m2)

main_coefs <- bind_rows(
  tidy(m1) %>% mutate(model = "labor_income"),
  tidy(m2) %>% mutate(model = "has_business_endline")
)

print(main_coefs)

# -----------------------------
# 8. Cost-adjusted comparison
# -----------------------------
budget <- 1000000
cost_loan <- 350
cost_grant <- 900

coef_m1 <- tidy(m1) %>%
  filter(term %in% c("Loan", "InKind", "Cash")) %>%
  select(term, estimate, std.error, p.value)

cost_effect_df <- coef_m1 %>%
  mutate(
    cost_per_person = case_when(
      term == "Loan" ~ cost_loan,
      term %in% c("InKind", "Cash") ~ cost_grant
    ),
    people_served = budget / cost_per_person,
    total_monthly_income_gain = estimate * people_served,
    label = case_when(
      term == "Loan" ~ "Loan",
      term == "InKind" ~ "In-kind grant",
      term == "Cash" ~ "Cash grant"
    )
  )

print(cost_effect_df)

# -----------------------------
# 9. Heterogeneity regressions
# -----------------------------
h1 <- feols(
  labor_income ~ Loan * has_business + InKind * has_business + Cash * has_business | cohort,
  data = df,
  vcov = "hetero"
)

h2 <- feols(
  has_business_endline ~ Loan * has_business + InKind * has_business + Cash * has_business | cohort,
  data = df,
  vcov = "hetero"
)

etable(h1, h2)

hetero_coefs <- bind_rows(
  tidy(h1) %>% mutate(model = "labor_income"),
  tidy(h2) %>% mutate(model = "has_business_endline")
)

print(hetero_coefs)

# Subgroup effects for interpretation
coef_h1 <- coef(h1)

loan_no_biz    <- coef_h1["Loan"]
loan_with_biz  <- coef_h1["Loan"] + coef_h1["Loan:has_business"]

inkind_no_biz   <- coef_h1["InKind"]
inkind_with_biz <- coef_h1["InKind"] + coef_h1["has_business:InKind"]

cash_no_biz   <- coef_h1["Cash"]
cash_with_biz <- coef_h1["Cash"] + coef_h1["has_business:Cash"]

subgroup_effects <- tibble(
  treatment = c("Loan", "Loan", "In-kind grant", "In-kind grant", "Cash grant", "Cash grant"),
  subgroup = c("No baseline business", "Has baseline business",
               "No baseline business", "Has baseline business",
               "No baseline business", "Has baseline business"),
  labor_income_effect = c(
    loan_no_biz, loan_with_biz,
    inkind_no_biz, inkind_with_biz,
    cash_no_biz, cash_with_biz
  )
)

print(subgroup_effects)

# -----------------------------
# 10. Appendix Table 1
# -----------------------------
table1_gt <- balance_table_out %>%
  gt() %>%
  tab_header(
    title = "Table 1. Baseline balance across treatment arms"
  ) %>%
  fmt_number(
    columns = c(Control, Loan, `In-kind grant`, `Cash grant`),
    decimals = 3
  ) %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    variable = "",
    Control = "Control",
    Loan = "Loan",
    `In-kind grant` = "In-kind grant",
    `Cash grant` = "Cash grant",
    p_value = "Joint p-value"
  ) %>%
  tab_source_note(
    source_note = html(
      "Notes: Entries are group means in the merged analysis sample.<br>Joint p-values come from F-tests across treatment arms."
    )
  ) %>%
  tab_options(
    table.font.size = px(14),
    source_notes.font.size = px(11)
  )

table1_gt
gtsave(table1_gt, "table1_balance.html")

# -----------------------------
# 11. Appendix Table 2
# -----------------------------
table2_gt <- modelsummary(
  list(
    "Labor income" = m1,
    "Has business at endline" = m2
  ),
  stars = TRUE,
  coef_map = c(
    "Loan" = "Loan",
    "InKind" = "In-kind grant",
    "Cash" = "Cash grant"
  ),
  gof_map = tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  add_rows = tibble::tribble(
    ~term, ~`Labor income`, ~`Has business at endline`,
    "Cohort fixed effects", "Yes", "Yes"
  ),
  title = "Table 2. Main treatment effects on labor income and business ownership",
  output = "gt"
)

table2_gt <- table2_gt %>%
  tab_source_note(
    source_note = "Notes: All regressions include cohort fixed effects. The omitted group is the control group. Robust standard errors are in parentheses."
  ) %>%
  tab_options(
    table.font.size = px(13),
    source_notes.font.size = px(11)
  )

table2_gt
gtsave(table2_gt, "table2_main_results.html")

# -----------------------------
# 12. Appendix Table 3
# -----------------------------
table3_gt <- modelsummary(
  list(
    "Labor income" = h1,
    "Has business at endline" = h2
  ),
  stars = TRUE,
  coef_map = c(
    "Loan" = "Loan",
    "InKind" = "In-kind grant",
    "Cash" = "Cash grant",
    "has_business" = "Had business at baseline",
    "Loan:has_business" = "Loan × Had business",
    "has_business:InKind" = "In-kind × Had business",
    "has_business:Cash" = "Cash × Had business"
  ),
  gof_map = tribble(
    ~raw, ~clean, ~fmt,
    "nobs", "Observations", 0,
    "r.squared", "R-squared", 3
  ),
  add_rows = tibble::tribble(
    ~term, ~`Labor income`, ~`Has business at endline`,
    "Cohort fixed effects", "Yes", "Yes"
  ),
  title = "Table 3. Heterogeneous effects by baseline business ownership",
  output = "gt"
)

table3_gt <- table3_gt %>%
  tab_source_note(
    source_note = html(
      "Notes: All regressions include cohort fixed effects and interactions between treatment indicators and baseline business ownership.<br>The omitted group is the control group with no baseline business. Robust standard errors are in parentheses."
    )
  ) %>%
  tab_options(
    table.font.size = px(13),
    source_notes.font.size = px(11)
  )

table3_gt
gtsave(table3_gt, "table3_heterogeneity.html")

# -----------------------------
# 13. Figure 1
# -----------------------------
fig_df <- cost_effect_df %>%
  mutate(
    label = factor(label, levels = c("Cash grant", "In-kind grant", "Loan")),
    value_label = comma(round(total_monthly_income_gain, 0))
  )

figure1 <- ggplot(fig_df, aes(x = label, y = total_monthly_income_gain)) +
  geom_col(width = 0.65) +
  coord_flip() +
  geom_text(aes(label = value_label), hjust = -0.1, size = 4.2) +
  scale_y_continuous(
    labels = comma,
    expand = expansion(mult = c(0, 0.18))
  ) +
  labs(
    title = "A $1 million budget goes furthest with subsidized loans",
    subtitle = "Estimated additional monthly labor income generated by each product",
    x = NULL,
    y = "Additional monthly labor income (EGP)",
    caption = "Loans serve about 2,850 people; grants serve about 1,100."
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold"),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

figure1
ggsave("figure1_budget_comparison.png", figure1, width = 9, height = 5.8, dpi = 300)

# -----------------------------
# 14. Optional backup exports
# -----------------------------
write_csv(balance_table_out, "table1_balance.csv")
write_csv(main_coefs, "main_coefs.csv")
write_csv(cost_effect_df, "cost_effect_df.csv")
write_csv(subgroup_effects, "subgroup_effects.csv")

