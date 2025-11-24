df <- read.csv("data/med_data_tidy.csv")

library(dplyr)
library(ggplot2)

# write sex in the correcet order.
df <- df %>%
  mutate(sex = factor(sex, levels = c("female", "male")))

# subset with cancer-patienter
df_cancer <- df %>% filter(diag_class == "Cancer")

# sex summary.
summarise_by_sex <- function(data) {
  data %>%
    group_by(sex) %>%
    summarise(
      n = n(),
      death_rate = mean(death, na.rm = TRUE),
      survival_rate = 1 - death_rate,
      se = sqrt(death_rate * (1 - death_rate) / n),
      CI_lower = death_rate - 2 * se,
      CI_upper = death_rate + 2 * se,
      .groups = "drop"
    )
}

# simple stats.
total_stats  <- summarise_by_sex(df)
cancer_stats <- summarise_by_sex(df_cancer)

# we plot the survival rate for total group
ggplot(df, aes(x = sex, fill = factor(death))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Death (1 = died)",
    title = "Total Survival by Sex"
  )

# we plot the survival rate for cancer sub_group
ggplot(df_cancer, aes(x = sex, fill = factor(death))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Death (1 = died)",
    title = "Cancer Survival by Sex"
  )

##perform a chi square test to get p_value.
chi_total  <- chisq.test(table(df$sex, df$death))
chi_cancer <- chisq.test(table(df_cancer$sex, df_cancer$death))

chi_total
chi_cancer

#logistic regression to get r^2 for both cases.
model_total  <- glm(death ~ sex, data = df,        family = binomial)
model_cancer <- glm(death ~ sex, data = df_cancer, family = binomial)

pseudoR2_total  <- 1 - model_total$deviance  / model_total$null.deviance
pseudoR2_cancer <- 1 - model_cancer$deviance / model_cancer$null.deviance

pseudoR2_total
pseudoR2_cancer

# Table to format results nicely.
combined_table <- tibble::tibble(
  Analysis        = c("All patients", "Cancer patients"),
  Chi_square_p    = c(chi_total$p.value, chi_cancer$p.value),
  Pseudo_R2       = c(pseudoR2_total,   pseudoR2_cancer),
  Male_death_rate = c(
    total_stats$death_rate[total_stats$sex == "male"],
    cancer_stats$death_rate[cancer_stats$sex == "male"]
  ),
  Female_death_rate = c(
    total_stats$death_rate[total_stats$sex == "female"],
    cancer_stats$death_rate[cancer_stats$sex == "female"]
  ),
  SE_male = c(
    total_stats$se[total_stats$sex == "male"],
    cancer_stats$se[cancer_stats$sex == "male"]
  ),
  SE_female = c(
    total_stats$se[total_stats$sex == "female"],
    cancer_stats$se[cancer_stats$sex == "female"]
  ),
  N_male = c(
    total_stats$n[total_stats$sex == "male"],
    cancer_stats$n[cancer_stats$sex == "male"]
  ),
  N_female = c(
    total_stats$n[total_stats$sex == "female"],
    cancer_stats$n[cancer_stats$sex == "female"]
  )
)

combined_table
