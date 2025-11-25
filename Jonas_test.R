df <- read.csv("data/med_data_tidy.csv")

library(dplyr)
library(ggplot2)
library(tibble)
library(here)
# write sex in the correct order.
df <- df |>
  mutate(sex = factor(sex, levels = c("female", "male")))

# subset with cancer-patienter
df_cancer <- df |> filter(diag_class == "Cancer")

df
# sex summary.
summarise_by_sex <- function(data) {
  data |>
    group_by(sex) |>
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
total_stats
cancer_stats

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

## perform a chi square test to get p_value.
chi_total  <- chisq.test(table(df$sex, df$death))
chi_cancer <- chisq.test(table(df_cancer$sex, df_cancer$death))

chi_total
chi_cancer

# linear models to get R^2 for both cases.
model_total  <- lm(death ~ sex, data = df)
model_cancer <- lm(death ~ sex, data = df_cancer)

R2_total  <- summary(model_total)$r.squared
R2_cancer <- summary(model_cancer)$r.squared

R2_total
R2_cancer

# Table to format results nicely.
combined_table <- tibble::tibble(
  Analysis        = c("All patients", "Cancer patients"),
  Chi_square_p    = c(chi_total$p.value, chi_cancer$p.value),
  R2              = c(R2_total,       R2_cancer),
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

DF_result <- tibble(
  Analysis = c("All patients", "Cancer patients"),
  models   = list(model_total, model_cancer)
) |>
  left_join(combined_table, by = "Analysis") |>
  mutate(variable = "sex", .before = 1)

DF_result

DF_result$models[[1]]  # total
DF_result$models[[2]]  # cancer



# we wish to create a format that follow the other group members.

# we wish to create a linear model that binds male = 1 and female = 0
# and then create a linear model of the survival rate
# for men and female accordingly.

df <- df |>
  mutate(
    male_binary = ifelse(sex == "male", 1, 0),
    survival = 1 - death
  )

female_model <- lm(survival ~ 1, data = df |> filter(sex == "female"))
male_model   <- lm(survival ~ 1, data = df |> filter(sex == "male"))

model_df <- tibble(
  group = c("female", "male"),
  model = list(female_model, male_model)
)

model_df

summary(model_df$model[[1]])
summary(model_df$model[[2]])


saveRDS(model_df, here("data", "models", "sex.RDS"))
readRDS(here("data", "models", "sex.RDS"))

