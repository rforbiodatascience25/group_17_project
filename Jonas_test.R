df <- read.csv("data/med_data_tidy.csv")
head(df)

#finding the mortality and survival rate for each of the sex
df$sex <- factor(df$sex, levels = c("female", "male"))
library(dplyr)

total_survival <- df %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    deaths = sum(death, na.rm = TRUE),
    mortality_rate = mean(death, na.rm = TRUE),
    survival_rate = 1 - mortality_rate
  )
total_survival

#plotting the total survival by sex
library(ggplot2)
ggplot(df, aes(x = sex, fill = factor(death))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Death (1 = died)",
    title = "Total Survival by Sex"
  )


#subset of cancer patients:
df_cancer <- df %>% filter(diag_class == "Cancer")
cancer_survival <- df_cancer %>%
  group_by(sex) %>%
  summarise(
    n = n(),
    deaths = sum(death, na.rm = TRUE),
    mortality_rate = mean(death, na.rm = TRUE),
    survival_rate = 1 - mortality_rate
  )
cancer_survival

#barplot of the cancer survival
ggplot(df_cancer, aes(x = sex, fill = factor(death))) +
  geom_bar(position = "fill") +
  labs(
    y = "Proportion",
    fill = "Death (1 = died)",
    title = "Cancer Survival by Sex"
  )


#pvalue
#we can perform a chi square test to see the mortality rate
chisq.test(table(df$sex, df$death))
chisq.test(table(df_cancer$sex, df_cancer$death))
