## PlantGrowth one-way ANOVA
## Author: Harneyyer Clores


library(tidyverse)
library(car)
library(effectsize)

## Load built-in dataset
data("PlantGrowth")

## Descriptive statistics
PlantGrowth %>%
  group_by(group) %>%
  summarise(
    n = n(),
    mean_weight = mean(weight),
    sd_weight = sd(weight)
  )

## Boxplot (check outliers)
ggplot(PlantGrowth, aes(x = group, y = weight)) +
  geom_boxplot(outlier.colour = NA) +
  geom_jitter(width = 0.1, alpha = 0.7) +
  labs(x = "Treatment group", y = "Dried weight") +
  theme_minimal()

## Shapiro-Wilk for normality
by(PlantGrowth$weight, PlantGrowth$group, shapiro.test)

## Levene’s test for equal variances
leveneTest(weight ~ group, data = PlantGrowth)

## One-way ANOVA
model <- aov(weight ~ group, data = PlantGrowth)
summary(model)

## Effect size
eta_squared(model, partial = TRUE)

## Tukey post hoc
TukeyHSD(model)

## APA-style summary printed automatically
anova_table <- summary(model)[[1]]
F_val <- anova_table$`F value`[1]
p_val <- anova_table$`Pr(>F)`[1]
df1 <- anova_table$Df[1]
df2 <- anova_table$Df[2]
eta2 <- eta_squared(model, partial=TRUE)$Eta2_partial[1]

cat(sprintf(
  "One-way ANOVA: F(%d, %d) = %.2f, p = %.4f, partial eta^2 = %.2f\n",
  df1, df2, F_val, p_val, eta2
))
