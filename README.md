# APM1111 – CLORES & ESPIRITU – One-Way ANOVA (PlantGrowth)

This repository contains all required files for our one-way ANOVA analysis of the PlantGrowth dataset in R.

## 📁 Files Included
- **PlantGrowth_ANOVA.R** – Full R script used for descriptive statistics, assumption checking, ANOVA, Tukey HSD, and effect size.
- **CLORES-ESPIRITU-APM1111-FA8.docx** – Complete APA-style report including screenshots of all statistical outputs and the boxplot.

## 📌 Summary of Findings
A one-way ANOVA showed a significant effect of treatment group on plant weight:

**F(2, 27) = 4.85, p = .016, partial η² = .26**

Tukey HSD indicated:
- trt2 > trt1 (p = .012, significant)
- ctrl vs trt1: ns
- ctrl vs trt2: ns

## 👤 Authors
- **Harneyyer Clores**
- **Joseph Espiritu**

## 🛠 Tools Used
- RStudio 2025
- R version 4.5.2
- tidyverse, car, effectsize packages

