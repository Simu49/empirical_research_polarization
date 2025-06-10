# Social Media and Political Polarization in Germany

This repository contains the R code and documentation for a group project at the University of Bern (Spring Semester 2024). The project analyzes the relationship between social media usage and political polarization in Germany using panel data from the German Longitudinal Election Study (GLES).

## Project Summary

We used panel data from GLES waves 1–9 and 19 to examine how social media activity relates to shifts in political polarization over time. The analysis involved data wrangling, visualization, and panel regression models to estimate the effect of social media engagement on individual political attitudes.

### Key Steps:
- Data cleaning and merging of multiple GLES waves
- Construction of polarization and online participation indices
- Visualization of trends in social media use and polarization
- Panel regression modeling (pooled OLS, fixed effects, random effects)
- Subgroup analyses by political orientation (left, center, right)

## Research Questions

- Does frequent social media use increase political polarization?
- How do online participation and demographic factors influence polarization?
- Are effects consistent across different political orientations?

## Main Findings

- Social media usage and political polarization both show variation across survey waves.
- Fixed effects panel models suggest a relationship between social media activity and changes in polarization, robust to demographic controls.
- Patterns differ by political orientation subgroup.

## Files Included

| File / Folder | Description |
|---------------|-------------|
| `Polarization.R` | Main R script for data wrangling, visualization, and modeling |
| `Empirical_research_paper_Kellner_Schlaepfer_Bernhard.pdf` | Research paper |
| `Summary_research_paper_Kellner_Schlaepfer_Bernhard.pdf` | Short summary of the paper |
| `README.md` | Project overview and instructions |

## References

- German Longitudinal Election Study (GLES)
- R packages: `tidyverse`, `plm`, `stargazer`, `ggpubr`, `psych`, `lmtest`, `panelr`, `hrbrthemes`, `scales`

## Authors

- **Simon Bernhard** | simon.bernhard@students.unibe.ch
- **Manuel Kellner** | manuel.kellner@students.unibe.ch
- **Kevin Jan Schläpfer** | kevin.schlaepfer@students.unibe.ch

University of Bern, Department of Social Sciences
