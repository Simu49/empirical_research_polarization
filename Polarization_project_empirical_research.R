# Political Polarization - group project by Simon Bernhard, Manuel Kellner, Kevin Jan Schläpfer
# Training in Empirical Research
# Departement of social sciences
# Institute of political science
# University of Bern
# Spring semester 2024

# Preparation -------------------------------------------------------------
# setwd("your working directory") # set working directory

# load necessary libraries

library(haven)
library(tidyverse)
library(labelled)
library(plm)
library(stargazer)
library(ggpubr)
library(psych)
library(lmtest)
library(panelr)
library(hrbrthemes)
library(scales)


GLES1_to_9 <- read_dta("ZA6838_w1to9_sA_v6-0-0.dta") # read dataset with the waves 1 to 9

GLES19 <- read_dta("ZA6838_w19_sA_v6-0-0.dta") # read dataset of the wave 19

# Create dataset -------------------------------------------------
dflist <- list(GLES1_to_9, GLES19)

panel <- dflist %>%
  reduce(left_join, by = "lfdn") # merge dataframes

# Tidy dataset ------------------------------------------------------------
panel <- panel %>%
  select(
    "lfdn", "kpx_2280.x", "kpx_2290s.x", "kp1_2320", "kp9_2320",
    matches("^(kp4_|kp7_|kp19_)(1500|1610|1616|010)"),
    -ends_with("flag")
  ) # only keep relevant variables

panel <- panel %>%
  filter(if_all(everything(), ~ . >= 0)) # exclude all missing values (e.g. coded as -97)

Panel_long <- panel %>% # format Panel data to long format
  pivot_longer(
    cols = -c(lfdn, kpx_2280.x, kpx_2290s.x, kp1_2320, kp9_2320),
    names_to = c("wave", ".value"),
    names_sep = "_"
  )

variables_with_labels = map(Panel_long, function(x) attr(x, "class") == "haven_labelled") %>% # extract all variable names to a vector to change variabla types from haven_labelled (double labelled) to factor
  names()

variables_with_labels_to_labels <- variables_with_labels[c(3, 6, 9:12)] # variables with labels as relevant values
variables_with_labels_to_values <- variables_with_labels[c(1, 2, 4, 5, 7, 8, 13)] # variables with values as relevant values

Panel_long_factors <- Panel_long %>% # change Variable types to factors
  mutate_at(all_of(variables_with_labels_to_labels), as_factor) %>%
  mutate_at(all_of(variables_with_labels_to_values), as_factor, "values")

Panel_long_factors <- Panel_long_factors %>%
  mutate(wave = as.Date(paste0(ifelse(wave == "kp4", "2017-07",
    ifelse(wave == "kp7", "2017-09",
      ifelse(wave == "kp19", "2021-09", wave)
    )
  ), "-01"))) # format wave as date

Panel_long_factors <- Panel_long_factors %>%
  rename(
    sex = "kpx_2280.x", birthyear = "kpx_2290s.x", edu1 = "kp1_2320", edu2 = "kp9_2320",
    pol_interest = "010", left_right_self = "1500", soc_med_pol = "1610",
    online_part_like_com = "1616a", online_part_shared = "1616b",
    online_part_post = "1616c", online_part_none = "1616y"
  ) # rename variable to sensible names

Panel_long_factors <- Panel_long_factors %>%
  mutate(
    across(.cols = everything(), ~ str_replace(., " Tage?", "")),
    across(.cols = everything(), ~ str_replace(., " und frueher", "")),
    across(.cols = c(1:5, 7:13), as.numeric),
    sex = sex - 1,
    agecategory = cut(birthyear,
      breaks = c(1955, seq(1956, 2001, by = 9)),
      labels = c(
        "1955 and earlier",
        paste(seq(1956, 1992, by = 9),
          seq(1964, 2001, by = 9),
          sep = "-"
        )
      ),
      right = FALSE
    ),
    polarization = abs(left_right_self - 6),
    index_active_online_part = online_part_like_com + online_part_shared + online_part_post
  ) # format as numeric, add a polarization variable for left-right, add index for active online participation and convert sex to real dummy

# alpha cronbach's reliability score for index
psych::alpha(subset(Panel_long_factors, select = c(online_part_like_com, online_part_shared, online_part_post)), check.keys = TRUE) # alpha cronbach indicates that building an indexvariable is valid

# save(Panel_long_factors ,file = "Panel_long_factors.Rda")

# load("Panel_long_factors.Rda")

# Descriptive analysis ----------------------------------------------------
ggboxplot(Panel_long_factors, x = "wave", y = "soc_med_pol") # boxplots for soc_med_pol distribution across waves
ggboxplot(Panel_long_factors, x = "wave", y = "polarization") # boxplots for polarization distribution across waves

summary(Panel_long_factors$soc_med_pol) # across all waves: mean = 2.264 & median = 1
summary(Panel_long_factors$polarization) # across all waves: mean = 1.574 & median = 1

Panel_long_factors %>%
  group_by(wave) %>%
  summarize(
    mean = mean(soc_med_pol),
    sd = sd(soc_med_pol),
    median = median(soc_med_pol)
  )

# wave    mean  sd median
# 2017-07-01 2.25 2.61   1
# 2017-09-01 2.36 2.63   1
# 2021-09-01 2.40 2.72   1

Panel_long_factors %>%
  group_by(wave) %>%
  summarize(
    mean = mean(polarization),
    sd = sd(polarization),
    median = median(soc_med_pol)
  )

# wave    mean sd  median
# 2017-07-01 1.61 1.43   1
# 2017-09-01 1.64 1.46   1
# 2021-09-01 1.53 1.38   1

# stacked Barplot for distribution of polarization across waves
Panel_long_factors %>%
  count(wave, polarization) %>%
  group_by(wave) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = wave, y = prop, fill = factor(polarization))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
  ggtitle("Distribution of polarization variable across waves") +
  xlab("Wave") +
  ylab("Proportion") +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "Polarization")

ggsave("distribution_polarization_stacked.png")

# stacked Barplot for distribution of social media usage across waves
Panel_long_factors %>%
  count(wave, soc_med_pol) %>%
  group_by(wave) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = wave, y = prop, fill = factor(soc_med_pol))) +
  geom_bar(stat = "identity") +
  scale_fill_viridis_d(option = "viridis") +
  theme_minimal() +
  ggtitle("Distribution of Social Media Usage across waves") +
  xlab("Wave") +
  ylab("Proportion") +
  theme(
    legend.position = "right",
    axis.title.x = element_text(size = 18),
    axis.title.y = element_text(size = 18),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(fill = "Days per week")

ggsave("distribution_social_media_stacked.png")
                            
# Distribution of social media usage visualized in barplots
Panel_long_factors %>%
  count(wave, soc_med_pol) %>%
  group_by(wave) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(soc_med_pol), y = prop, fill = factor(soc_med_pol))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_grey(start = 0.2, end = 0.8) +
  ggtitle("Distribution of social media usage") +
  facet_wrap(~wave) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  xlab("Days per week") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format())

ggsave("distribution_social_media.png")

# Distribution of polarization variable visualized in barplots
Panel_long_factors %>%
  count(wave, left_right_self) %>%
  group_by(wave) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(left_right_self), y = prop, fill = factor(left_right_self))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_grey(start = 0.2, end = 0.8) +
  ggtitle("Distribution of political orientation") +
  facet_wrap(~wave) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  xlab("Political orientation") +
  ylab("Proportion") +
  scale_x_discrete(labels = c("1" = "left")) +
  scale_y_continuous(labels = scales::percent_format())

ggsave("distribution_left_right.png")

# Distribution of polarization variable visualized in barplots
Panel_long_factors %>%
  count(wave, polarization) %>%
  group_by(wave) %>%
  mutate(prop = n / sum(n)) %>%
  ggplot(aes(x = factor(polarization), y = prop, fill = factor(polarization))) +
  geom_bar(position = "dodge", stat = "identity") +
  scale_fill_grey(start = 0.2, end = 0.8) +
  ggtitle("Distribution of polarization across the waves") +
  facet_wrap(~wave) +
  theme_ipsum() +
  theme(
    legend.position = "none",
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  ) +
  xlab("Polarization") +
  ylab("Proportion")

ggsave("distribution_polarization.png")

# Boxplots of the distribution of political polarization by social media usage across the whole panel
ggplot(Panel_long_factors, aes(x = factor(soc_med_pol), y = polarization)) +
  geom_boxplot() +
  labs(
    x = "Social Media usage (days per week)", y = "Political Polarization",
    title = "Distribution of political polarization by social media usage"
  ) +
  theme_minimal() +
  theme(
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    axis.text = element_text(size = 14),
    strip.text = element_text(size = 16),
    plot.title = element_text(size = 18, face = "bold")
  )


ggsave("distribution_polarization_social_media_usage.png")

# Panel Regression without controls ----------------------------------------------
Panel_polarization <- pdata.frame(Panel_long_factors, index = c("lfdn", "wave")) # transform dataframe to panel dataframe

POLS_nc <- plm(polarization ~ soc_med_pol + index_active_online_part, model = "pooling", data = Panel_polarization) # Pooled OLS

bptest(polarization ~ soc_med_pol + index_active_online_part, data = Panel_polarization, studentize = F) # Breusch pagan test p-value < 2.2e-16 --> heteroscedasticity is present

fixed_effects_nc <- plm(polarization ~ soc_med_pol + index_active_online_part, data = Panel_polarization, index = c("lfdn", "wave"), model = "within") # fixed model
random_effects_nc <- plm(polarization ~ soc_med_pol + index_active_online_part, data = Panel_polarization, index = c("lfdn", "wave"), model = "random") # random model

phtest(fixed_effects_nc, random_effects_nc) # Hausman test indicates fixed effects model is preferred

summary(fixed_effects_nc)

# Panel Regression with controls ------------------------------------------
POLS_wc <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, model = "pooling", data = Panel_polarization) # Pooled OLS

bptest(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization, studentize = F) # Breusch pagan test p-value < 2.2e-16 --> heteroscedasticity is present

fixed_effects_wc <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization, index = c("lfdn", "wave"), model = "within") # fixed model
random_effects_wc <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization, index = c("lfdn", "wave"), model = "random") # random model
phtest(fixed_effects_wc, random_effects_wc) # Hausman test indicates fixed effects model is preferred

summary(fixed_effects_wc)

# Compute cluster-robust standard errors
cluster_robust_se_nc <- vcovHC(fixed_effects_nc, type = "HC1", cluster = "group")
cluster_robust_se_wc <- vcovHC(fixed_effects_wc, type = "HC1", cluster = "group")

# Extract standard errors
se_nc <- sqrt(diag(cluster_robust_se_nc))
se_wc <- sqrt(diag(cluster_robust_se_wc))

# Export Regression table with stargazer
stargazer(fixed_effects_nc, fixed_effects_wc,
  column.labels = c("without controls", "with controls"),
  se = list(se_nc, se_wc),
  title = "Panel Regression Model with fixed effects of social media usage on political polarization",
  type = "html",
  header = F,
  model.numbers = F,
  out = "models_fixed_effects.html"
)

# Panel Regression grouped -----------------------------------------------
Filter_panel <- Panel_long_factors[!duplicated(Panel_long_factors$lfdn), ] # get first observation of each case in a dataframe to filter cases

filter_panel_left <- Filter_panel %>%
  filter(left_right_self < 6) # left filter datapanel

filter_panel_right <- Filter_panel %>%
  filter(left_right_self > 6) # right filter datapanel

filter_panel_middle <- Filter_panel %>%
  filter(left_right_self == 6) # middle filter datapanel

Panel_left <- semi_join(Panel_long_factors, filter_panel_left, by = "lfdn")

Panel_right <- semi_join(Panel_long_factors, filter_panel_right, by = "lfdn")

Panel_middle <- semi_join(Panel_long_factors, filter_panel_middle, by = "lfdn")

Panel_polarization_left <- pdata.frame(Panel_left, index = c("lfdn", "wave")) # transform dataframe to panel dataframe

Panel_polarization_right <- pdata.frame(Panel_right, index = c("lfdn", "wave")) # transform dataframe to panel dataframe

Panel_polarization_middle <- pdata.frame(Panel_middle, index = c("lfdn", "wave")) # transform dataframe to panel dataframe

# left group
POLS_left <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2,
  model = "pooling", data = Panel_polarization_left
)

bptest(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_left, studentize = F) # Breusch pagan test p-value = 3.01e-11 --> heteroscedasticity is present

fixed_effects_left <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_left, index = c("lfdn", "wave"), model = "within") # fixed model
random_effects_left <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_left, index = c("lfdn", "wave"), model = "random") # random model
phtest(fixed_effects_left, random_effects_left) # Hausman test indicates fixed effects model is preferred

summary(fixed_effects_left)

# right group
POLS_right <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2,
  model = "pooling", data = Panel_polarization_right
)

bptest(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_right, studentize = F) # Breusch pagan test p-value = 9.194e-16 --> heteroscedasticity is present

fixed_effects_right <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_right, index = c("lfdn", "wave"), model = "within") # fixed model
random_effects_right <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_right, index = c("lfdn", "wave"), model = "random") # random model
phtest(fixed_effects_right, random_effects_right) # Hausman test indicates fixed effects model is preferred

summary(fixed_effects_right)

# middle group
POLS_middle <- plm(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2,
  model = "pooling", data = Panel_polarization_middle
)

bptest(polarization ~ soc_med_pol + index_active_online_part + pol_interest + sex + agecategory + edu2, data = Panel_polarization_middle, studentize = F) # Breusch pagan test p-value < 2.2e-16 --> heteroscedasticity is present

fixed_effects_middle <- plm(polarization ~ soc_med_pol + pol_interest + sex + agecategory + edu2 + index_active_online_part, data = Panel_polarization_middle, index = c("lfdn", "wave"), model = "within") # fixed model
random_effects_middle <- plm(polarization ~ soc_med_pol + pol_interest + sex + agecategory + edu2 + index_active_online_part, data = Panel_polarization_middle, index = c("lfdn", "wave"), model = "random") # random model
phtest(fixed_effects_middle, random_effects_middle) # Hausman test indicates fixed effects model is preferred

summary(fixed_effects_middle)

# Compute cluster-robust standard errors for each model
cluster_robust_se_left <- vcovHC(fixed_effects_left, type = "HC1", cluster = "group")
cluster_robust_se_middle <- vcovHC(fixed_effects_middle, type = "HC1", cluster = "group")
cluster_robust_se_right <- vcovHC(fixed_effects_right, type = "HC1", cluster = "group")

# Extract standard errors
se_left <- sqrt(diag(cluster_robust_se_left))
se_middle <- sqrt(diag(cluster_robust_se_middle))
se_right <- sqrt(diag(cluster_robust_se_right))

# Export Regression table with stargazer
stargazer(fixed_effects_left, fixed_effects_middle, fixed_effects_right,
  column.labels = c("left subsample", "middle subsample", "right subsample"),
  se = list(se_left, se_middle, se_right),
  title = "Panel Regression Models with fixed effects of social media usage on political polarization with subsamples",
  type = "html",
  header = F,
  model.numbers = F,
  out = "models_fixed_effects_subgroups.html"
)
