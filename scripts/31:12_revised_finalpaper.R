rm(list=ls(all=TRUE))
library(tidyverse)
library(stargazer)
library(haven)
library(interactions)
library(ggplot2)
library(patchwork)
library(sjPlot)

getwd()

evs2017 <- read_dta("ZA7500_v5-0-0.dta")
head(evs2017)
set.seed(123)

# select the variables
evs_selected <- evs2017[,c("v205", "v206", "v207", "age_r3", "v225", "v243_edulvlb_1", "v31", "v130", "country")]
head(evs_selected)
table(evs_selected$country)
   
# filter countries
evs_subset <- evs_selected %>%
filter(country %in% c(276, 752, 616, 380)) %>%
mutate(country = factor(country,
                    levels = c(276, 752, 616, 380),
                    labels = c("Germany", "Sweden", "Poland", "Italy")))
glimpse(evs_subset)

# check the NA
colSums(is.na(evs_subset))

# rename
evs_clean <- evs_subset %>%
  rename(support_camera = v205, 
         support_email = v206, 
         support_phone = v207, 
         gender = v225,
         education = v243_edulvlb_1,
         soc_trust = v31,
         pol_trust = v130) %>%
# decoding dependent variables
  mutate(across(c(support_camera, support_email, support_phone), 
                ~ case_when(. %in% 1:4 ~ 5 - as.numeric(.), 
                            . %in% -10:-1 ~ NA_real_, TRUE ~ NA_real_)),
  
# convert dependent variables to ordered factors
  across(c(support_camera, support_email, support_phone),
         ~ ordered(., levels = 1:4)),

# decoding the independent variables
# pol trust
pol_trust = case_when(pol_trust %in% 1:4 ~ 5 - as.numeric(pol_trust), 
                      pol_trust %in% -10:-1 ~ NA_real_, TRUE ~ NA_real_),
# soc trust
soc_trust = case_when(soc_trust %in% 1:2 ~ 2 - as.numeric(soc_trust), 
                      soc_trust %in% -10:-1 ~ NA_real_, TRUE ~ NA_real_),

# gender
gender = case_when(gender %in% 1:2 ~ as.character(gender), 
                   gender %in% -10:-1 ~ NA_character_, TRUE ~ NA_character_),
gender = factor(gender, levels = c("1", "2"),
                labels = c("Male", "Female" )),
# age
age = case_when(age_r3 == 1 ~ "18-24",
                age_r3 == 2 ~ "25-34",
                age_r3 == 3 ~ "35-44",
                age_r3 == 4 ~ "45-54",
                age_r3 == 5 ~ "55-64",
                age_r3 == 6 ~ "65-74",
                age_r3 == 7 ~ "75+",
                age_r3 < 0 ~ NA_character_,
                TRUE ~ NA_character_),
# convert age to ordered factor
age = factor(age, levels = c("18-24", "25-34", "35-44",
                             "45-54", "55-64", "75+",
                             ordered = TRUE)),
# edu
edu_level = case_when(education %in% c(0, 1, 2) ~ "Low", 
                      education %in% c(3, 4) ~ "Medium",
                      education %in% c(5, 6, 7, 8) ~ "High",
                      education == 66 ~ NA_character_,
                      education %in% -9:-1 ~ NA_character_,
                      TRUE ~ NA_character_),
# convert edu to ordered factor
edu_level = factor(edu_level, levels = c("Low", "Medium", "High"),
                     ordered = TRUE)) %>%
# removing the original edu variables
select(-education, -age_r3)

names(evs_clean)

# check the NA
missing_value <- map_int(evs_clean, ~sum(is.na(.)))
print(missing_value)
missing_percent <- map_dbl(evs_clean, ~mean(is.na(.)) * 100)
print(missing_percent)

# check thetatal cases
total_samples <- sum(complete.cases(evs_clean))
print(total_samples)

## [Deepseek guide] 
## The original sample is 6993, but after handling the variables,
## the final number of sample becomes to 5169. Almost 26% of the samples were lost.
## The 16.75% missing vlue of age is kinda significantly, which i think it is worth to pay attention.
## I think simply deleting them would impact the results. 
## Therefore, I consulted Deepseek regarding the appropriate handling method.
## He advised me to drop missing values solely in the analytical variables.
## However, i followed the steps as he suggested, the final samples wre kept 5294 in this case.
## The difference between this approach and removing all missing values was only 2.4%.
## Which i think is extremely close, so i still remove all missing values in this case.

# keep all the samples in dependent variables
df_final <- evs_clean %>%
  drop_na(support_camera, pol_trust, age, gender, edu_level, soc_trust, country)

# check the distribution in 4 countries
final_distribution <- df_final %>%
  group_by(country) %>%
  summarise(n_final = n(), percentage_final = n()/nrow(df_final)*100)
print(final_distribution)

# drop all the missing values
df <- evs_clean %>% 
  drop_na()

# check the sample size
nrow(df)

# check the correlation between camera&email&phone
cor_matrix <- df %>%
  select(support_camera, support_email, support_phone) %>%
  mutate(across(everything(), as.numeric)) %>%
  cor(use = "complete.obs")
print(cor_matrix)

# check the distribution in 4 countries
df_distribution <- df %>%
  group_by(country) %>%
  summarise(n_complete = n(), percentage_complete = n()/nrow(df)*100)
print(df_distribution)

# camparison
# Number of keeping all denpent variables
nrow(df_final)
#Number of removing all the missing values
nrow(df)
# Overall difference
nrow(df_final) - nrow(df)
    round((nrow(df_final) - nrow(df))/nrow(df_final)*100, 1)

## [Deepseek Guide]
## Here i got some problems in comparing the details of 4 countries.
## I asked Deepseek to help combine the 2 tables together, and compare the missing values of the 4 countries separately.

# compare the two tables across 4 countries
comparison <- final_distribution %>%
  left_join(df_distribution, by = "country") %>%
  mutate(
    n_diff = n_final - n_complete,
    perc_diff = round((n_diff / n_final) * 100, 1)
  )
print(comparison)

cat("\nDifference across 4 countries:\n")
for(i in 1:nrow(comparison)) {
  country_name <- comparison$country[i]
  n_diff <- comparison$n_diff[i]
  perc_diff <- comparison$perc_diff[i]
  
  cat(country_name, "difference:", n_diff, "(", perc_diff, "%)\n")
}


# OLS model in German for H1
## H1:The higher the level of political trust, 
## the greater the public's acceptance of state surveillance.

# select the DE samples
DE <- df %>%
  filter(country == "Germany")
nrow(DE)

# OLS
fit <- lm(as.numeric(support_camera) ~ pol_trust + age + gender + edu_level + soc_trust,
          data = DE)
summary(fit)
confint(fit)

# OLS models for comparing support for monitoring types (H2)
## H2:Public acceptance of state surveillance lessens as intrusiveness increases.

# OLS for 3 monitor types seperately 
monitor_models <- list(
  camera = lm(as.numeric(support_camera) ~ pol_trust, data = DE),
  email = lm(as.numeric(support_email) ~ pol_trust, data = DE),
  phone = lm(as.numeric(support_phone) ~ pol_trust, data = DE)
)

# create the regression tables
stargazer(monitor_models,
          type = "text",
          style = "apsr",
          title = "3 Types of Monitor Acceptance in Germany",
          dep.var.labels = c("camera", "email", "phone"),
          covariate.labels = c("Political Trust"),
          out = "table1.html")

# OLS for 3 monitor types seperately (Control 'age\gender\edu\soc_trust')
monitor_models_full <- list(
  camera = lm(as.numeric(support_camera) ~ pol_trust + age + gender + edu_level + soc_trust, data = DE),
  email = lm(as.numeric(support_email) ~ pol_trust + age + gender + edu_level + soc_trust, data = DE),
  phone = lm(as.numeric(support_phone) ~ pol_trust + age + gender + edu_level + soc_trust, data = DE)
)

summary(monitor_models_full$camera)
summary(monitor_models_full$email)
summary(monitor_models_full$phone)

stargazer(monitor_models_full,
          type = "text", 
          style = "apsr",
          title = "3 Types of Monitor Acceptance in Germany (Full)",
          dep.var.labels = c("camera", "email", "phone"),
          out = "table2_full.html")

# interaction model for H3 (Camera only)
## H3: Public acceptance of different degrees of state surveillance differs across countries.
df_clean <-df %>%
  drop_na()

# create a interaction model (pol_trust x countries) for H3
camera_interaction <- lm(as.numeric(support_camera) ~ pol_trust * country +  age + gender + edu_level + soc_trust,
                         data = df_clean)
summary(camera_interaction)
stargazer(camera_interaction,
          type = "text",
          style = "apsr",
          title = "Public acceptance of State Camera across countries",
          out = "table2_h3.html")

# visualizations - H3
h3 <- plot_model(camera_interaction, 
                 type = "pred", 
                 terms = c("pol_trust", "country"),
                 ci.lvl = 0.95,
                 show.data = FALSE) +
  labs(title = "Effect of political trust on camera support across countries",
       y = "Support for Camera (1-4)", 
       x = "Political Trust (1-4, High=Trust)") +
  theme_sjplot()

h3
