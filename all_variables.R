#####################################
##### Life course all variables #####
#####################################

# Packages ----
library(tidyverse)
library(countrycode)
library(janitor)
library(splines)
library(performance)
library(ggrepel)
theme_set(theme_bw())

# Data importation & cleaning ----
load("Data/wvs_timeseries.rdata")
d <- data1
rm(data1)

# Read in the data 
d_short <- d %>% 
  select(S002VS, 
         COUNTRY_ALPHA,
         S020,
         S003,
         X001,
         X003, 
         X007, 
         A001:A006,
         A029:A042,
         A124_02, 
         A124_03, 
         A124_06:A124_09,
         A165,
         A170,
         A173,
         C001,
         C002,
         D057,
         E033,
         E035:E037,
         E039,
         E069_01:E069_02,
         E069_04:E069_08,
         E069_10:E069_13,
         E069_17,
         F028,
         F034,
         F050,
         F053,
         F063,
         F114A,
         F115:F123,
         G006) %>% 
  rename(wave = S002VS, 
         country = COUNTRY_ALPHA,
         country_code = S003, 
         year_survey = S020,
         sex = X001,
         age = X003, 
         
         # Importance battery: 1 means very important, 4 means not at all important
         important_family = A001,
         important_friends = A002, 
         important_leisure = A003, 
         important_poltics = A004,
         important_work = A005, 
         important_religion = A006, 
         
         # Important qualities in children: 1 means mentioned, 0 means not mentioned
         child_independence = A029, 
         child_hard_work = A030, 
         child_feeling_responsibility = A032, 
         child_imagination = A034, 
         child_tolerance = A035, 
         child_thrift = A038, 
         child_determination = A039, 
         child_religion = A040, 
         child_unselfish = A041,
         child_obedience = A042,
         
         # People you would not like to have as neighbours: 1 mentioned, 0 means not mentioned
         neigh_diff_race = A124_02, 
         neigh_drink = A124_03, 
         neigh_imm = A124_06, 
         neigh_aids = A124_07, 
         neigh_drugs = A124_08, 
         neigh_gay = A124_09,
         
         # Most people can be trusted: (1) Most people, (2) you can never be too careful
         trust_people = A165,
         
         # (10) means more satisfied
         life_satisf = A170, 
         
         # Control over life's choices: (10) feels most in control. 
         choice_control = A173, 
         
         # Men should be given jobs over women: (1) Agree, (2) Disagree, (3) Neither A nor D
         jobs_men_over_women = C001, 
         
         # Nationals should be given jobs over foreigners: (1) Agree, (2) Disagree, (3) Neither A nor D
         jobs_national_over_foreign = C002, 
         
         # Likert: (1) Strongly Agree -- (4) Strongly Disagree
         housewife_fulfilling = D057,
         
         # Left/Right placement: (1) Left -- (10) Right
         politics_scale = E033,
         
         # Importance of income equality 
         income_eq = E035, 
         
         # Business should be public/private owned: (1) Private -- (10) Public
         pvt_state_owned = E036, 
         
         # Government should take responsibility vs individual responsibility: (1) Gvt -- (10) Individual
         gvt_responsibility = E037,
         
         # Competition good or harmful: (1) Good --- (10) harmful
         competition_good_evil = E039, 
         
         # Confidence battery: (1) A great deal; (2) Quite a lot; (3) Not very much; (4) None at all
         confidence_churches = E069_01, 
         confidence_armed_forces = E069_02,
         confidence_press = E069_04, 
         confidence_unions = E069_05, 
         confidence_police = E069_06, 
         confidence_parliament = E069_07,
         confidence_civil = E069_08,
         confidence_television = E069_10,
         confidence_governement = E069_11,
         confidence_political_party = E069_12,
         confidence_major_companies = E069_13,
         confidence_justice_courts = E069_17,
         
         # How often attends religious services?
         # (1) More than once a week -- (8) Never
         attend_relig = F028, 
         
         # (1) Religious; (2) not religious; (3) atheist
         religious_person = F034,
         
         # Believes (1), does not believe (0)
         believe_god = F050, 
         believe_hell = F053, 
         
         # (1) Not at all important -- (10) very important
         important_god = F063,
         just_gvt_benefits = F114A,
         just_fare_public_trans = F115,
         just_cheat_taxes = F116,
         just_bribe = F117,
         just_homosexuality = F118, 
         just_prostitution = F119, 
         just_abortion = F120, 
         just_divorce = F121, 
         just_euthanasia = F122, 
         just_suicide = F123, 
         proud_nationality = G006, 
         marital_status = X007) 



# Zap labels 
d_short <- haven::zap_labels(d_short)

# All missing values are indicated with negative values
# Turn them into NAs for now

d_short <- d_short %>% 
  mutate(
    across( 
      .cols = 5:68, 
      ~ ifelse(
        . < 0, 
        NA_real_, 
        .
      ))
  )

# Create the variables for manipulation

d_waves <- d_short %>% 
  pivot_longer(important_family:proud_nationality,
               names_to = "variable",
               values_to = "y") %>%
  mutate(age01 = (age - 25)/(64 - 25),
         birthyear = year_survey - age,
         year0 = year_survey - 1981,
         cohort5 = floor(birthyear/5) * 5,
         cohort10 = floor(birthyear/10) * 10) %>%
  drop_na(c(y, birthyear)) %>% 
  group_by(country, variable) %>% 
  mutate(start = min(year_survey),
         end = max(year_survey),
         span = end - start,
         waves = n_distinct(wave),
         obs = n()) %>% 
  ungroup()

# Filter dataset appropriately 
d30 <- d_waves %>% 
  filter(waves >= 4 & span >= 30 & age > 24)
d30 <- d30 %>% 
  mutate(cohort10t = case_when(
    cohort10 < 1920 ~ 1920,
    cohort10 > 1980 ~ 1980,
    TRUE ~ cohort10
  ))

# Functions ----
getmod_lm_b2 <- function(df) {
  lm(y ~ cohort10t,
     data = df)
}

getmod_lm_bw2 <- function(df) {
  lm(y ~ ns(year0, df = 1) * cohort10t,        # spline (or linear; check current setting)
     data = df)                                # df=2 means one bend; df=1 means line
}

# get R2
get_r2 <- function(mod) {
  r2 <- r2(mod) %>% .[[1]] %>% as.numeric()
  return(r2)
}

# Analysis ----

# Nest dataset
d30_nest <- d30 %>% 
  group_by(country, variable) %>% 
  nest()

# Carry out analyses
results <- d30_nest %>% 
  mutate(mb = map(data, getmod_lm_b2),
         mbw = map(data, getmod_lm_bw2),
         rb = map_dbl(mb, get_r2),
         rbw = map_dbl(mbw, get_r2),
         pbetween = rb/rbw) %>% 
  select(country, variable, rb, rbw, pbetween)

# Results ----
ggplot(results,
       aes(x = rbw,
           y = pbetween,
           color = variable, 
           label = variable)) +
  geom_point() + 
  geom_text_repel(
    data = results %>% 
      filter(rbw >= 0.1)
  ) +
  facet_wrap(~country) +
  theme(legend.position = "none")

### CHANGING X-AXIS TO ABSOLUTE CHANGE ####

# Find variables with meaningful changes from first to last wave
d_change <- d_waves %>%
  filter(waves >= 4 & span >= 30 & age > 24) %>% 
  mutate(first_obs = min(year0),
         last_obs = max(year0),
         .by = c(country, variable)) %>% 
  filter(year0 == first_obs | year0 == last_obs) %>% 
  mutate(post = if_else(year0 == last_obs, 1L, 0L)) %>%
  mutate(ymean = mean(y),
         .by = c(country, variable, post)) %>% 
  mutate(ysd = sd(y),
         .by = c(country, variable)) %>%
  group_by(country, variable, post) %>%
  select(ymean, ysd) %>% 
  slice_head(n = 1) %>%
  ungroup() %>% 
  pivot_wider(names_from = post,
              values_from = ymean,
              names_prefix = "mean") %>% 
  mutate(change = (mean1 - mean0) / ysd,
         abschange = abs(change))
  
hist(d_change$abschange)

# attach to results
results <- left_join(results,
                     select(d_change, country, variable, abschange))

# Results ----
ggplot(results,
       aes(x = abschange,
           y = pbetween,
           color = variable, 
           label = variable)) +
  geom_point() + 
  geom_text_repel(
    data = results %>% 
      filter(abschange >= .8) # "large" differences according to Cohen's d (arbitrary)
  ) +
  facet_wrap(~country) +
  theme(legend.position = "none")
