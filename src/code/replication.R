" Replication Project COMPAS Analysis
Replication
Jorge Acedo
POLI 271 - Winter 2023
Replication source : https://github.com/propublica/compas-analysis
"

# Import libraries
library(dplyr)

# Risk of Recidivism -----------------------------------------------------------

# Import data
raw_data <- read.csv("data/compas-scores-two-years.csv")
nrow(raw_data)

# Recreating filters done by the authors
df <- raw_data %>% 
  select(age, c_charge_degree, race, age_cat, score_text, sex, priors_count, 
         days_b_screening_arrest, decile_score, is_recid, two_year_recid, 
         c_jail_in, c_jail_out) %>% 
  filter(days_b_screening_arrest <= 30, #Charge date of COMPAS score around 30 
         days_b_screening_arrest >= -30, #days from the arrest.
         is_recid != -1, #no COMPAS case found
         c_charge_degree != "O", # Ordinary offenses
         score_text != "N/A") 
nrow(df)

# Demographics counts/percentages

## Age categories
df %>% 
  group_by(age_cat) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))

## Race
df %>% 
  group_by(race) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))

## Sex
df %>% 
  group_by(sex) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))

## Score
df %>% 
  group_by(score_text) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))


## Score
df %>% 
  group_by(decile_score) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))

## Recidivism
df %>% 
  group_by(two_year_recid) %>% 
  summarise(count = n(),
            perc = count/nrow(df)) %>% 
  arrange(desc(count))

# Logistic regression comparing low vs high scores (medium + high)
df <- df %>% 
  mutate(crime_factor = factor(c_charge_degree),
         age_factor = factor(age_cat),
         race_factor = factor(race),
         gender_factor = factor(sex, labels = c("Female", "Male")),
         score_factor = factor(score_text != "Low", 
                               labels = c("LowScore", 
                                          "HighScore"))) %>% 
  within(age_factor <- relevel(age_factor, ref = 1)) %>% 
  within(race_factor <- relevel(race_factor, ref = 3)) %>% 
  within(gender_factor <- relevel(gender_factor, ref = 2))

model_rep <- glm(score_factor ~ gender_factor + age_factor + race_factor + 
               priors_count + crime_factor + two_year_recid, family="binomial", 
             data=df)
summary(model_rep)

"
Black defendants are 45% more likely than white defendants to receive a higher 
score correcting for the seriousness of their crime, previous arrests, and 
future criminal behavior.
"

int_coef <- model_rep[["coefficients"]][["(Intercept)"]]
control <- exp(int_coef) / (1 + exp(int_coef))
black_coef <- model_rep[["coefficients"]][["race_factorAfrican-American"]]
exp(black_coef) / (1 - control + (control * exp(black_coef)))

"
Women are 19.4% more likely than men to get a higher score.
"
fem_coef <- model_rep[["coefficients"]][["gender_factorFemale"]]
exp(fem_coef) / (1 - control + (control * exp(fem_coef)))


"
Most surprisingly, people under 25 are 2.5 times as likely to get a higher 
score as middle aged defendants.
"
young_coef <- model_rep[["coefficients"]][["age_factorLess than 25"]]
exp(young_coef) / (1 - control + (control * exp(young_coef)))


# Risk of Violent Recidivism ---------------------------------------------------

# Import data
raw_v_data <- read.csv("data/compas-scores-two-years-violent.csv")
nrow(raw_v_data)

# Recreating filters
dfv <- raw_v_data %>%
  select(age, c_charge_degree, race, age_cat, v_score_text, sex,
         priors_count, days_b_screening_arrest, v_decile_score, is_recid, 
         two_year_recid) %>% 
  filter(days_b_screening_arrest <= 30,
         days_b_screening_arrest >= -30,
         is_recid != -1,
         c_charge_degree != "O",
         v_score_text != 'N/A')
nrow(dfv)

# Demographics counts/percentages

## Age categories
dfv %>% 
  group_by(age_cat) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))

## Race
dfv %>% 
  group_by(race) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))

## Sex
dfv %>% 
  group_by(sex) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))

## Score
dfv %>% 
  group_by(v_score_text) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))


## Score
dfv %>% 
  group_by(v_decile_score) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))

## Recidivism
dfv %>% 
  group_by(two_year_recid) %>% 
  summarise(count = n(),
            perc = count/nrow(dfv)) %>% 
  arrange(desc(count))

# Logistic regression comparing low vs high scores (medium + high)
dfv <- dfv %>%  
  mutate(crime_factor = factor(c_charge_degree),
         age_factor = as.factor(age_cat),
         race_factor = factor(race,
                              labels = c("African-American", 
                                         "Asian",
                                         "Caucasian", 
                                         "Hispanic", 
                                         "Native American",
                                         "Other")),
         gender_factor = factor(sex, labels= c("Female","Male")),
         score_factor = factor(v_score_text != "Low", 
                               labels = c("LowScore","HighScore"))) %>%
  within(age_factor <- relevel(age_factor, ref = 1)) %>%
  within(race_factor <- relevel(race_factor, ref = 3)) %>%
  within(gender_factor <- relevel(gender_factor, ref = 2))

model_rep_v <- glm(score_factor ~ gender_factor + age_factor + race_factor +
               priors_count + crime_factor + two_year_recid, family="binomial", 
               data=dfv)

summary(model_rep_v)

"
The violent score overpredicts recidivism for black defendants by 77.3% compared 
to white defendants.
"

int_coef_v <- model_rep_v[["coefficients"]][["(Intercept)"]]
control_v <- exp(int_coef_v) / (1 + exp(int_coef_v))
black_coef_v <- model_rep_v[["coefficients"]][["race_factorAfrican-American"]]
exp(black_coef_v) / (1 - control_v + (control_v * exp(black_coef_v)))

"
Defendands under 25 are 7.4 times as likely to get a higher score as middle aged 
defendants.
"

young_coef_v <- model_rep_v[["coefficients"]][["age_factorLess than 25"]]
exp(young_coef_v) / (1 - control_v + (control_v * exp(young_coef_v)))

rm(raw_data, raw_v_data)