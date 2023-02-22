" Replication Project COMPAS Analysis
Scenarios
Jorge Acedo
POLI 271 - Winter 2023
Replication source : https://github.com/propublica/compas-analysis
"

# Import libraries
library(dplyr)
library(marginaleffects)

# Run replication code to get models
source("replication.R")

# Plot of predicted probability across priors and sex
 
plot_predictions(model_rep, condition = list("priors_count", "gender_factor"))

# Plot of predicted probability across priors and race

plot_predictions(model_rep, condition = list("priors_count", 
                                             race_factor = c("Caucasian",
                                                             "African-American")))

# Plot of predicted probability across ages and sex

model_age <- glm(score_factor ~ gender_factor + age + race_factor + 
                   priors_count + crime_factor + two_year_recid, 
                 family="binomial", 
                 data=df)

plot_predictions(model_age, condition = list("age", "gender_factor"))

# Plot of predicted probability across ages and race

plot_predictions(model_age, condition = list("age", 
                                             race_factor = c("Caucasian",
                                                             "African-American")))
 
plot_predictions(model_age, condition = list("age", 
                                             race_factor = c("Caucasian",
                                                             "African-American"),
                                             "priors_count" = 0)) # With 0 prior crimes.
