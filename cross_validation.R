" Replication Project COMPAS Analysis
Cross Validation - Out of Sample Predictions
Jorge Acedo
POLI 271 - Winter 2023
Replication source : https://github.com/propublica/compas-analysis
"

# Import libraries
library(dplyr)
library(caret)
library(ROCR)

# Run replication code to get models
source("replication.R")

# Competing models -------------------------------------------------------------

# What happens if we exclude race from the regression? The authors claim is that
# race influences the score of a person. If a model without race have better
# out-of-sample predictions, will that would suggest that race is not an 
# important factor to predict the COMPAS score.
model_comp_race <- glm(score_factor ~ gender_factor + age_factor + 
priors_count + crime_factor + two_year_recid, family="binomial", 
data=df)

# What happens if we exclude the recidivism variable? The conclusions that
# the authors make rely on the fact that the recidivism variable will control
# for the actual recidivism likelihood. If a model without recidivism have a 
# better out-of-sample performance, will that suggest that recidivism is not
# an important indicator of COMPAS score. 
model_comp_recid <- glm(score_factor ~ gender_factor + age_factor + race_factor + 
                         priors_count + crime_factor , family="binomial", 
                       data=df)

# Cross validation -------------------------------------------------------------
set.seed(1)

# Change score_factor to HighScore == 1, LowScore == 0.
df <- mutate(df, score_bin = ifelse(score_factor=="HighScore",1,0))

# defining 10-fold cross-validation
cv <- trainControl(method = "cv", number = 10, savePredictions="all", 
                   classProbs=T)

# Fit models with 10-fold CV
cv_fit_rep <- train(score_bin ~ gender_factor + age_factor + race_factor + 
                      priors_count + crime_factor + two_year_recid, data=df, 
                       method="glm", 
                       family=binomial(link = "logit"), 
                       trControl=cv)

cv_fit_race <- train(score_bin ~ gender_factor + age_factor + 
                      priors_count + crime_factor + two_year_recid, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)

cv_fit_recid <- train(score_bin ~ gender_factor + age_factor + race_factor + 
                      priors_count + crime_factor, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)

cv_fit_age <- train(score_bin ~ gender_factor  + race_factor + 
                      priors_count + crime_factor + two_year_recid, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)

cv_fit_gender <- train(score_bin ~  age_factor + race_factor + 
                      priors_count + crime_factor + two_year_recid, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)

cv_fit_crime <- train(score_bin ~ gender_factor + age_factor + race_factor + 
                      priors_count + two_year_recid, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)

cv_fit_priors <- train(score_bin ~ gender_factor + age_factor + race_factor + 
                       crime_factor + two_year_recid, data=df, 
                    method="glm", 
                    family=binomial(link = "logit"), 
                    trControl=cv)


# Out-of-sample ROC curves
cv_pred_rep <- prediction(cv_fit_rep[["pred"]]$pred, cv_fit_rep[["pred"]]$obs)
cv_perf_rep <- performance(cv_pred_rep,"tpr","fpr")

cv_pred_race <- prediction(cv_fit_race[["pred"]]$pred, cv_fit_race[["pred"]]$obs)
cv_perf_race <- performance(cv_pred_race,"tpr","fpr")

cv_pred_recid <- prediction(cv_fit_recid[["pred"]]$pred, cv_fit_recid[["pred"]]$obs)
cv_perf_recid <- performance(cv_pred_recid,"tpr","fpr")

cv_pred_age <- prediction(cv_fit_age[["pred"]]$pred, cv_fit_age[["pred"]]$obs)
cv_perf_age <- performance(cv_pred_age,"tpr","fpr")

cv_pred_gender <- prediction(cv_fit_gender[["pred"]]$pred, cv_fit_gender[["pred"]]$obs)
cv_perf_gender <- performance(cv_pred_gender,"tpr","fpr")

cv_pred_crime <- prediction(cv_fit_crime[["pred"]]$pred, cv_fit_crime[["pred"]]$obs)
cv_perf_crime <- performance(cv_pred_crime,"tpr","fpr")

cv_pred_priors <- prediction(cv_fit_priors[["pred"]]$pred, cv_fit_priors[["pred"]]$obs)
cv_perf_priors <- performance(cv_pred_priors,"tpr","fpr")

# ROC curves
par(las=1, bty="n")  
plot(cv_perf_rep, main="Out of Sample ROC Curves", bty="n",lwd=3, col = "#00008B")
plot(cv_perf_race, lwd=3, add=T, lty=2, col="#8B0046")
plot(cv_perf_recid, lwd=3, add=T, lty=2, col="#8B008B")
plot(cv_perf_age, lwd=3, add=T, lty=2, col="#8B4500")
plot(cv_perf_gender, lwd=3, add=T, lty=2, col="#8B8B00")
plot(cv_perf_crime, lwd=3, add=T, lty=2, col="#468B00")
plot(cv_perf_priors, lwd=3, add=T, lty=2, col="#008B8B")
#lines(as.vector(cv_fit_rep[["pred"]]$obs), as.vector(cv_fit_rep[["pred"]]$obs), 
#      lty=3)
legend("bottomright", legend = c("Replication", "W/o Race", 
                                 "W/o Recidivism", "W/o Age", "W/o Gender",
                                 "W/o Previous Crime", "W/o Priors"), 
       col = c("#00008B", "#8B0046", "#8B008B", "#8B4500", "#8B8B00", 
                        "#468B00", "#008B8B"), lwd=2,
       lty = c(1,2,2,2,2,2,2))