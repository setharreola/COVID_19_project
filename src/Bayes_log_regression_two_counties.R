# This code is based off the Bayesian logistic code from Dr. Behseta
library(caret)
library(tidyverse)
library(dplyr)
library(glmmfields)
library(rstanarm)
library(ggplot2)

##### Bayesian logistic
# Firstly read SoCal data
socal_counties_imputed <- read_csv("./data/output/socal_counties_imputed.csv")

# We will test this on two counties first 
# Pick two counties (LA and Ventura)
two_counties = c("Los AngelesCA","VenturaCA")
covid_data = socal_counties_imputed[socal_counties_imputed$county %in% two_counties,]
covid_data <- covid_data %>% 
  dplyr::select(-c(date, income, population, households))  #remove non-mobility and covid variables

# scale predictors 
for (i in 2:12) {
  covid_data[i] <- scale(covid_data[i])
}

#### design matrix and binary y
covid_data$county <- factor(covid_data$county)
# preparing the inputs
x <- model.matrix(county ~ . - 1, data = covid_data)
y <- covid_data$county

### Gelman has argued for t priors 
#t_prior <- student_t(df = 7, location = 0, scale = 2.5)
normal_prior <- normal(location= 0,scale = sqrt(10^5))
post1 <- stan_glm(county ~ ., data = covid_data,
                  family = binomial(link = "logit"),
                  prior = t_prior, prior_intercept = t_prior, QR=TRUE)

pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
pplot+ geom_vline(xintercept = 0)

### point and interval estimation
round(coef(post1), 2)
round(posterior_interval(post1, prob = 0.9), 2)

#### accuracy
linpred <- posterior_linpred(post1)
preds <- posterior_linpred(post1, transform=TRUE)
pred <- colMeans(preds)
pr <- as.integer(pred >= 0.5)
# posterior classification accuracy
round(mean(xor(pr,as.integer(y==0))),2)

# getting 0.5 for accuracy?

# make this into a function for future pairwise bayes log regression
bayesian <- function(the_data){
  x <- model.matrix(county ~ . - 1, data = covid_data)
  y <- covid_data$county
  t_prior <- student_t(df = 7, location = 0, scale = 2.5)
  post1 <- stan_glm(county ~ ., data = covid_data,
                    family = binomial(link = "logit"),
                    prior = t_prior, prior_intercept = t_prior, QR=TRUE)
  pplot<-plot(post1, "areas", prob = 0.95, prob_outer = 1)
  pplot+ geom_vline(xintercept = 0)
  round(coef(post1), 2)
  round(posterior_interval(post1, prob = 0.9), 2)
  linpred <- posterior_linpred(post1)
  preds <- posterior_linpred(post1, transform=TRUE)
  pred <- colMeans(preds)
  pr <- as.integer(pred >= 0.5)
  acc <- round(mean(xor(pr,as.integer(y==0))),2)
  return(acc)
}

acc <- bayesian(covid_data)