library(rstanarm)
socal_counties_imputed <- read_csv("./data/output/socal_counties_imputed.csv")

covid_data <- socal_counties_imputed %>% 
  mutate(county = as.factor(county),
         folds = sample(1:5, nrow(.), replace = T)) %>% 
  dplyr::select(-c(date, income, population, households))

scaled_covid_data <- covid_data %>% 
  mutate(across(where(is.numeric), function(x) (x - mean(x))/sd(x)),
         folds = sample(1:5, nrow(.), replace = T))

new_data <- scaled_covid_data %>% 
  filter(county %in% c("San BernardinoCA", "RiversideCA")) %>% 
  mutate(folds = sample(1:5, nrow(.), replace = T),
         county = as.factor(county)) #%>% 
  select(-c(state, date))

accuracy <- data.frame()
t_prior <- student_t(df = 7, location = 0, scale = 2.5)
norm_prior <- normal(location = 0, scale = 10^5)
for (i in 1:5){
  train_data <- new_data %>% filter(folds != i)
  test_data <- new_data %>% filter(folds == i)
  model_fit <- stan_glm(county ~ .,
                        data = train_data,
                        family = binomial(link = "logit"),
                        prior = t_prior,
                        prior_intercept = t_prior)
  accuracy <- test_data %>% 
    mutate(probs = predict(model_fit, newdata = test_data, type = "response"),
           prediction = ifelse(probs <= 0.5, "RiversideCA", "San BernardinoCA")) %>% 
    group_by(county) %>% 
    summarise(count = n(),
              accuracy = sum(county == prediction)/n()) %>% 
    bind_rows(accuracy)
}

results_by_county <- accuracy %>%
  group_by(county) %>%
  summarise(accuracy = sum(count*accuracy)/sum(count))

init_bayes_mnm <- brm(county ~ ., data = scaled_covid_data, family = categorical())

# Pairwise logistic regression
# county_combs <- crossing(county_1 = unique(scaled_covid_data$county),
#             county_2 = unique(scaled_covid_data$county)) %>% 
#   filter(county_1 != county_2) %>% 
#   mutate(county_1 = as.character(county_1),
#          county_2 = as.character(county_2)) %>% 
#   filter(county_1 > county_2)

counties <- unique(scaled_covid_data$county)
county_combs <- expand_grid(county_1 = counties,
                            county_2 = counties) %>% 
  filter(county_1 != county_2)

i = 1

for (i in 1:7){
  for (j in 1:7){
    if (i == j){
      next
    }
    data <- scaled_covid_data %>% 
      filter(county %in% c(as.vector(counties[i]), as.vector(counties[j])))
    
    model <- glm(county ~ ., family = binomial, data = data)
    varname = paste0("preds_", i, "_", j)
    scaled_covid_data[[varname]] = predict(model, type = "response", newdata = scaled_covid_data)
  }
}



data <- scaled_covid_data %>% 
  filter(county %in% county_combs[i,])


init_bayes_logit <- stan_glm(county ~ .,
                             data = data,
                             family = binomial(link = "logit"),
                             prior = t_prior,
                             prior_intercept = t_prior)

plot(init_bayes_logit, plotfun = "areas")



google_data <- google_data %>%
  mutate(sub_region_2 = str_replace(sub_region_2, "ñ", "n"))

apple_data <- apple_data %>%
  mutate(region = str_replace(region, "ñ", "n"))
