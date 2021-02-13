library(caret)
library(tidyverse)

socal_counties_imputed <- read_csv("./data/output/socal_counties_imputed.csv")

covid_data <- socal_counties_imputed %>% 
  mutate(county = as.factor(county),
         folds = sample(1:5, nrow(.), replace = T)) %>% 
  dplyr::select(-c(date, income, population, households))

scaled_covid_data <- covid_data %>% 
  mutate(across(where(is.numeric), function(x) (x - mean(x))/sd(x)),
         folds = sample(1:5, nrow(.), replace = T))

model_types = c("knn", "qda", "lda", "multinom")

get_accuracy <- function(method){
  accuracy <- data.frame()
  for (i in 1:5){
    data <- if (method == "knn") scaled_covid_data else covid_data
    train_data <- data %>% filter(folds != i)
    test_data <- data %>% filter(folds == i)
    model_formula = county ~ cases + apple_avg + retail_avg + grocery_avg + parks_avg + transit_avg + work_avg + residential_avg
    model_fit <- train(model_formula, data = train_data,
                     method = method, tuneGrid = (if (method == "knn") data.frame(k = 5) else NULL))
    accuracy <- test_data %>% 
      mutate(prediction = predict(model_fit, test_data)) %>% 
      group_by(county) %>% 
      summarise(count = n(),
                accuracy = sum(county == prediction)/n()) %>% 
      bind_rows(accuracy)
  }
  
  results_by_county <- accuracy %>%
    group_by(county) %>%
    summarise(accuracy = sum(count*accuracy)/sum(count))
  
  return(results_by_county)
}

results <- data.frame()

for (methods in model_types){
  results <- bind_rows(results, get_accuracy(methods))
}

results <- results %>% 
  mutate(method = rep(model_types, each = 7)) %>% 
  dplyr::select(method, everything())

results %>% 
  mutate(method = fct_recode(method, `KNN` = "knn", `LDA` = "lda",
                             `Multinomial` = "multinom", `QDA` = "qda")) %>% 
  ggplot() +
  geom_point(aes(x = county, y = accuracy, color = method),
             position = position_jitter(width = 0.1)) +
  labs(title = "Performance of ML algorithms in predicting SoCal counties",
       x = "",
       y = "Accuracy",
       color = "Method") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
