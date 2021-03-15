library(caret)
library(tidyverse)

counties <- c("San Bernardino County", "San Diego County", "Los Angeles County",
              "Orange County", "Ventura County", "Riverside County", "Imperial County")
#socal_data <- covid_data %>% filter(state == "California",
#                                    county %in% counties) %>% na.omit() %>% ungroup()
#socal_counties_imputed <- read_csv("./data/output/socal_counties_imputed.csv")

socal_data <- read_csv("///Users/setharreola/Documents/COVID_19_project/data/socal_covid_data_imputed_02_10.csv")
socal_data <- socal_data[,-1]
socal_data <- socal_data %>% na.omit() %>% ungroup()

socal_data_to_model <- socal_data %>% 
  mutate(county = as.factor(county),
         folds = sample(1:5, nrow(.), replace = T)) %>% 
  dplyr::select(-c(date, income, population, households))

#scaled_covid_data <- socal_data_to_model %>% 
#  mutate(across(where(is.numeric), function(x) (x - mean(x))/sd(x)),
#         folds = sample(1:5, nrow(.), replace = T))

scaled_covid_data <- socal_data_to_model %>% 
  mutate(cases_sum = (cases_sum - mean(cases_sum))/sd(cases_sum),
         deaths_sum = (deaths_sum - mean(deaths_sum))/sd(deaths_sum),
         apple_avg = (apple_avg - mean(apple_avg))/sd(apple_avg),
         retail_avg = (retail_avg - mean(retail_avg))/sd(retail_avg),
         grocery_avg = (grocery_avg- mean(grocery_avg))/sd(grocery_avg),
         parks_avg = (parks_avg - mean(parks_avg))/sd(parks_avg),
         transit_avg = (transit_avg- mean(transit_avg))/sd(transit_avg),
         work_avg = (work_avg - mean(work_avg))/sd(work_avg),
         residential_avg = (residential_avg - mean(residential_avg))/sd(residential_avg),
         cases = (cases - mean(cases))/sd(cases),
         deaths = (deaths - mean(deaths))/sd(deaths),
         folds = sample(1:5, nrow(.), replace = T))


model_types = c("knn", "qda", "lda", "multinom", "nnet")

get_accuracy <- function(method){
  accuracy <- data.frame()
  for (i in 1:5){
    data <- if (method %in% c("knn", "nnet")) scaled_covid_data else socal_data_to_model
    train_data <- data %>% filter(folds != i)
    test_data <- data %>% filter(folds == i)
    model_formula = county ~ cases + apple_avg + retail_avg + grocery_avg + parks_avg + transit_avg + work_avg + residential_avg
    model_fit <- train(model_formula, data = train_data,
                     method = method, tuneGrid = #(if (method == "knn") data.frame(k = 5) else NULL))
                                                if(method == "knn") data.frame(k = 5) else if (method == "nnet") data.frame(size = 15, decay = 0.01) else NULL)
    accuracy <- test_data %>% 
      mutate(prediction = predict(model_fit, test_data)) %>% 
      group_by(county) %>% 
      summarise(count = n(),
                accuracy = sum(county == prediction)/n()) %>% 
      bind_rows(accuracy)
  }
  
  # results_by_county <- accuracy %>%
  #   group_by(county) %>%
  #   summarise(accuracy = sum(count*accuracy)/sum(count))
  # 
  # return(results_by_county)
  
  results <- accuracy %>%
    mutate(folds = rep(1:7, 5), county = county) %>% 
    group_by(folds) %>% 
    group_by(county) %>%
    summarise(accuracy = sum(count*accuracy)/sum(count))
  
  return(results)
}

results <- data.frame()

for (methods in model_types){
  results <- bind_rows(results, get_accuracy(methods))
}

results <- results %>% 
  mutate(method = rep(model_types, each = 7)) %>% 
  dplyr::select(method, everything())

results %>% 
  group_by(method) %>% 
  summarise(accuracy = mean(accuracy))


results %>% 
  mutate(method = fct_recode(method, `KNN` = "knn", `LDA` = "lda",
                             `Multinomial` = "multinom", `QDA` = "qda", `Neural Net` = "nnet")) %>% 
  ggplot(aes(x = county, y = accuracy, color = method)) +
  geom_point() +
  geom_line(aes(group = method)) +
  labs(title = "Performance of ML algorithms in predicting SoCal counties",
       x = "",
       y = "Accuracy",
       color = "Method") +
  theme_bw() +
  ylim(0.5, 1) + 
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#### Neural nets ####

nn_control <- trainControl(method = "repeatedcv",
                           number = 5,
                           repeats = 3)

tic()
init_nn_mod <- train(model_formula, data = scaled_covid_data, method = "nnet",
                     tuneGrid = expand.grid(size = 12:17, decay = seq(0.01, 0.1, by = 0.02)),
                     trControl = nn_control)
toc()

init_nn_mod$results %>%
  ggplot() +
  geom_tile(aes(x = decay, fill = Accuracy, y = as.factor(size))) +
  theme_bw()



#### Multinomial ####

model_formula = county ~ cases + apple_avg + retail_avg + grocery_avg + parks_avg + transit_avg + work_avg + residential_avg
init_multi_model <- train(model_formula, data = socal_data_to_model, method = "multinom")

init_multi_model <- nnet::multinom(model_formula, data = socal_data_to_model)

multinom_coefs <- coef(init_multi_model) %>%
  broom::tidy() %>%
  janitor::clean_names() %>% 
  select(county = rownames, cases:residential_avg) %>% 
  pivot_longer(cols = cases:residential_avg, names_to = "variable", values_to = "coefficient") %>% 
  filter(variable != "cases")




# All variables
multinom_coefs %>% 
  ggplot(aes(x = county, y = abs(coefficient), color = variable)) +
  geom_point() +
  geom_line(aes(group = variable)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Consistent variables
cons_coefs <- multinom_coefs %>% 
  filter(variable %in% c("apple_avg", "retail_avg", "grocery_avg"))


# Erratic variables
erratic_coefs <- multinom_coefs %>% 
  filter(variable %in% c("parks_avg", "residential_avg", "transit_avg", "work_avg"))

# Highlighted variables


multinom_coefs %>% 
  ggplot(aes(x = county, y = abs(coefficient))) +
  geom_point(color = "gray", alpha = 0.5) +
  geom_line(aes(group = variable), color = "gray", alpha = 0.5) +
  geom_point(aes(color = variable), data = cons_coefs) +
  geom_line(aes(group = variable, color = variable), data = cons_coefs) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


multinom_coefs %>% 
  ggplot(aes(x = county, y = abs(coefficient))) +
  geom_point(color = "gray", alpha = 0.5) +
  geom_line(aes(group = variable), color = "gray", alpha = 0.5) +
  geom_point(aes(color = variable), data = erratic_coefs) +
  geom_line(aes(group = variable, color = variable), data = erratic_coefs) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))






#### Accuracy SD ####

total_accuracy <- method_accuracy
for (i in 2:7){
  method_accuracy <- data.frame(index = 1:7)
  for (method in model_types) {
    accuracy <- get_accuracy(method) %>% pull(accuracy)
    method_accuracy <- method_accuracy %>% 
      bind_cols(accuracy)
  }
  total_accuracy <- total_accuracy %>% bind_rows(method_accuracy)
}

mean_accuracy <- total_accuracy %>% 
  setNames(c("folds", model_types)) %>% 
  select(-folds) %>% 
  summarise_all(mean) %>% 
  pivot_longer(cols = everything(), names_to = "method", values_to = "accuracy")

sd_accuracy <- total_accuracy %>% 
  setNames(c("folds", model_types)) %>% 
  select(-folds) %>% 
  summarise_all(sd) %>% 
  pivot_longer(cols = everything(), names_to = "method", values_to = "std_dev")

accuracy <- mean_accuracy %>% 
  left_join(sd_accuracy)

accuracy %>% 
  ggplot() +
  geom_bar(aes(x = reorder(method, accuracy), y = accuracy, fill = method), stat = "identity") +
  geom_errorbar(aes(x = reorder(method, accuracy), ymin = accuracy - 1*std_dev, ymax = accuracy + 1*std_dev)) +
  theme_bw() +
  coord_flip()


