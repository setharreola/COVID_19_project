library(tidyverse)
library(MASS)

socal_counties_imputed <- read_csv("../data/output/socal_counties_imputed.csv")

covid_data <- socal_counties_imputed %>% 
  mutate(county = as.factor(county),
         folds = sample(1:5, nrow(.), replace = T)) %>% 
  dplyr::select(-c(date, income, population, households))

accuracy <- data.frame()
for (i in 1:5){
  train_data <- covid_data %>% filter(folds != i)
  test_data <- covid_data %>% filter(folds == i)
  qda_fit <- qda(county ~ ., data = train_data)
  
  accuracy <- test_data %>% 
    mutate(prediction = predict(qda_fit, test_data, type = "class")$class) %>% 
    group_by(county) %>% 
    summarise(count = n(),
              accuracy = sum(county == prediction)/n()) %>% 
    bind_rows(accuracy)
  
}

qda_summary <- accuracy %>%
  group_by(county) %>%
  summarise(accuracy = sum(count*accuracy)/sum(count))

qda_summary %>% 
  ggplot() +
  geom_bar(aes(x = county, y = accuracy), stat = "identity") +
  labs(title = "QDA Accuracy by county",
       x = "",
       y = "Accuracy") + 
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70))
