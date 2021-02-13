#LDA_performance on Socal counties
library(keras)
library(devtools)
library(reticulate)
library(MASS)
library(ggplot2)
library(nnet)

output = read.csv("../data/output/socal_counties_imputed.csv") # scocal counties

covid_data <- output %>% 
  mutate(county = as.factor(county),
         folds = sample(1:5, nrow(.), replace = T)) %>% 
  dplyr::select(-c(date, income, population, households))

lda.accuracy <- data.frame()
qda.accuracy <- data.frame()
mn.accuracy <- data.frame()

for (i in 1:5){
  train_data <- covid_data %>% filter(folds != i)
  test_data <- covid_data %>% filter(folds == i)
  lda_fit <- lda(county ~ ., data = train_data)
  qda_fit <- qda(county ~ ., data = train_data)
  mn_fit = multinom(county~.,data=train.data)
  
  lda.accuracy <- test_data %>% 
    mutate(prediction = predict(lda_fit, test_data, type = "class")$class) %>% 
    group_by(county) %>% 
    summarise(count = n(),
              lda.accuracy = sum(county == prediction)/n()) %>% 
    bind_rows(lda.accuracy)
  
  qda.accuracy <- test_data %>% 
    mutate(prediction = predict(qda_fit, test_data, type = "class")$class) %>% 
    group_by(county) %>% 
    summarise(count = n(),
              qda.accuracy = sum(county == prediction)/n()) %>% 
    bind_rows(qda.accuracy)
  
  mn.accuracy <- test_data %>% 
    mutate(prediction = predict(mn_fit, test_data, type = "class")) %>% 
    group_by(county) %>% 
    summarise(count = n(),
              mn.accuracy = sum(county == prediction)/n()) %>% 
    bind_rows(mn.accuracy)
  
}

lda_summary <- lda.accuracy %>%
  group_by(county) %>%
  summarise(lda.accuracy = sum(count*lda.accuracy)/sum(count))
qda_summary <- qda.accuracy %>%
  group_by(county) %>%
  summarise(qda.accuracy = sum(count*qda.accuracy)/sum(count))
mn_summary <- mn.accuracy %>%
  group_by(county) %>%
  summarise(mn.accuracy = sum(count*mn.accuracy)/sum(count))


lda_summary
qda_summary
mn_summary

ggplot() +
  geom_point(aes(x = lda_summary$county, y = lda_summary$lda.accuracy,color = "lda")) +
  geom_point(aes(x = qda_summary$county, y = qda_summary$qda.accuracy,color = "qda")) +
  geom_point(aes(x = mn_summary$county, y = mn_summary$mn.accuracy,color = "multinomial")) +
  labs(title = "SoCal Accuracy by county",
       x = "",
       y = "Accuracy") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 70))