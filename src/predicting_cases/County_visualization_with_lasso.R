
library(ggplot2)
library(splines)
library(dplyr)
library(caret)
library(tidyverse)
library(dplyr)
library(glmnet)


output = read.csv("///Users/setharreola/Documents/COVID_19_project/data/100_county_covid_data_imputed_03_01.csv")
covid_data <- output
covid_data <- covid_data[,-1]

# if we want to scale the data
covid_data <- covid_data %>%
  mutate(cases = (10000*cases)/population)


#covid_data$county <- as.character(covid_data$county)

#save dates
covid_dates <- unique(covid_data$date)

#remove date 
#covid_data <- covid_data %>%
  #dplyr::select(-c(date))


#split date into bottom 90% and top 10%
median_income = quantile(covid_data$income, c(.90))
below_avg_data = covid_data[(covid_data$income<median_income),]
above_avg_data = covid_data[(covid_data$income>=median_income),]

below_avg_mean_pop <- mean(below_avg_data$population)
above_avg_mean_pop <- mean(above_avg_data$population)

below_avg_data <- below_avg_data %>% 
  dplyr::select(-c(state,income, population, households))
above_avg_data <- above_avg_data %>% 
  dplyr::select(-c(state,income, population, households))

below_avg_counties <- unique(below_avg_data$county)
above_avg_counties <- unique(above_avg_data$county)

### Lasso on Below avg counties 
lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(below_avg_counties)))
lasso_predictions[,1:length(below_avg_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:90){
  test_county <- below_avg_counties[i]
  remaining_counties <- below_avg_counties[-i]
  train_data <- below_avg_data[below_avg_data$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- below_avg_data[below_avg_data$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_below_cases = rowMeans(lasso_predictions)


act_below_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_below_cases[i] = mean(below_avg_data[(below_avg_data$date==covid_dates[i]),"cases"])
}


### Lasso on Above avg counties 
lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(above_avg_counties)))
lasso_predictions[,1:length(above_avg_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:10){
  test_county <- above_avg_counties[i]
  remaining_counties <- above_avg_counties[-i]
  train_data <- above_avg_data[above_avg_data$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- above_avg_data[above_avg_data$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_above_cases = rowMeans(lasso_predictions)

act_above_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_above_cases[i] = mean(above_avg_data[(above_avg_data$date==covid_dates[i]),"cases"])
}

### Lasso on all counties 

covid_data_all <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))

covid_counties<- unique(covid_data_all$county)

lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(covid_counties)))
lasso_predictions[,1:length(covid_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:100){
  test_county <- covid_counties[i]
  remaining_counties <- covid_counties[-i]
  train_data <- covid_data_all[covid_data_all$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- covid_data_all[covid_data_all$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_all_cases = rowMeans(lasso_predictions)

act_all_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_all_cases[i] = mean(covid_data_all[(covid_data_all$date==covid_dates[i]),"cases"])
}


comp_df = data.frame(index=1:length(covid_dates),date=as.Date(covid_dates),
                     below=pred_below_cases,act_below = act_below_cases,
                     above=pred_above_cases,act_above=act_above_cases,
                     pred_all=pred_all_cases,act_all=act_all_cases)

#lm.below.fit = lm(below~bs(index,df=10), data=comp_df)
#lm.act.below.fit = lm(act_below~bs(index,df=10), data=comp_df)
#lm.above.fit = lm(above~bs(index,df=10), data=comp_df)
#lm.act.above.fit = lm(act_above~bs(index,df=10), data=comp_df)
#dt = seq(1, length(covid_dates))
#comp_df$below = predict(lm.below.fit, data.frame(index=dt))
#comp_df$act_below = predict(lm.act.below.fit, data.frame(index=dt))
#comp_df$above = predict(lm.above.fit, data.frame(index=dt))
#comp_df$act_above = predict(lm.above.fit, data.frame(index=dt))
#comp_df = comp_df[-(1:10),]
ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below,
                   yend=dplyr::lead(below),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_below,
                   yend=dplyr::lead(act_below),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above,
                   yend=dplyr::lead(above),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_above,
                   yend=dplyr::lead(act_above),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred_all,
                   yend=dplyr::lead(pred_all),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_all,
                   yend=dplyr::lead(act_all),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(data=comp_df) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above,
                   yend=dplyr::lead(above),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below,
                   yend=dplyr::lead(below), colour="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States via Lasso") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



ggplot(data=comp_df) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_above_cases,
                   yend=dplyr::lead(act_above_cases),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_below_cases,
                   yend=dplyr::lead(act_below_cases), colour="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States ") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

ggplot(data=comp_df) +
  geom_line(aes(x=date, y=act_above_cases,colour="red"),lwd=1) +
  geom_line(aes(x=date, y=act_below_cases, colour="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Scaled by Population") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States ") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

################



comp_df_2 = data.frame(index=1:length(covid_dates),date=as.Date(covid_dates),
                     below=pred_below_cases,act_below = act_below_cases,
                     above=pred_above_cases,act_above=act_above_cases,
                     pred_all=pred_all_cases,act_all=act_all_cases)

lm.actual.above.fit = lm(act_above~bs(index,df=22), data=comp_df_2)
lm.actual.below.fit = lm(act_below~bs(index,df=22), data=comp_df_2)
dt = seq(1, length(covid_dates))
comp_df_2$above_fit = predict(lm.actual.above.fit, data.frame(index=dt))
comp_df_2$below_fit = predict(lm.actual.below.fit, data.frame(index=dt))
#comp_df_2$upper_conf =  ic_df$pred_fit + 1.92*sqrt(ic_df$pred_fit)
#comp_df_2$lower_conf =  ic_df$pred_fit - 1.92*sqrt(ic_df$pred_fit)
ggplot(data=comp_df_2) +
  geom_line(aes(x=date, y=below_fit,color="black"),lwd=1) +
  geom_line(aes(x=date, y=above_fit,color="red"),lwd=1) +
  #geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  #geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  #geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported (scaled by population)") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States, Smoothed") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","black"="gray30"),labels = c("Below 90th Percentile","Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))




######## Above vs Below 90th income percentile ########

library(splines)
library(caret)

## All counties
output = read.csv("///Users/setharreola/Documents/COVID_19_project/data/100_county_covid_data_imputed_03_01.csv")
covid_data <- output
covid_data <- covid_data[,-1]
covid_dates <- unique(covid_data$date)

# if we want to scale the data
#covid_data <- covid_data %>%
#  mutate(cases = (10000*cases)/population)

#remove date 
#covid_data <- covid_data %>%
#dplyr::select(-c(date))


#split date into bottom 90% and top 10%
median_income = quantile(covid_data$income, c(.90))
below_avg_data = covid_data[(covid_data$income<median_income),]
above_avg_data = covid_data[(covid_data$income>=median_income),]

below_avg_mean_pop <- mean(below_avg_data$population)
above_avg_mean_pop <- mean(above_avg_data$population)

below_avg_data <- below_avg_data %>% 
  dplyr::select(-c(state,income, population, households))
above_avg_data <- above_avg_data %>% 
  dplyr::select(-c(state,income, population, households))

below_avg_counties <- unique(below_avg_data$county)
above_avg_counties <- unique(above_avg_data$county)

### Lasso on Below avg counties 
lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(below_avg_counties)))
lasso_predictions[,1:length(below_avg_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:90){
  test_county <- below_avg_counties[i]
  remaining_counties <- below_avg_counties[-i]
  train_data <- below_avg_data[below_avg_data$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- below_avg_data[below_avg_data$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_below_cases = rowMeans(lasso_predictions)


act_below_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_below_cases[i] = mean(below_avg_data[(below_avg_data$date==covid_dates[i]),"cases"])
}


### Lasso on Above avg counties 
lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(above_avg_counties)))
lasso_predictions[,1:length(above_avg_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:10){
  test_county <- above_avg_counties[i]
  remaining_counties <- above_avg_counties[-i]
  train_data <- above_avg_data[above_avg_data$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- above_avg_data[above_avg_data$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_above_cases = rowMeans(lasso_predictions)

act_above_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_above_cases[i] = mean(above_avg_data[(above_avg_data$date==covid_dates[i]),"cases"])
}

### Lasso on all counties 

covid_data_all <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))

covid_counties<- unique(covid_data_all$county)

lasso_predictions <- as.data.frame(matrix(nrow = length(covid_dates),ncol = length(covid_counties)))
lasso_predictions[,1:length(covid_counties)] <- 0
lasso_predictions[,2] <- 0
for(i in 1:100){
  test_county <- covid_counties[i]
  remaining_counties <- covid_counties[-i]
  train_data <- covid_data_all[covid_data_all$county %in% remaining_counties,]
  train_data <- train_data %>% dplyr::select(-c(county,date))
  test_data <- covid_data_all[covid_data_all$county %in% test_county,]
  test_data <- test_data %>% dplyr::select(-c(county,date))
  x <- model.matrix(cases~., train_data)
  y <- train_data$cases 
  # Traning the model
  cv.out <- cv.glmnet(x,y,alpha=1)
  bestlam <- cv.out$lambda.min
  actual_test_county_y <- test_data$cases
  x <- model.matrix(cases~., test_data)
  # test on county
  lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
  lasso_predictions[,i] <- lasso_test_county_pred
}

pred_all_cases = rowMeans(lasso_predictions)

act_all_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_all_cases[i] = mean(covid_data_all[(covid_data_all$date==covid_dates[i]),"cases"])
}


comp_df = data.frame(index=1:length(covid_dates),date=as.Date(covid_dates),
                     below=pred_below_cases,act_below = act_below_cases,
                     above=pred_above_cases,act_above=act_above_cases,
                     pred_all=pred_all_cases,act_all=act_all_cases)

counties_below_median_90th_income_plot <- ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below,
                   yend=dplyr::lead(below),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_below,
                   yend=dplyr::lead(act_below),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


counties_above_median_90th_income_plot <- ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above,
                   yend=dplyr::lead(above),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_above,
                   yend=dplyr::lead(act_above),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

all_100_counties_plot <- ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred_all,
                   yend=dplyr::lead(pred_all),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_all,
                   yend=dplyr::lead(act_all),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Predicted (Lasso)", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

above_vs_below_lasso_plot <- ggplot(data=comp_df) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above,
                   yend=dplyr::lead(above),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below,
                   yend=dplyr::lead(below), colour="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States via Lasso") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



above_vs_below_all_plot <- ggplot(data=comp_df) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_above_cases,
                   yend=dplyr::lead(act_above_cases),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_below_cases,
                   yend=dplyr::lead(act_below_cases), colour="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States ") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

