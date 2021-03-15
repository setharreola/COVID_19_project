# Shiny app plots

library(caret)
library(tidyverse)
library(dplyr)
library(glmnet)
library(glarma)
library(gridExtra)
library(gtable)
library(gganimate)
library(transformr)
library(ggtext)
library(splines)


####### 100 county plots ########

output = read.csv("///Users/setharreola/Documents/COVID_19_project/data/100_county_covid_data_imputed_03_11.csv")
covid_data <- output
covid_data <- covid_data[,-1]



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


act_below_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_below_cases[i] = mean(below_avg_data[(below_avg_data$date==covid_dates[i]),"cases"])
}

act_above_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_above_cases[i] = mean(above_avg_data[(above_avg_data$date==covid_dates[i]),"cases"])
}

covid_data_all <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))

act_all_cases = rep(0,length(covid_dates))
for (i in 1:length(covid_dates)) {
  act_all_cases[i] = mean(covid_data_all[(covid_data_all$date==covid_dates[i]),"cases"])
}


comp_df = data.frame(index=1:length(covid_dates),
                     date=as.Date(covid_dates),
                     act_below = act_below_cases,
                     act_above=act_above_cases,
                     act_all=act_all_cases)


lm.actual.fit = lm(act_below~bs(index,df=20), data=comp_df)
dt = seq(1, length(covid_dates))
comp_df$below_y_fit = predict(lm.actual.fit, data.frame(index=dt))

lm.actual.fit = lm(act_above~bs(index,df=20), data=comp_df)
dt = seq(1, length(covid_dates))
comp_df$above_y_fit = predict(lm.actual.fit, data.frame(index=dt))



ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_y_fit,
                   yend=dplyr::lead(below_y_fit),color="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_y_fit,
                   yend=dplyr::lead(above_y_fit),color="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States ") +
  scale_color_manual(name="Average New Cases:",values=c("red"="#F8766D","black"="gray30"), labels=c("Below 90th Percentile", "Above 90th Percentile")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_all,
                   yend=dplyr::lead(act_all),color="black"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_manual(name="Average New Cases:",values=c("black"="gray30"), labels=c("Observed")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

above_vs_below_plot <- ggplot(data=comp_df) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=act_all,
                   yend=dplyr::lead(act_all),color="grey"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_y_fit,
                   yend=dplyr::lead(below_y_fit),color="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_y_fit,
                   yend=dplyr::lead(above_y_fit),color="blue"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the Median Income 90th Percentile \nOut of the Top 2 most Populous Counties for All 50 States ") +
  #scale_color_discrete(name = "Reported Cases:", labels=c("Counties Above 90th Percentile","All Actual Observed Counties","Counties Below 90th Percentile")) +
  scale_color_manual(name="Average New Cases:",values=c("blue"="#00BFC4","red"="#F8766D", "grey" = "grey54"), labels=c("Counties Above 90th Percentile","All Actual Observed Counties","Counties Below 90th Percentile")) +
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










##### The data for prediction plots ######
socal_counties_imputed <- read_csv("///Users/setharreola/Documents/COVID_19_project/data/socal_imputed_covid_data_03_11.csv")
covid_data <- socal_counties_imputed
# remove unnecessary column if needed 
covid_data <- covid_data[,-1]
covid_data <- covid_data[,-1]
# remove unused predictors
covid_data <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))
# save dates
covid_dates <- unique(covid_data$date)
all_covid_dates <- covid_data$date
# remove date 
covid_data <- covid_data %>%
  dplyr::select(-c(date))
# save counties 
counties <- unique(covid_data$county)

####################  Prediction Plots #####################

##### Orange county ######
# Remove negative values from cases, replace with zero
covid_data[(covid_data$cases<0),"cases"]=0

# oc plot
#oc = c("Orange County")
#oc.data = covid_data[covid_data$county %in% oc,]
#y = oc.data$cases
#orange_df= data.frame(date=as.Date(covid_dates),actual=y)


#oc_plot <- ggplot(data=orange_df, aes(x=date, y=actual)) +
#geom_vline(xintercept = as.numeric(covid_dates[287])) +
# scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1))
#geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
#  xlab("Date") + ylab("COVID-19 Cases Reported") +
#  ggtitle("Reported Cases of COVID-19 in OC County") +
#  scale_color_discrete(name = "Reported Cases:", labels = c("Cases Reported","first vac")) +
#  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
#  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Poisson 
oc = c("Orange County")
remaining_counties = unique(covid_data$county)[-3]
train.data = covid_data[covid_data$county %in% remaining_counties,]
orange.data = covid_data[covid_data$county %in% oc,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, orange.data[,-1], type="response")
oc_glm.mse = mean((orange.data$cases-glm.probs)^2)
orange_y = orange.data$cases
orange_dates = covid_dates

orange_df_1 = data.frame(date=as.Date(orange_dates),pred=glm.probs,
                         actual=orange_y)



oc_poisson <- ggplot(data=orange_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# ridge plot
test_county <- oc
remaining_counties = unique(covid_data$county)[-3]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge2_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
oc_ridge_mse <- ridge2_test_county_mse

orange_df_2 = data.frame(date=as.Date(covid_dates), pred=ridge_test_county_pred, actual=actual_test_county_y)
colnames(orange_df_2) <- c("date","pred","actual")


oc_ridge <- ggplot(data=orange_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# lasso plot
test_county <- oc
remaining_counties = unique(covid_data$county)[-3]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
oc_lasso_mse <- lasso_test_county_mse


orange_df_3 = data.frame(date=as.Date(covid_dates),pred = lasso_test_county_pred, actual=actual_test_county_y)
colnames(orange_df_3) <- c("date","pred","actual")

oc_lasso <- ggplot(data=orange_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


#Glarma
oc = c("Orange County")
oc.data = covid_data[covid_data$county %in% oc,]
oc.data = oc.data[,-1]
x = model.matrix(cases~., oc.data)
y = oc.data$cases
#oc_dates = covid_data$date[covid_data$county %in% oc]
p = pacf(oc.data$cases,lag.max = 50)
a = acf(oc.data$cases,lag.max = 50)
glarma.fit = glarma(y,x,type = "Poi", method = "FS",phiLags = 1 ,thetaLags = 4,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
oc_glarma_mse <- glarma.mse
orange_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                         actual=y)
oc_glarma <- ggplot(data=orange_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



###### Riverside County ######
# Poisson
rs = c("Riverside County")
remaining_counties = unique(covid_data$county)[-4]
train.data = covid_data[covid_data$county %in% remaining_counties,]
rs.data = covid_data[covid_data$county %in% rs,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, rs.data[,-1], type="response")
rs_glm.mse = mean((rs.data$cases-glm.probs)^2)
rs_y = rs.data$cases
rs_dates = covid_dates

rs_df_1 = data.frame(date=as.Date(rs_dates),pred=glm.probs,
                     actual=rs_y)

rs_poisson <- ggplot(data=rs_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Ridge
test_county <- rs
remaining_counties = unique(covid_data$county)[-4]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge2_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
rs_ridge_mse <- ridge2_test_county_mse

rs_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(rs_df_2) <- c("date","pred","actual")

rs_ridge <- ggplot(data=rs_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso
test_county <- rs
remaining_counties = unique(covid_data$county)[-4]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso2_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
rs_lasso_mse <- lasso2_test_county_mse


rs_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(rs_df_3) <- c("date","pred","actual")

rs_lasso <- ggplot(data=rs_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Glarma plot
rs = c("Riverside County")
rs.data = covid_data[covid_data$county %in% rs,]
rs.data = rs.data[,-1]
x = model.matrix(cases~., rs.data)
y = rs.data$cases
#rs_dates = covid_data$date[covid_data$county %in% rs]
rs_dates <- covid_dates
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = c(6,7),thetaLags = 1,residuals = "Pearson" )
rs_glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)

rs_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
rs_glarma <- ggplot(data=rs_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))



########## Ventura ################
# Poisson
vc = c("Ventura County")
remaining_counties = unique(covid_data$county)[-7]
train.data = covid_data[covid_data$county %in% remaining_counties,]
vc.data = covid_data[covid_data$county %in% vc,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, vc.data[,-1], type="response")
vc_glm.mse = mean((vc.data$cases-glm.probs)^2)
vc_y = vc.data$cases
vc_dates = covid_dates

vc_df_1 = data.frame(date=as.Date(vc_dates),pred=glm.probs,
                     actual=vc_y)

vc_poisson <- ggplot(data=vc_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Ridge
test_county <- vc
remaining_counties = unique(covid_data$county)[-7]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
vc_ridge_mse <- ridge_test_county_mse

vc_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(vc_df_2) <- c("date","pred","actual")

vc_ridge <- ggplot(data=vc_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso plot
test_county <- vc
remaining_counties = unique(covid_data$county)[-7]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
vc_lasso_mse <- lasso_test_county_mse

vc_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(vc_df_3) <- c("date","pred","actual")

vc_lasso <- ggplot(data=vc_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# glarma
vc = c("Ventura County")
vc.data = covid_data[covid_data$county %in% vc,]
vc.data = vc.data[,-1]
x = model.matrix(cases~., vc.data)
y = vc.data$cases
vc_dates = covid_data$date[covid_data$county %in% vc]
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 7,thetaLags = 14,residuals = "Pearson" )
vc_glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
vc_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
vc_glarma <- ggplot(data=vc_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


###### Imperial County #######
# Poisson
ic = c("Imperial County")
remaining_counties = unique(covid_data$county)[-1]
train.data = covid_data[covid_data$county %in% remaining_counties,]
ic.data = covid_data[covid_data$county %in% ic,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, ic.data[,-1], type="response")
ic_glm.mse = mean((ic.data$cases-glm.probs)^2)
ic_y = ic.data$cases
ic_dates = covid_dates

ic_df_1 = data.frame(date=as.Date(ic_dates),pred=glm.probs,
                     actual=ic_y)
colnames(ic_df_1) <- c("date","pred","actual")

ic_poisson <- ggplot(data=ic_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Imperial County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# Ridge
test_county <- ic
remaining_counties = unique(covid_data$county)[-1]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
ic_ridge_mse <- ridge_test_county_mse

ic_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(ic_df_2) <- c("date","pred","actual")

ic_ridge <- ggplot(data=ic_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Imperial County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso plot
test_county <- ic
remaining_counties = unique(covid_data$county)[-1]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
ic_lasso_mse <- lasso_test_county_mse

ic_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(ic_df_3) <- c("date","pred","actual")

ic_lasso <- ggplot(data=ic_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Imperial County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


# glarma
ic = c("Imperial County")
ic.data = covid_data[covid_data$county %in% ic,]
ic.data = ic.data[,-1]
x = model.matrix(cases~., ic.data)
y = ic.data$cases
#ic_dates = covid_data$date[covid_data$county %in% ic]
# Finding the optimal lags via testing their significance 
p = pacf(ic.data$cases,lag.max = 50)
a = acf(ic.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = 7,residuals = "Pearson" )
glarma.mse = mean((ic.data$cases-fitted.glarma(glarma.fit))^2)
ic_glarma_mse <- glarma.mse

ic_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
ic_glarma <- ggplot(data=ic_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

######## Los Angeles County ######

# Poisson 
la = c("Los Angeles County")
remaining_counties = unique(covid_data$county)[-2]
train.data = covid_data[covid_data$county %in% remaining_counties,]
la.data = covid_data[covid_data$county %in% la,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, la.data[,-1], type="response")
la_glm.mse = mean((la.data$cases-glm.probs)^2)
la_y = la.data$cases
la_dates = covid_dates

la_df_1 = data.frame(date=as.Date(la_dates),pred=glm.probs,
                     actual=la_y)


la_poisson <- ggplot(data=la_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in LA County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Ridge
test_county <- la
remaining_counties = unique(covid_data$county)[-2]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
la_ridge_mse <- ridge_test_county_mse

la_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(la_df_2) <- c("date","pred","actual")

la_ridge <- ggplot(data=la_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in LA County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso plot
test_county <- la
remaining_counties = unique(covid_data$county)[-2]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
la_lasso_mse <- lasso_test_county_mse

la_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(la_df_3) <- c("date","pred","actual")

la_lasso <- ggplot(data=la_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in LA County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# glarma
la = c("Los Angeles County")
la.data = covid_data[covid_data$county %in% la,]
la.data = la.data[,-1]
x = model.matrix(cases~., la.data)
y = la.data$cases
#la_dates = covid_data$date[covid_data$county %in% la]
# Finding the optimal lags via testing their significance 
p = pacf(la.data$cases,lag.max = 50)
a = acf(la.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = 6,residuals = "Pearson" )
la_glarma.mse = mean((la.data$cases-fitted.glarma(glarma.fit))^2)
la_glarma_mse <- glarma.mse

la_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
la_glarma <- ggplot(data=la_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in LA County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


############ "San Bernardino County" ##########
# Poisson 
sb = c("San Bernardino County")
remaining_counties = unique(covid_data$county)[-5]
train.data = covid_data[covid_data$county %in% remaining_counties,]
sb.data = covid_data[covid_data$county %in% sb,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, sb.data[,-1], type="response")
sb_glm.mse = mean((sb.data$cases-glm.probs)^2)
sb_y = sb.data$cases
sb_dates = covid_dates

sb_df_1 = data.frame(date=as.Date(sb_dates),pred=glm.probs,
                     actual=sb_y)


sb_poisson <- ggplot(data=sb_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Bernardino County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Ridge
test_county <- sb
remaining_counties = unique(covid_data$county)[-5]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
sb_ridge_mse <- ridge_test_county_mse

sb_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(sb_df_2) <- c("date","pred","actual")

sb_ridge <- ggplot(data=sb_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Bernardino County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso plot
test_county <- sb
remaining_counties = unique(covid_data$county)[-5]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
sb_lasso_mse <- lasso_test_county_mse

sb_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(sb_df_3) <- c("date","pred","actual")

sb_lasso <- ggplot(data=sb_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Bernardino County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# glarma
sb.data = covid_data[covid_data$county %in% sb,]
sb.data = sb.data[,-1]
x = model.matrix(cases~., sb.data)
y = sb.data$cases
#la_dates = covid_data$date[covid_data$county %in% la]
# Finding the optimal lags via testing their significance 
p = pacf(sb.data$cases,lag.max = 50)
a = acf(sb.data$cases,lag.max = 50)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = 4,residuals = "Pearson" )
sb_glarma.mse = mean((sb.data$cases-fitted.glarma(glarma.fit))^2)
sb_glarma_mse <- glarma.mse

sb_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
sb_glarma <- ggplot(data=sb_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Bernardino County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

######### "San Diego County" #########
# Poisson 
sd = c("San Diego County")
remaining_counties = unique(covid_data$county)[-6]
train.data = covid_data[covid_data$county %in% remaining_counties,]
sd.data = covid_data[covid_data$county %in% sd,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, sd.data[,-1], type="response")
sd_glm.mse = mean((sd.data$cases-glm.probs)^2)
sd_y = sd.data$cases
sd_dates = covid_dates

sd_df_1 = data.frame(date=as.Date(sd_dates),pred=glm.probs,
                     actual=sd_y)


sd_poisson <- ggplot(data=sd_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Diego County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# Ridge
test_county <- sd
remaining_counties = unique(covid_data$county)[-6]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=0)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
ridge_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
ridge_test_county_mse <- mean((ridge_test_county_pred - actual_test_county_y)^2)
sd_ridge_mse <- ridge_test_county_mse

sd_df_2 = data.frame(date=as.Date(covid_dates),pred=ridge_test_county_pred,
                     actual=actual_test_county_y)
colnames(sd_df_2) <- c("date","pred","actual")

sd_ridge <- ggplot(data=sd_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Diego County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# lasso plot
test_county <- sd
remaining_counties = unique(covid_data$county)[-6]
train_data <- covid_data[covid_data$county %in% remaining_counties,]
train_data <- train_data %>% dplyr::select(-c(county))
test_data <- covid_data[covid_data$county %in% test_county,]
test_data <- test_data %>% dplyr::select(-c(county))
x <- model.matrix(cases~., train_data)
y <- train_data$cases 
# Traning the model
cv.out <- cv.glmnet(x,y,alpha=1)
bestlam <- cv.out$lambda.min
actual_test_county_y <- test_data$cases
x <- model.matrix(cases~., test_data)
# test on county
lasso_test_county_pred <- round(predict(cv.out$glmnet.fit,s=bestlam,newx=x))
lasso_test_county_mse <- mean((lasso_test_county_pred - actual_test_county_y)^2)
sd_lasso_mse <- lasso_test_county_mse

sd_df_3 = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                     actual=actual_test_county_y)
colnames(sd_df_3) <- c("date","pred","actual")

sd_lasso <- ggplot(data=sd_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Diego County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

# glarma
sd.data = covid_data[covid_data$county %in% sd,]
sd.data = sd.data[,-1]
x = model.matrix(cases~., sd.data)
y = sd.data$cases
#la_dates = covid_data$date[covid_data$county %in% la]
# Finding the optimal lags via testing their significance 
p = pacf(sd.data$cases,lag.max = 50)
a = acf(sd.data$cases,lag.max = 50)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = 4,residuals = "Pearson" )
sd_glarma.mse = mean((sb.data$cases-fitted.glarma(glarma.fit))^2)
sd_glarma_mse <- glarma.mse

sd_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                     actual=y)
sd_glarma <- ggplot(data=sd_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Diego County") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))













############### Classification plots ######################


library(caret)
library(tidyverse)

counties <- c("San Bernardino County", "San Diego County", "Los Angeles County",
              "Orange County", "Ventura County", "Riverside County", "Imperial County")
#socal_data <- covid_data %>% filter(state == "California",
#                                    county %in% counties) %>% na.omit() %>% ungroup()
#socal_counties_imputed <- read_csv("./data/output/socal_counties_imputed.csv")

socal_data <- read_csv("///Users/setharreola/Documents/COVID_19_project/data/socal_imputed_covid_data_03_11.csv")
socal_data <- socal_data[,-1]
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

model_formula = county ~ cases + apple_avg + retail_avg + grocery_avg + parks_avg + transit_avg + work_avg + residential_avg
init_multi_model <- train(model_formula, data = socal_data_to_model, method = "multinom")

init_multi_model <- nnet::multinom(model_formula, data = socal_data_to_model)

multinom_coefs <- coef(init_multi_model) %>%
  broom::tidy() %>%
  janitor::clean_names() %>% 
  select(county = rownames, cases:residential_avg) %>% 
  pivot_longer(cols = cases:residential_avg, names_to = "variable", values_to = "coefficient") %>% 
  filter(variable != "cases")




### Imperial county ###
ic_results <- results %>%
  filter(county == "Imperial County")

ic_class_accuary <- ggplot(ic_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

ic_multinom_coefs <- multinom_coefs %>%
  filter(county == "Imperial County")

# All variables
multinom_coefs %>% 
  ggplot(aes(x = county, y = abs(coefficient), color = variable)) +
  geom_point() +
  geom_line(aes(group = variable)) +
  labs(x = "", y = "") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

### San Bernardino county ###
sb_results <- results %>%
  filter(county == "San Bernardino County")

sb_class_accuracy <- ggplot(sb_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

sb_multinom_coefs <- multinom_coefs %>%
  filter(county == "San Bernardino County")

sb_varb_imp <- ggplot(sb_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") 

### San Diego County county ###
sd_results <- results %>%
  filter(county == "San Diego County")

sd_class_accuracy <- ggplot(sd_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

sd_multinom_coefs <- multinom_coefs %>%
  filter(county == "San Diego County")

sd_varb_imp <- ggplot(sd_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") +
  theme_grey()

### Los Angeles County  ###
la_results <- results %>%
  filter(county == "Los Angeles County")

la_class_accuracy <- ggplot(la_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

la_multinom_coefs <- multinom_coefs %>%
  filter(county == "Los Angeles County")

la_varb_imp <- ggplot(la_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") +
  theme_grey()


### Orange County  ###
oc_results <- results %>%
  filter(county == "Orange County")

oc_class_accuracy <- ggplot(oc_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

oc_multinom_coefs <- multinom_coefs %>%
  filter(county == "Orange County")

oc_varb_imp <- ggplot(oc_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") +
  theme_grey()

### Ventura County ###
vc_results <- results %>%
  filter(county == "Ventura County")

vc_class_accuracy <- ggplot(vc_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

vc_multinom_coefs <- multinom_coefs %>%
  filter(county == "Ventura County")

vc_varb_imp <- ggplot(vc_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") +
  theme_grey()


### Riverside County ###
rs_results <- results %>%
  filter(county == "Riverside County")

rs_class_accuracy <- ggplot(rs_results) +
  geom_bar(aes(x = method, y = accuracy,fill = method),stat="identity") +
  coord_cartesian(ylim = c(0.55, 1)) +
  geom_text(aes(x = method,y=accuracy,label= round(accuracy,2)), vjust=1.6, color="white", size=3.5) +
  labs(title = "Classification Methods",
       x = "",
       y = "Accuracy") +
  theme_grey()

rs_multinom_coefs <- multinom_coefs %>%
  filter(county == "Riverside County")

rs_varb_imp <- ggplot(rs_multinom_coefs) +
  geom_bar(aes(x = variable, y = abs(coefficient),fill = variable),stat="identity") +
  coord_polar(theta = "x", direction=1,clip = "off",start = 0.3 ) +
  labs(title = "Mobility Variable Importance",
       x = "",
       y = "Accuracy") +
  theme_grey()




