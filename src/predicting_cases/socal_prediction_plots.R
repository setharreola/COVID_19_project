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



##### The data for prediction plots ######
socal_counties_imputed <- read_csv("///Users/setharreola/Documents/COVID_19_project/data/socal_imputed_covid_data_03_01.csv")
covid_data <- socal_counties_imputed
# remove unnecessary column if needed 
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
  ggtitle("Reported Cases of COVID-19 in OC County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in OC County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in OC County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in OC County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in Riverside County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in Riverside County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in Riverside County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in Riverside County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in Ventura County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in Ventura County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in Ventura County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in Ventura County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in Imperial County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in Imperial County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in Imperial County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in Imperial County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in LA County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in LA County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in LA County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in LA County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in San Bernardino County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in San Bernardino County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in San Bernardino County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in San Bernardino County (GLARMA)") +
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
  ggtitle("Reported Cases of COVID-19 in San Diego County (Poisson)") +
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
  ggtitle("Reported Cases of COVID-19 in San Diego County (Ridge)") +
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
  ggtitle("Reported Cases of COVID-19 in San Diego County (Lasso)") +
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
  ggtitle("Reported Cases of COVID-19 in San Diego County (GLARMA)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))


