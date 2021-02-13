### SoCal COVID case prediction ###

# The goal here is to find the Average MSE for each method
# accross our socal counties

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

#socal_counties_imputed <- read_csv("///Users/setharreola/Documents/COVID-19/socal_data_1_22_2021.csv")
socal_counties_imputed <- read_csv("~/socal_data_1_22_2021.csv")
covid_data <- socal_counties_imputed

# remove unnecessary column if needed 
covid_data <- covid_data[,-1]

# remove unused predictors
covid_data <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))

# save dates
covid_dates <- unique(covid_data$date)

# remove date 
covid_data <- covid_data %>%
  dplyr::select(-c(date))

# save counties 
counties <- unique(covid_data$county)




### RIDGE 
ridge_predictions <- data.frame(matrix(nrow = length(covid_dates),ncol = length(counties)))
#ridge_predictions[,1] <- unique(covid_data$date)
#ridge_predictions[,1] <- covid_dates
ridge_predictions[,1:7] <- 0
ridge_mse <- rep(0,length(counties))
for(i in 1:7){
  test_county <- counties[i]
  remaining_counties <- counties[-i]
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
  ridge_mse[i] <- ridge2_test_county_mse
  ridge_predictions[,i] <- ridge_test_county_pred
}
average_ridge_mse <- mean(ridge_mse)


### Lasso 
lasso_predictions <- data.frame(matrix(nrow = length(covid_dates),ncol = length(counties)))
#ridge_predictions[,1] <- unique(covid_data$date)
#lasso_predictions[,1] <- covid_dates
lasso_predictions[,1:8] <- 0
lasso_mse <- rep(0,length(counties))
for(i in 1:7){
  test_county <- counties[i]
  remaining_counties <- counties[-i]
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
  lasso_mse[i] <- lasso_test_county_mse
  lasso_predictions[,i] <- lasso_test_county_pred
}
average_lasso_mse <- mean(lasso_mse)




#### Poisson
covid_data[(covid_data$cases<0),"cases"]=0
poisson_predictions <- data.frame(matrix(nrow = length(y)/length(counties),ncol = length(counties)))
#poisson_predictions[,1] <- unique(covid_data$date)
#poisson_predictions[,1] <- covid_dates
poisson_predictions[,1:8] <- 0
poisson_mse <- rep(0,length(counties))
for(i in 1:7){
  test_county <- counties[i]
  remaining_counties <- counties[-i]
  train_data = covid_data[covid_data$county %in% remaining_counties,]
  test_county_data = covid_data[covid_data$county %in% test_county,]
  glm.fit = glm(cases~., data=train_data[,-1], family="poisson")
  glm.probs = predict(glm.fit, test_county_data[,-1], type="response")
  glm.mse = mean((test_county_data$cases-glm.probs)^2)
  poisson_mse[i] <- glm.mse
  #poisson_predictions[,i] <- glm.probs
}
average_poisson_mse <- mean(poisson_mse)

#results <- data.frame(matrix(nrow = 7,ncol = 4))
#colnames(results) <- c("County","Ridge_MSE","Lasso_MSE","Poisson_MSE")
#results[,1] <- counties
#results[,2] <- ridge_mse
#results[,3] <- lasso_mse
#results[,4] <- poisson_mse


####### GLARMA for each county #######
# Remove negative values from cases, replace with zero
covid_data[(covid_data$cases<0),"cases"]=0

# we have todo  each county seperatly as they will need different lags

# Imperial County
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

# LA county
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
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = 2,residuals = "Pearson" )
glarma.mse = mean((la.data$cases-fitted.glarma(glarma.fit))^2)
la_glarma_mse <- glarma.mse

# Orange county
oc = c("Orange County")
oc.data = covid_data[covid_data$county %in% oc,]
oc.data = oc.data[,-1]
x = model.matrix(cases~., oc.data)
y = oc.data$cases
#oc_dates = covid_data$date[covid_data$county %in% oc]
# Finding the optimal lags via testing their significance 
p = pacf(oc.data$cases,lag.max = 50)
a = acf(oc.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 2,thetaLags = 1,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
oc_glarma_mse <- glarma.mse

# Riverside County
rs = c("Riverside County")
rs.data = covid_data[covid_data$county %in% rs,]
rs.data = rs.data[,-1]
x = model.matrix(cases~., rs.data)
y = rs.data$cases
#rs_dates = covid_data$date[covid_data$county %in% rs]
# Finding the optimal lags via testing their significance 
p = pacf(rs.data$cases,lag.max = 50)
a = acf(rs.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = c(6,7),thetaLags = 1,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
rs_glarma_mse <- glarma.mse

# San Bernardino County
sb = c("San Bernardino County")
sb.data = covid_data[covid_data$county %in% sb,]
sb.data = sb.data[,-1]
x = model.matrix(cases~., sb.data)
y = sb.data$cases
#sb_dates = covid_data$date[covid_data$county %in% sb]
# Finding the optimal lags via testing their significance 
p = pacf(sb.data$cases,lag.max = 50)
a = acf(sb.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 1,thetaLags = c(3,4),residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
sb_glarma_mse <- glarma.mse

# San Diego County
sd = c("San Diego County")
sd.data = covid_data[covid_data$county %in% sd,]
sd.data = sd.data[,-1]
x = model.matrix(cases~., sd.data)
y = sd.data$cases
#sd_dates = covid_data$date[covid_data$county %in% sd]
# Finding the optimal lags via testing their significance 
p = pacf(sd.data$cases,lag.max = 50)
a = acf(sd.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 2,thetaLags = 4,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
sd_glarma_mse <- glarma.mse

# Ventura County
vc = c("Ventura County")
vc.data = covid_data[covid_data$county %in% vc,]
vc.data = vc.data[,-1]
x = model.matrix(cases~., vc.data)
y = vc.data$cases
#vc_dates = covid_data$date[covid_data$county %in% vc]
# Finding the optimal lags via testing their significance 
p = pacf(vc.data$cases,lag.max = 50)
a = acf(vc.data$cases,lag.max = 50)
# philags = (1,7)
# thetalags = (1,7)
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 7,thetaLags = 14,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
vc_glarma_mse <- glarma.mse

# results
glarma_mse <- c(ic_glarma_mse,la_glarma_mse,oc_glarma_mse,rs_glarma_mse,sb_glarma_mse,sd_glarma_mse,vc_glarma_mse)
average_glarma_mse <- mean(glarma_mse)

results <- data.frame(matrix(nrow = 7,ncol = 5))
colnames(results) <- c("County","Ridge_MSE","Lasso_MSE","Poisson_MSE","Glarma_MSE")
results[,1] <- counties
results[,2] <- ridge_mse
results[,3] <- lasso_mse
results[,4] <- poisson_mse
results[,5] <- glarma_mse
options(scipen=999)

results[,2:5] <- sqrt(results[,2:5])

library(scales)

results %>% 
  set_names(c("County", "Ridge", "Lasso", "Poisson", "Glarma")) %>% 
  pivot_longer(cols = c("Ridge", "Lasso", "Poisson", "Glarma"),
               names_to = "method", values_to = "MSE") %>% 
  ggplot() +
  geom_point(aes(x = County, y = MSE, col = method)) +
  geom_line(aes(x = County, y = MSE, col = method, group = method)) +
  theme_bw() +
  xlab("")+
  ylab("Root Mean Squared Error")+
  ggtitle("Comparison of Method Performance via RMSE")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(trans = log2_trans())

#results %>% 
#  set_names(c("County", "Ridge", "Lasso", "Poisson", "Glarma")) %>% 
#  pivot_longer(cols = c("Ridge", "Lasso", "Poisson", "Glarma"),
#               names_to = "method", values_to = "MSE") %>% 
#  ggplot() +
#  geom_point(aes(x = County, y = MSE, col = method)) +
#  geom_line(aes(x = County, y = MSE, col = method, group = method)) +
#  theme_bw() +
#  xlab("")+
#  ylab("Inverse Root Mean Squared Error")+
#  ggtitle("Comparison of Method Performance via Inverse RMSE")+
#  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
#  scale_y_continuous(trans = log2_trans())


# So we see that lasso does better in riverside and ventura
# looking at Riverside lasso plot
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
lasso_mse <- lasso2_test_county_mse


rs_df = data.frame(date=as.Date(covid_dates),pred=lasso_test_county_pred,
                   actual=actual_test_county_y)
colnames(rs_df) <- c("date","pred","actual")

ggplot(data=rs_df) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County, CA\nfrom 2/24/2020 to 11/29/2020 (LASSO) MSE: 95997.32 (SoCal Data)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") 


# Now compared with Riverside Glarma plot
rs = c("Riverside County")
rs.data = covid_data[covid_data$county %in% rs,]
rs.data = rs.data[,-1]
x = model.matrix(cases~., rs.data)
y = rs.data$cases
#rs_dates = covid_data$date[covid_data$county %in% rs]
rs_dates <- covid_dates
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = c(6,7),thetaLags = 1,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)

rs_df = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
ggplot(data=rs_df) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County, CA\nfrom 2/24/2020 to 11/29/2020 (Glarma) MSE: 103606.5") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")

# now lets looks at ventura
# looking at Ventura County lasso plot
vc = c("Ventura County")
remaining_counties = unique(covid_data$county)[-7]
x = model.matrix(cases~., covid_data)
y = covid_data$cases
cv.out = cv.glmnet(x[covid_data$county %in% remaining_counties,],y[covid_data$county %in% remaining_counties],alpha=1)
bestlam=cv.out$lambda.min
vc_y = covid_data$cases[covid_data$county %in% vc]
vc_dates = covid_data$date[covid_data$county %in% vc]
lasso.vc.pred = round(predict(cv.out$glmnet.fit,s=bestlam,newx=x[covid_data$county %in% vc,]))
lasso.vc.mse = mean((lasso.vc.pred-vc_y)^2)
vc_df = data.frame(date=as.Date(vc_dates),lasso.vc.pred, actual=vc_y)

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
lasso_mse <- lasso_test_county_mse

vc_df = data.frame(date=as.Date(covid_dates),lasso_test_county_pred, actual=actual_test_county_y)
ggplot(data=vc_df) +
  geom_line(aes(x=date, y=X1,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventrua County, CA\nfrom 2/24/2020 to 11/29/2020 (LASSO) MSE: 11911.98 (SoCal Data)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") 

# now compared with glarma
vc = c("Ventura County")
vc.data = covid_data[covid_data$county %in% vc,]
vc.data = vc.data[,-1]
x = model.matrix(cases~., vc.data)
y = vc.data$cases
vc_dates = covid_data$date[covid_data$county %in% vc]
glarma.fit = glarma(y,x,type = "Poi", method = "FS", phiLags = 7,thetaLags = 14,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
vc_df = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
ggplot(data=vc_df) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Ventura County, CA\nfrom 2/24/2020 to 11/29/2020 (Glarma) MSE: 125700.8") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


##### finally lets consider Orange couty, our universitys home county, and a 
# county which does not have a substantial amounty of 0 case obs. ####

# Poisson Method 1
oc = c("Orange County")
remaining_counties = unique(covid_data$county)[-3]
train.data = covid_data[covid_data$county %in% remaining_counties,]
orange.data = covid_data[covid_data$county %in% oc,]
glm.fit = glm(cases~., data=train.data[,-1], family="poisson")
glm.probs = predict(glm.fit, orange.data[,-1], type="response")
glm.mse = mean((orange.data$cases-glm.probs)^2)
orange_y = orange.data$cases
orange_dates = covid_dates

orange_df_1 = data.frame(date=as.Date(orange_dates),pred=glm.probs,
                       actual=orange_y)

ggplot(data=orange_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  #xlab("Date") + ylab("COVID-19 Cases Reported") +
  #ggtitle(" **POISSON** Reported Cases of COVID-19 in Orange County, CA\nfrom 2/24/2020 to 11/29/2020") +
  labs(title = "<span style='color:#F8766D;'>POISSON</span>  \nReported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(
    plot.title = element_markdown(lineheight = 1.1))

ggplot(data=orange_df_1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred,yend=dplyr::lead(pred),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=actual,yend=dplyr::lead(actual), colour="blue"),lwd=1) +
  labs(title = "<span style='color:#F8766D;'>POISSON</span>  \nReported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_manual(name="Reported Cases:",values=c("red"="#F8766D","blue"="#00BFC4"), labels=c("Actual", "Predicted")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(plot.title = element_markdown(lineheight = 1.1))

ggplot(data=orange_df_1) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County, CA\nfrom 2/24/2020 to 11/29/2020 (Poisson) MSE: 65852.24") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


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
ridge_mse <- ridge2_test_county_mse

orange_df_2 = data.frame(date=as.Date(covid_dates), pred=ridge_test_county_pred, actual=actual_test_county_y)
colnames(orange_df_2) <- c("date","pred","actual")

ridge_OC_plot <- ggplot(data=orange_df_2) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred,yend=dplyr::lead(pred),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=actual,yend=dplyr::lead(actual), colour="blue"),lwd=1) +
  labs(title = "<span style='color:yellow3;'>RIDGE</span>  \nReported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_manual(name="Reported Cases:",values=c("red"="yellow3","blue"="#00BFC4"), labels=c("Actual", "Predicted")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(plot.title = element_markdown(lineheight = 1.1))

ggplot(data=orange_df_2) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County, CA\nfrom 2/24/2020 to 11/29/2020 (Ridge) MSE: 47737.51") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")

ggplot(data=orange_df_2) +
  geom_line(aes(x=date, y=X1,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Orange County, CA\nfrom 2/24/2020 to 11/29/2020 (RIDGE) MSE: 48396.585 (SoCal Data)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")

# lasso
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
lasso_mse <- lasso_test_county_mse


orange_df_3 = data.frame(date=as.Date(covid_dates),pred = lasso_test_county_pred, actual=actual_test_county_y)
colnames(orange_df_3) <- c("date","pred","actual")

ggplot(data=orange_df_3) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County, CA\nfrom 2/24/2020 to 11/29/2020 (Lasso) MSE: 48313.24") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


lasso_OC_plot <- ggplot(data=orange_df_3) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred,yend=dplyr::lead(pred),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=actual,yend=dplyr::lead(actual), colour="blue"),lwd=1) +
  labs(title = "<span style='color:mediumpurple2;'>LASSO</span>  \nReported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_manual(name="Reported Cases:",values=c("red"="mediumpurple2","blue"="#00BFC4"), labels=c("Actual", "Predicted")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(plot.title = element_markdown(lineheight = 1.1))

ggplot(data=orange_df_3) +
  geom_line(aes(x=date, y=X1,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Orange County, CA\nfrom 2/24/2020 to 11/29/2020 (LASSO) MSE: 44821.55 (SoCal Data)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") 


#Glarma
oc = c("Orange County")
oc.data = covid_data[covid_data$county %in% oc,]
oc.data = oc.data[,-1]
x = model.matrix(cases~., oc.data)
y = oc.data$cases
#oc_dates = covid_data$date[covid_data$county %in% oc]
glarma.fit = glarma(y,x,type = "Poi", method = "FS",phiLags = 2 ,thetaLags = 1,residuals = "Pearson" )
glarma.mse = mean((oc.data$cases-fitted.glarma(glarma.fit))^2)
oc_glarma_mse <- glarma.mse
orange_df_4 = data.frame(date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                       actual=y)
ggplot(data=orange_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in OC County, CA\nfrom 2/24/2020 to 11/29/2020 (Glarma) MSE: 37559.14") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")

ggplot(data=orange_df_4) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=pred,yend=dplyr::lead(pred),colour="red"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=actual,yend=dplyr::lead(actual), colour="blue"),lwd=1) +
  labs(title = "<span style='color:chocolate2;'>GLARMA</span>  \nReported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_manual(name="Reported Cases:",values=c("red"="chocolate2","blue"="#00BFC4"), labels=c("Actual", "Predicted")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") 

ggplot(data=orange_df_4) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Orange County, CA\nfrom 2/24/2020 to 11/29/2020 (LASSO) MSE: 44821.55 (SoCal Data)") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") 



method <- rep(0,length(covid_dates))
for(i in 1:length(covid_dates)){
  method[i] <- "Poisson"
}
orange_df_1 <- cbind(orange_df_1,method)

method <- rep(0,length(covid_dates))
for(i in 1:length(covid_dates)){
  method[i] <- "Ridge"
}
orange_df_2 <- cbind(orange_df_2,method)

method <- rep(0,length(covid_dates))
for(i in 1:length(covid_dates)){
  method[i] <- "Lasso"
}
orange_df_3 <- cbind(orange_df_3,method)

method <- rep(0,length(covid_dates))
for(i in 1:length(covid_dates)){
  method[i] <- "GLARMA"
}
orange_df_4 <- cbind(orange_df_4,method)

orange_df <- rbind(orange_df_1,orange_df_2,orange_df_3,orange_df_4)

# cool animation
anim_a <- ggplot(data=orange_df) +
  geom_line(aes(x=date, y=pred,color="brown1"),lwd=1) +
  geom_line(aes(x=date, y=actual,color="dodgerblue1"),lwd=1) +
  labs(title = "Method: {closest_state}  \nReported Cases of COVID-19 in Orange County, CA \n from 2/24/2020 to 11/29/2020",
       x = "Date", y = "COVID-19 Cases Reported") +
  scale_color_discrete(name = "Reported Cases:", labels = c("Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month") +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  transition_states(method,state_length = 4)
  
anim_a
animation_to_save <- anim_a + exit_shrink()
anim_save("first_saved_animation.gif", animation = animation_to_save)
