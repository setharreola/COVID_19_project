## Plot for school reporter ###

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

socal_counties_imputed <- read_csv("///Users/setharreola/Documents/COVID-19/socal_data_1_22_2021.csv")
covid_data <- socal_counties_imputed
covid_data <- covid_data[,-1]

#if we want to scale the data
covid_data <- covid_data %>%
 mutate(cases = (100000*cases)/population)

covid_data <- covid_data %>% 
  dplyr::select(-c(state,income, population, households))

#save dates
covid_dates <- unique(covid_data$date)

#remove date 
covid_data <- covid_data %>%
  dplyr::select(-c(date))

counties <- unique(covid_data$county)

# Remove negative values from cases, replace with zero
covid_data[(covid_data$cases<0),"cases"]=0



# we have to specify each county seperatly as they will need different lags


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
ic_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=ic_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=ic_df)
dt = seq(1, length(covid_dates))
ic_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
ic_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
ic_df$upper_conf =  ic_df$pred_fit + 1.92*sqrt(ic_df$pred_fit)
ic_df$lower_conf =  ic_df$pred_fit - 1.92*sqrt(ic_df$pred_fit)
ic_plot =ggplot(data=ic_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported (scaled by population)") +
  ggtitle("Reported Cases of COVID-19 in Imperial County (lowest income), CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
ic_plot


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
la_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=la_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=la_df)
dt = seq(1, length(covid_dates))
la_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
la_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
la_df$upper_conf =  la_df$pred_fit + 1.92*sqrt(la_df$pred_fit)
la_df$lower_conf =  la_df$pred_fit - 1.92*sqrt(la_df$pred_fit)
la_plot =ggplot(data=la_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in LA County, CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
la_plot



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
oc_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=oc_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=oc_df)
dt = seq(1, length(covid_dates))
oc_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
oc_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
oc_df$upper_conf =  oc_df$pred_fit + 1.92*sqrt(oc_df$pred_fit)
oc_df$lower_conf =  oc_df$pred_fit - 1.92*sqrt(oc_df$pred_fit)
oc_plot =ggplot(data=oc_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported (scaled by population)") +
  ggtitle("Reported Cases of COVID-19 in Orange County, CA from 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
oc_plot




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
rs_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=rs_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=rs_df)
dt = seq(1, length(covid_dates))
rs_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
rs_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
rs_df$upper_conf =  rs_df$pred_fit + 1.92*sqrt(rs_df$pred_fit)
rs_df$lower_conf =  rs_df$pred_fit - 1.92*sqrt(rs_df$pred_fit)
rs_plot =ggplot(data=rs_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in Riverside County, CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
rs_plot




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
sb_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=sb_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=sb_df)
dt = seq(1, length(covid_dates))
sb_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
sb_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
sb_df$upper_conf =  sb_df$pred_fit + 1.92*sqrt(sb_df$pred_fit)
sb_df$lower_conf =  sb_df$pred_fit - 1.92*sqrt(sb_df$pred_fit)
sb_plot =ggplot(data=sb_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Bernardino County, CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
sb_plot




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
sd_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=sd_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=sd_df)
dt = seq(1, length(covid_dates))
sd_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
sd_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
sd_df$upper_conf =  sd_df$pred_fit + 1.92*sqrt(sd_df$pred_fit)
sd_df$lower_conf =  sd_df$pred_fit - 1.92*sqrt(sd_df$pred_fit)
sd_plot =ggplot(data=sd_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Reported Cases of COVID-19 in San Diego County, CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
sd_plot




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
vc_df = data.frame(index=1:length(covid_dates), date=as.Date(covid_dates),pred=fitted.glarma(glarma.fit),
                   actual=y)
lm.actual.fit = lm(y~bs(index,df=15), data=vc_df)
lm.pred.fit = lm(pred~bs(index,df=15), data=vc_df)
dt = seq(1, length(covid_dates))
vc_df$y_fit = predict(lm.actual.fit, data.frame(index=dt))
vc_df$pred_fit = predict(lm.pred.fit, data.frame(index=dt))
vc_df$upper_conf =  vc_df$pred_fit + 1.92*sqrt(vc_df$pred_fit)
vc_df$lower_conf =  vc_df$pred_fit - 1.92*sqrt(vc_df$pred_fit)
vc_plot =ggplot(data=vc_df) +
  geom_line(aes(x=date, y=pred_fit,color="blue"),lwd=1) +
  geom_line(aes(x=date, y=y_fit,color="red"),lwd=1) +
  geom_line(aes(x=date, y=upper_conf, color="black"),lwd=0.5,linetype=2) +
  geom_line(aes(x=date, y=lower_conf,color="black"),lwd=0.5,linetype=2) +
  geom_ribbon(aes(x=date, ymin=lower_conf, ymax=upper_conf), linetype=2, alpha=0.1)+
  xlab("Date") + ylab("COVID-19 Cases Reported (scaled by population)") +
  ggtitle("Reported Cases of COVID-19 in Ventura County (highest income), CA\nfrom 2/24/2020 to 1/19/2021 (Glarma)") +
  scale_color_manual(name = "Reported Cases:", values=c("red"="#F8766D","blue" = "skyblue3","black"="gray30"),labels = c("Confidence Interval","Predicted", "Actual")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))
vc_plot

ic_plot

grid.arrange(vc_plot, ic_plot, ncol=2)


