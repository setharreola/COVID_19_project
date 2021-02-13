library(ggplot2)
library(splines)
library(dplyr)

untransformed_data = read.csv("../data/output/untransformed_output_imputed.csv")
output = read.csv("../data/output/transformed_output.csv")
output$population = strtoi(gsub(",", "", output$population))
output$households = strtoi(gsub(",", "", output$households))
covid_data = output
covid_data$cases_ratio = covid_data$cases/covid_data$population
covid_data$deaths_ratio = covid_data$deaths/covid_data$population
median_income = quantile(output$income, c(.90))

## Bottom 90% vs Top 10%
below_avg = covid_data[(covid_data$income<median_income),]
dates = unique(below_avg$date)
n=length(dates)
dates = dates[-c(n-10,n-9,n-8,n-7,n-6,n-5,n-4,n-3,n-2,n-1,n)]
below_cases = rep(0,length(dates))
for (i in 1:length(dates)) {
  below_cases[i] = mean(below_avg[(below_avg$date==dates[i]),"cases"])
}

above_avg = covid_data[(covid_data$income>=median_income),]
above_cases = rep(0,length(dates))
for (i in 1:length(dates)) {
  above_cases[i] = mean(above_avg[(above_avg$date==dates[i]),"cases"])
}

comp_df = data.frame(index=1:length(dates),date=as.Date(dates),
                     below=below_cases, above=above_cases)
  
#Infection by Income Disparity
lm.below.fit = lm(below~bs(index,df=5), data=comp_df)
lm.above.fit = lm(above~bs(index,df=5), data=comp_df)
dt = seq(1, length(dates))
comp_df$below_fit = predict(lm.below.fit, data.frame(index=dt))
comp_df$above_fit = predict(lm.above.fit, data.frame(index=dt))
#comp_df = comp_df[-(1:10),]
ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_fit,
                   yend=dplyr::lead(below_fit),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_fit,
                   yend=dplyr::lead(above_fit),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Cases Reported") +
  ggtitle("Average Reported Cases of COVID-19 for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average New Cases:", labels=c("Bottom 90% of Counties", "Top 10% of Counties")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


# Death by Income Disparity
below_avg = covid_data[(covid_data$income<median_income),]
dates = unique(below_avg$date)
n=length(dates)
dates = dates[-c(n-10,n-9,n-8,n-7,n-6,n-5,n-4,n-3,n-2,n-1,n)]
below_deaths = rep(0,length(dates))
for (i in 1:length(dates)) {
  below_deaths[i] = mean(below_avg[(below_avg$date==dates[i]),"deaths"])
}

above_avg = covid_data[(covid_data$income>=median_income),]
above_deaths = rep(0,length(dates))
for (i in 1:length(dates)) {
  above_deaths[i] = mean(above_avg[(above_avg$date==dates[i]),"deaths"])
}

comp_df = data.frame(index=1:length(dates),date=as.Date(dates),
                     below=below_deaths, above=above_deaths)
lm.below.fit = lm(below~bs(index,df=5), data=comp_df)
lm.above.fit = lm(above~bs(index,df=5), data=comp_df)
dt = seq(1, length(dates))
comp_df$below_fit = predict(lm.below.fit, data.frame(index=dt))
comp_df$above_fit = predict(lm.above.fit, data.frame(index=dt))
ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_fit,
                   yend=dplyr::lead(below_fit),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_fit,
                   yend=dplyr::lead(above_fit),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("COVID-19 Deaths Reported") +
  ggtitle("Average COVID-19 Related Deaths for Counties Above and Below the\nMedian Income 90th Percentile Out of the Top 2 most Populous Counties\nfor All 50 States") +
  scale_color_discrete(name="Average COVID-19 Related Deaths:", labels=c("Bottom 90% of Counties", "Top 10% of Counties")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


# Infection RATIO by Income Disparity
below_avg = covid_data[(covid_data$income<median_income),]
dates = unique(below_avg$date)
n=length(dates)
dates = dates[-c(n-10,n-9,n-8,n-7,n-6,n-5,n-4,n-3,n-2,n-1,n)]
below_cases = rep(0,length(dates))
for (i in 1:length(dates)) {
  below_cases[i] = mean(below_avg[(below_avg$date==dates[i]),"cases_ratio"])
}

above_avg = covid_data[(covid_data$income>=median_income),]
above_cases = rep(0,length(dates))
for (i in 1:length(dates)) {
  above_cases[i] = mean(above_avg[(above_avg$date==dates[i]),"cases_ratio"])
}

comp_df = data.frame(index=1:length(dates),date=as.Date(dates),
                     below=below_cases, above=above_cases)
lm.below.fit = lm(below~bs(index,df=21), data=comp_df)
lm.above.fit = lm(above~bs(index,df=21), data=comp_df)
dt = seq(1, length(dates))

comp_df$below_fit = predict(lm.below.fit, data.frame(index=dt))
comp_df$above_fit = predict(lm.above.fit, data.frame(index=dt))
ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_fit,
                   yend=dplyr::lead(below_fit),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_fit,
                   yend=dplyr::lead(above_fit),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("Average Number of Cases per Population Size") +
  ggtitle("Average COVID-19 New Reported Cases Relative to Population Size for Counties Above\nand Below the Median Income 90th Percentile Out of the Top 2 most Populous\nCounties for All 50 States") +
  scale_color_discrete(name="Cases Per Pop. Size", labels=c("Bottom 90% of Counties", "Top 10% of Counties")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


# Death RATIO by Income Disparity
below_avg = covid_data[(covid_data$income<median_income),]
dates = unique(below_avg$date)
n=length(dates)
dates = dates[-c(n-10,n-9,n-8,n-7,n-6,n-5,n-4,n-3,n-2,n-1,n)]
below_deaths = rep(0,length(dates))
for (i in 1:length(dates)) {
  below_deaths[i] = mean(below_avg[(below_avg$date==dates[i]),"deaths_ratio"])
}

above_avg = covid_data[(covid_data$income>=median_income),]
above_deaths = rep(0,length(dates))
for (i in 1:length(dates)) {
  above_deaths[i] = mean(above_avg[(above_avg$date==dates[i]),"deaths_ratio"])
}

comp_df = data.frame(index=1:length(dates),date=as.Date(dates),
                     below=below_deaths, above=above_deaths)
lm.below.fit = lm(below~bs(index,df=5), data=comp_df)
lm.above.fit = lm(above~bs(index,df=5), data=comp_df)
dt = seq(1, length(dates))
comp_df$below_fit = predict(lm.below.fit, data.frame(index=dt))
comp_df$above_fit = predict(lm.above.fit, data.frame(index=dt))
ggplot((data=comp_df)) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=below_fit,
                   yend=dplyr::lead(below_fit),color="brown1"),lwd=1) +
  geom_segment(aes(x=date,xend=dplyr::lead(date), y=above_fit,
                   yend=dplyr::lead(above_fit),color="dodgerblue1"),lwd=1) +
  xlab("Date") + ylab("Average COVID-19 Related Deaths Relative to Population Size") +
  ggtitle("Average COVID-19 Related Deaths Relative to Population Size for Counties Above\nand Below the Median Income 90th Percentile Out of the Top 2 most Populous\nCounties for All 50 States") +
  scale_color_discrete(name="Deaths per Pop. Size:", labels=c("Bottom 90% of Counties", "Top 10% of Counties")) +
  scale_x_date(date_labels = "%b %d %Y", date_breaks = "1 month")


# 3-D Visualization



