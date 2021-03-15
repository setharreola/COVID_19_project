library(mi)
library(naniar)
library(tidyverse)
library(dplyr)

output = read.csv("///Users/setharreola/Documents/COVID_19_project/data/covid_data_unimputed_03_11.csv")

# socal data
socal_counties <- c("San Bernardino County","Riverside County", "Imperial County",
                    "Los Angeles County","Ventura County", "Orange County", "San Diego County")
output <- output %>%
  filter(state == "California",
         county %in% socal_counties)
###

#covid_data %>%   slice(which(select(., county, state) %IN% top_100_counties))

# most two pop counties per state data # updated
library(Rfast)
our_states <- unique(covid_data$state)

output <- covid_data %>%
  filter(state == our_states[1]) 
n <- length(unique(output$population))
output <- output %>%
  filter(population == sort(unique(output$population))[n-1] | population == sort(unique(output$population))[n-2])

for(i in 2:17){
  counties1 <- covid_data %>%
    filter(state == our_states[i]) 
  n <- length(unique(counties1$population))
  counties1 <- counties1 %>%
    filter(population == sort(unique(counties1$population))[n] | population == sort(unique(counties1$population))[n-1])
  output <- rbind(output, counties1)
}

counties1 <- covid_data %>%
  filter(state == our_states[18]) 
n <- length(unique(counties1$population))
counties1 <- counties1 %>%
  filter(county == "Caddo County" | county == "Orleans County")
output <- rbind(output, counties1)

for(i in 19:29){
  counties1 <- covid_data %>%
    filter(state == our_states[i]) 
  n <- length(unique(counties1$population))
  counties1 <- counties1 %>%
    filter(population == sort(unique(counties1$population))[n] | population == sort(unique(counties1$population))[n-1])
  output <- rbind(output, counties1)
}
counties1 <- covid_data %>%
  filter(state == our_states[30]) 
n <- length(unique(counties1$population))
counties1 <- counties1 %>%
  filter(population == sort(unique(counties1$population))[n-3] | population == sort(unique(counties1$population))[n-2])
output <- rbind(output, counties1)

for(i in 31:50){
  counties1 <- covid_data %>%
    filter(state == our_states[i]) 
  n <- length(unique(counties1$population))
  counties1 <- counties1 %>%
    filter(population == sort(unique(counties1$population))[n] | population == sort(unique(counties1$population))[n-1])
  output <- rbind(output, counties1)
}
####
# county check
old_covid_data <- read.csv("///Users/setharreola/Documents/COVID_19_project/data/100_county_covid_data_imputed_03_01.csv")
our_counties <- unique(old_covid_data$county)
our_states <- unique(old_covid_data$state)

county_check <- data.frame(unique(output$county),unique(old_covid_data$county))



### imputation section 

unique(output$county)


vis_miss(output)

# make county and date factors

missing = missing_data.frame(as.data.frame(output))

hist(missing)

imputations = mi(missing, n.iter=5)


imputed_data = mi::complete(imputations, m=1)

imputed_data = imputed_data %>% 
  select(-ncol(.))
#write.csv(imputed_data, "")
