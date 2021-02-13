library(mi)
library(naniar)

output = read.csv("C:/Users/cwjab/OneDrive/School/CSUF/Summer2020/202b/final project/covid/output/output.csv")

# socal data
socal_counties <- c("San Bernardino County","Riverside County", "Imperial County",
                    "Los Angeles County","Ventura County", "Orange County", "San Diego County")
output <- output %>%
  filter(state == "California",
         county %in% socal_counties)
###

#covid_data %>%   slice(which(select(., county, state) %IN% top_100_counties))

# most two pop counties per state data
library(Rfast)
our_states <- unique(covid_data$state)

output <- covid_data %>%
  filter(state == our_states[1]) 
output <- output %>%
  filter(population == nth(unique(output$population),2,descending = T))
counties1 <- covid_data %>%
  filter(state == our_states[1]) 
counties1 <- counties1 %>%
  filter(population == nth(unique(counties1$population),3,descending = T))
output <- rbind(output, counties1)

for(i in 2:29){
  counties1 <- covid_data %>%
    filter(state == our_states[i]) 
  counties1 <- counties1 %>%
    filter(population >= nth(unique(counties1$population),2,descending = T))
  output <- rbind(output, counties1)
}
counties1 <- covid_data %>%
  filter(state == our_states[30]) 
counties1 <- counties1 %>%
  filter(population == nth(unique(counties1$population),4,descending = T))
output <- rbind(output, counties1)
counties1 <- covid_data %>%
  filter(state == our_states[30]) 
counties1 <- counties1 %>%
  filter(population == nth(unique(counties1$population),3,descending = T))
output <- rbind(output, counties1)
for(i in 31:50){
  counties1 <- covid_data %>%
    filter(state == our_states[i]) 
  counties1 <- counties1 %>%
    filter(population >= nth(unique(counties1$population),2,descending = T))
  output <- rbind(output, counties1)
}
####


unique(output$county)


vis_miss(output)

# make county and date factors

missing = missing_data.frame(as.data.frame(output))

hist(missing)

imputations = mi(missing, n.iter=5)


imputed_data = complete(imputations, m=1)

imputed_data = imputed_data %>% 
  select(-ncol(.))
#write.csv(imputed_data, "")
