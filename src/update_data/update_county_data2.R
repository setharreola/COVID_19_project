
# 1) Get the link to the apple data
apple_link = "https://covid19-static.cdn-apple.com/covid19-mobility-data/2103HotfixDev16/v3/en-us/applemobilitytrends-2021-03-10.csv"

# 2) Load necessary functions
source("src/update_data/functions.R")

# 3) Load the data

load_updated_data()
get_combined_data()

# 4) Remove counties that aren't merging properly
#covid_data <- raw_data
covid_data <- transformed_data
bad_merges <- covid_data %>%
  group_by(county, state, date) %>%
  summarise(count = n()) %>%
  filter(count > 1) %>%
  distinct(county, state)


covid_data <- covid_data %>% 
  slice(which(!(select(., county, state) %IN% bad_merges)))

covid_data <- covid_data %>% 
  group_by(county, state) %>% 
  filter(row_number() > 5)

# 5) Go to the imputations script to handle missing data


