library(tidyverse)
library(lubridate)
library(rvest)
library(RSelenium)
library(data.table)

# Step 1 - Update Data Sources

# Get link to Apple data
# driver <- rsDriver(browser = "chrome", port = 4570L, chromever = "87.0.4280.88")
# rem_driv <- driver$client
# 
# rem_driv$open()
# rem_driv$navigate("https://covid19.apple.com/mobility")
# 
# apple_webpage <- rem_driv$getPageSource()[[1]] %>%
#   read_html()
# 
# apple_link <- apple_webpage %>%
#   html_nodes(".download-button-container a") %>%
#   html_attr("href")
# 
# rem_driv$close()
# driver$server$stop()
# rm(rem_driv, driver, apple_webpage)

# Read data into env

load_updated_data <- function(){
  covid_cases <<- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_confirmed_usafacts.csv")
  covid_deaths <<- read_csv("https://usafactsstatic.blob.core.windows.net/public/data/covid-19/covid_deaths_usafacts.csv")
  apple_data <<- read_csv(apple_link)
  
  temp <- tempfile()
  download.file("https://www.gstatic.com/covid19/mobility/Region_Mobility_Report_CSVs.zip", temp)
  google_data <<- read_csv(unz(temp, "2020_US_Region_Mobility_Report.csv"))
  unlink(temp)
  
  income_df <<- read_csv("./data/median_income.csv")
  county_metadata <<- read_csv("./data/county_metadata.csv")
  
}



# Step 2 - Combine and Transform

get_combined_data <- function(){
  g_daily <- google_data %>% 
    rename(state = "sub_region_1",
           county = "sub_region_2") %>% 
    filter(date >= "2020-02-26",
           !is.na(state),
           !is.na(county)) %>% 
    mutate(county = str_replace(county, "ñ", "n"),
           county = str_replace(county, "Parish", "County"),
           county = ifelse(str_detect(county, "ounty$"), county, paste(county, "County")),
           county = str_replace(county, "Borough County", "County"),
           county = str_replace(county, "City County", "County"),
           county = str_replace(county, "De ", "De")) %>% 
    select(county, state, date,
           retail = retail_and_recreation_percent_change_from_baseline,
           grocery = grocery_and_pharmacy_percent_change_from_baseline,
           parks = parks_percent_change_from_baseline,
           transit = transit_stations_percent_change_from_baseline,
           work = workplaces_percent_change_from_baseline,
           residential = residential_percent_change_from_baseline)
  
  a_daily <- apple_data %>% 
    pivot_longer(cols = matches("\\d"), names_to = "date", values_to = "value") %>% 
    pivot_wider(names_from = transportation_type, values_from = value) %>% 
    rename(state = "sub-region",
           county = "region") %>% 
    mutate(date = ymd(date),
           county = str_replace(county, "ñ", "n"),
           county = str_replace(county, "Parish", "County"),
           county = str_replace(county, "Municipio", "County"),
           county = ifelse(str_detect(county, "ounty$"), county, paste(county, "County")),
           county = str_replace(county, "City County", "County"),
           county = str_replace(county, "Borough County", "County"),
           county = str_replace(county, "De ", "De")) %>% 
    filter(date >= "2020-02-26",
           !is.na(state),
           !is.na(county),
           geo_type == "county",
           country == "United States") %>% 
    select(county, state, date,
           apple_driving = driving)
  
  cases_daily <- covid_cases %>% 
    pivot_longer(cols = matches("\\d"), names_to = "date", values_to = "cases_sum") %>% 
    rename(state = State,
           county = "County Name") 
  
  deaths_daily <- covid_deaths %>% 
    pivot_longer(cols = matches("\\d"), names_to = "date", values_to = "deaths_sum") %>% 
    rename(state = State,
           county = "County Name") 
  
  covid_daily <- cases_daily %>% 
    left_join(deaths_daily) %>% 
    mutate(date = ymd(date),
           state = fct_recode(state,
                              `Alaska` = "AK", `Alabama` = "AL", `Arkansas` = "AR",
                              `Arizona` = "AZ", `California` = "CA", `Colorado` = "CO",
                              `Connecticut` = "CT", `Delaware` = "DE", `Florida` = "FL",
                              `Georgia` = "GA", `Hawaii` = "HI", `Iowa` = "IA",
                              `Idaho` = "ID", `Illinois` = "IL", `Indiana` = "IN",
                              `Kansas` = "KS", `Kentucky` = "KY", `Louisiana` = "LA",
                              `Massachusetts` = "MA", `Maryland` = "MD", `Maine` = "ME",
                              `Michigan` = "MI", `Minnesota` = "MN", `Missouri` = "MO",
                              `Mississippi` = "MS", `Montana` = "MT", `North Carolina` = "NC",
                              `North Dakota` = "ND", `Nebraska` = "NE", `New Hampshire` = "NH",
                              `New Jersey` = "NJ", `New Mexico` = "NM", `Nevada` = "NV",
                              `New York` = "NY", `Ohio` = "OH", `Oklahoma` = "OK",
                              `Oregon` = "OR", `Pennsylvania` = "PA", `Rhode Island` = "RI",
                              `South Carolina` = "SC", `South Dakota`= "SD", `Tennessee` = "TN",
                              `Texas` = "TX", `Utah` = "UT", `Virginia` = "VA",
                              `Vermont` = "VT", `Washington` = "WA", `Wisconsin` = "WI",
                              `West Virginia` = "WV", `Wyoming` = "WY"),
           county = str_replace(county, "Parish", "County"),
           county = str_replace(county, "Borough", "County"),
           county = str_replace(county, "Census Area", "County"),
           county = str_replace(county, "City and ", ""),
           county = str_replace(county, "(C|c)ity", "County"),
           county = str_replace(county, "De ", "De"),
           county = str_replace(county, "County County", "County")) %>% 
    filter(date >= "2020-02-26",
           county != "Statewide Unallocated") %>% 
    select(county, state, date, cases_sum, deaths_sum)
  
  rm(cases_daily, deaths_daily)  
  
  
  
  i_g_daily <- income_df %>% 
    janitor::clean_names() %>% 
    mutate(county = str_replace(county, "Census Area County", "County"),
           county = str_replace(county, "County County", "County"),
           county = str_replace(county, "City County", "County")) %>%
    right_join(g_daily, by = c("county", "state"))
  
  i_g_a_daily <- i_g_daily %>% 
    left_join(a_daily, by = c("county", "state", "date"))
  
  combined_daily <- i_g_a_daily %>% 
    left_join(covid_daily, by = c("county", "state", "date")) %>% 
    filter(!is.na(deaths_sum),
           !is.na(cases_sum)) %>% 
    group_by(county, state) %>% 
    mutate(cases = cases_sum - lag(cases_sum),
           deaths = deaths_sum - lag(deaths_sum)) %>%
    ungroup() %>% 
    select(-rank)
  
  
  transformed_data <<- combined_daily %>% 
    filter(!is.na(deaths),
           !is.na(cases)) %>%
    group_by(state, county) %>% 
    mutate(income = median_household, households = number_of_households,
           cases_sum = rowSums(cbind(lag(cases, 5), lag(cases, 6), lag(cases, 7),
                                     lag(cases, 8), lag(cases, 9), lag(cases, 10),
                                     lag(cases, 11)), na.rm = T),
           deaths_sum = rowSums(cbind(lag(deaths, 5), lag(deaths, 6), lag(deaths, 7),
                                      lag(deaths, 8), lag(deaths, 9), lag(deaths, 10),
                                      lag(deaths, 11)), na.rm = T),
           apple_avg = rowMeans(cbind(lag(apple_driving, 5), lag(apple_driving, 6),
                                      lag(apple_driving, 7), lag(apple_driving, 8),
                                      lag(apple_driving, 9), lag(apple_driving, 10),
                                      lag(apple_driving, 11)), na.rm = T),
           retail_avg = rowMeans(cbind(lag(retail, 5), lag(retail, 6), lag(retail, 7),
                                       lag(retail, 8), lag(retail, 9), lag(retail, 10),
                                       lag(retail, 11)), na.rm = T),
           grocery_avg = rowMeans(cbind(lag(grocery, 5), lag(grocery, 6), lag(grocery, 7),
                                        lag(grocery, 8), lag(grocery, 9), lag(grocery, 10),
                                        lag(grocery, 11)), na.rm = T),
           parks_avg = rowMeans(cbind(lag(parks, 5), lag(parks, 6), lag(parks, 7),
                                      lag(parks, 8), lag(parks, 9), lag(parks, 10),
                                      lag(parks, 11)), na.rm = T),
           transit_avg = rowMeans(cbind(lag(transit, 5), lag(transit, 6), lag(transit, 7),
                                        lag(transit, 8), lag(transit, 9), lag(transit, 10),
                                        lag(transit, 11)), na.rm = T),
           work_avg = rowMeans(cbind(lag(work, 5), lag(work, 6), lag(work, 7),
                                     lag(work, 8), lag(work, 9), lag(work, 10),
                                     lag(work, 11)), na.rm = T),
           residential_avg = rowMeans(cbind(lag(residential, 5), lag(residential, 6), lag(residential, 7),
                                            lag(residential, 8), lag(residential, 9), lag(residential, 10),
                                            lag(residential, 11)), na.rm = T)) %>% 
    ungroup() %>% 
    select(county, state, income, population, households, date, cases_sum,
           deaths_sum, apple_avg, retail_avg, grocery_avg, parks_avg, transit_avg,
           work_avg, residential_avg, cases, deaths)
  
  raw_data <<- combined_daily %>% 
    filter(!is.na(deaths),
           !is.na(cases)) %>% 
    select(county, state, income = median_household, population,
           households = number_of_households, date, cases, deaths,
           apple_driving, retail, grocery, parks, transit, work, residential)
}



`%IN%` <- function(y, x) {
  tmp = rbindlist(list(x,y))
  len_ = nrow(x)
  tmp[, idx := any(.I <= len_) & .N > 1L, by=names(tmp)]
  tail(tmp$idx, nrow(y))
}
