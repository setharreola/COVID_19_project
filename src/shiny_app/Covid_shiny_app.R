########  Covid Shiny App  ########
# Shiny packages
library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(shinythemes)

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

# Load plots 
source("src/shiny_app/shiny_covid_plots.R")


method <- c("Ridge", "Lasso", "Poisson", "Glarma", "None")




########### User Interface #############
ui <- fluidPage(
  
  titlePanel("A Few Strategies for the Statistical Modeling of the COVID-19 Pandemic Data: Visualization Dashboard"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("my_county","County",choices = counties),
      selectInput("my_method","Predictive Method",choices = method)
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Predictive Plot",
                 p(strong("Choose a county in the sidebar and predictive method")),
                 plotOutput("plot1"),
                 p("In this analysis four predictive methods are used to model county cases via mobility data."),
                 p(strong("Lasso Regression")," Shrinkage/Regularization method"),
                 p(strong("Ridge Regression")," Shrinkage/Regularization method"),
                 p(strong("Poisson Regression")," Generalized linear model with respoonce variable of a Poisson distribution"),
                 p(strong("GLARMA")," Generalized Linear Auto-Regressive Moving-AverageModeling (time-seires model)"),
                 br(),
                 br(),
                 p("Compare the above plot vs the plot below, which contains the average reported cases across the top two most populated counties in the US.(Grey)"),
                 plotOutput("above_v_below_plot"),
                 p("The red and blue lines represent the disparity in cases between counties above and below the 90th median income percentile. ")
                 ), 
        tabPanel("Predictive Plot Summary",
             p("County: "),
             verbatimTextOutput("county_name"),
             p("Dates: "),
             verbatimTextOutput("dates_used"),
             p("Method: "),
             verbatimTextOutput("method_used"),
             p("Formula"),
             uiOutput("formula"),
             p("Mean Squared Error: "),
             verbatimTextOutput("summary1")),
        tabPanel("Classification",
                 p("In this analysis five classification methods are used to classify counties based on their covid-mobility data."),
                 br(),
                 p(strong("Choose a county in the sidebar to view accuracy of classification methods")),
                 plotOutput("class_plot"),
                 p(strong("Methods used:")),
                 p(strong("KNN "), "k-nearest neighbors (Non-paremetric classification algorithem"),
                 p(strong("Neural Network "), "(system on imputs and outputs determind via the method of backpropagation)"),
                 p(strong("LDA "), "Linear Discriminant Analysis"),
                 p(strong("QDA "), "Quadratic Discriminant Analysis"),
                 p(strong("Multinomial Logistic Regression "), "multi-class logistic regression"),
                 br(),
                 br(),
                 p("By examining the coefficents of the multinomial model, we can interpret the importance of individual parameters in the classificatiton process. Note in this instance Imperial County is used as a \"base case\". "),
                 p(strong("Choose a county in the sidebar to view variable importance in classification ")),
                 plotOutput("varb_plot")
        ),
        tabPanel("The Data",
                 p("In this analysis three sources of data were used:"),
                 uiOutput("google_link"),
                 uiOutput("apple_link"),
                 uiOutput("usaFacts_link"),
                 br(),
                 br(),
                 p("The data is first scrapped from these sites, combined, cleaned, and transformed through a seies of data wrangling techniques. Note, transformations were made on this data set in an attempt to account for infection periods"),
                 br(),
                 p(strong("Choose a county in the sidebar to view the finished data for said county")),
                 tableOutput("county_data")
        )
      )
    )
  )
)





######## server function(s) ########
server <- function(input, output) {
  
  output$above_v_below_plot <- renderPlot(above_vs_below_plot)
  output$plot1 <- renderPlot({
    if(input$my_county == "Orange County" & input$my_method == "Ridge"){
      oc_ridge
    }
    else if(input$my_county == "Orange County" & input$my_method == "Lasso"){
      oc_lasso
    }
    else if(input$my_county == "Orange County" & input$my_method == "Poisson"){
      oc_poisson
    }
    else if(input$my_county == "Orange County" & input$my_method == "Glarma"){
      oc_glarma
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Ridge"){
      ic_ridge
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Lasso"){
      ic_lasso
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Poisson"){
      ic_poisson
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Glarma"){
      ic_glarma
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Ridge"){
      la_ridge
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Lasso"){
      la_lasso
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Poisson"){
      la_poisson
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Glarma"){
      la_glarma
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Ridge"){
      rs_ridge
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Lasso"){
      rs_lasso
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Poisson"){
      rs_poisson
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Glarma"){
      rs_glarma
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Ridge"){
      sb_ridge
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Lasso"){
      sb_lasso
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Poisson"){
      sb_poisson
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Glarma"){
      sb_glarma
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Ridge"){
      sd_ridge
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Lasso"){
      sd_lasso
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Poisson"){
      sd_poisson
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Glarma"){
      sd_glarma
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Ridge"){
      vc_ridge
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Lasso"){
      vc_lasso
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Poisson"){
      vc_poisson
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Glarma"){
      vc_glarma
    }
  })
  output$summary1 <- renderText({
    if(input$my_county == "Orange County" & input$my_method == "Ridge"){
      paste(round(oc_ridge_mse))
    }
    else if(input$my_county == "Orange County" & input$my_method == "Lasso"){
      paste(round(oc_lasso_mse))
    }
    else if(input$my_county == "Orange County" & input$my_method == "Poisson"){
      paste(round(oc_glm.mse))
    }
    else if(input$my_county == "Orange County" & input$my_method == "Glarma"){
      paste(round(oc_glarma_mse))
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Ridge"){
      paste(round(ic_ridge_mse))
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Lasso"){
      paste(round(ic_lasso_mse))
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Poisson"){
      paste(round(ic_glm.mse))
    }
    else if(input$my_county == "Imperial County" & input$my_method == "Glarma"){
      paste(round(ic_glarma_mse))
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Ridge"){
      paste(round(la_ridge_mse))
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Lasso"){
      paste(round(la_lasso_mse))
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Poisson"){
      paste(round(la_glm.mse))
    }
    else if(input$my_county == "Los Angeles County" & input$my_method == "Glarma"){
      paste(round(la_glarma.mse))
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Ridge"){
      paste(round(rs_ridge_mse))
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Lasso"){
      paste(round(rs_lasso_mse))
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Poisson"){
      paste(round(rs_glm.mse))
    }
    else if(input$my_county == "Riverside County" & input$my_method == "Glarma"){
      paste(round(rs_glarma.mse))
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Ridge"){
      paste(round(sb_ridge_mse))
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Lasso"){
      paste(round(sb_lasso_mse))
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Poisson"){
      paste(round(sb_glm.mse))
    }
    else if(input$my_county == "San Bernardino County" & input$my_method == "Glarma"){
      paste(round(sb_glarma.mse))
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Ridge"){
      paste(round(sd_ridge_mse))
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Lasso"){
      paste(round(sd_lasso_mse))
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Poisson"){
      paste(round(sd_glm.mse))
    }
    else if(input$my_county == "San Diego County" & input$my_method == "Glarma"){
      paste(round(sd_glarma.mse))
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Ridge"){
      paste(round(vc_ridge_mse))
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Lasso"){
      paste(round(vc_lasso_mse))
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Poisson"){
      paste(round(vc_glm.mse))
    }
    else if(input$my_county == "Ventura County" & input$my_method == "Glarma"){
      paste(round(vc_glarma.mse))
    }
  })
  output$county_name <- renderText({input$my_county})
  output$method_used <- renderText({paste(input$my_method)})
  output$formula <- renderUI({
    if(input$my_method == "Poisson"){
    withMathJax("$$\\lambda=\\exp(\\alpha+\\beta x^T)$$")
    }
    else if(input$my_method == "Ridge"){
      withMathJax("$$\\sum_{i = 1}^n \\left(y_i - \\beta_0 - \\sum_{j=1}^p \\beta_j x_{ij}\\right)^2 + \\lambda \\sum_{j=1}^p \\beta_j^2$$")
    }
    else if(input$my_method == "Lasso"){
      withMathJax("$$\\sum_{i = 1}^n \\left(y_i - \\beta_0 - \\sum_{j=1}^p \\beta_j x_{ij}\\right)^2 + \\lambda \\sum_{j=1}^p |\\beta_j|$$")
    }
    else if(input$my_method == "Glarma"){
      withMathJax("$$y_t=Pois(\\lambda_t)$$
      $$\\lambda_t=\\exp(\\alpha+\\beta x^T_t+z_t)$$
      $$z_t=\\sum_{i=1}^{p} \\phi_i(z_{t-i}+e_{t-i}) + \\sum_{j=1}^{q} \\theta_i e_{t-i}$$")
    }
  })
  output$dates_used <- renderText({paste(min(covid_dates)," to ",max(covid_dates))})
  output$class_plot <- renderPlot({
    if(input$my_county == "Orange County"){
      oc_class_accuracy
    }
    else if(input$my_county == "Imperial County"){
      ic_class_accuary
    }
    else if(input$my_county == "Los Angeles County"){
      la_class_accuracy
    }
    else if(input$my_county == "Riverside County"){
      rs_class_accuracy
    }
    else if(input$my_county == "San Bernardino County"){
      sb_class_accuracy
    }
    else if(input$my_county == "San Diego County"){
      sd_class_accuracy
    }
    else if(input$my_county == "Ventura County"){
      vc_class_accuracy
    }
  })
  output$varb_plot <- renderPlot({
    if(input$my_county == "Orange County"){
      oc_varb_imp
    }
    else if(input$my_county == "Imperial County"){
      
    }
    else if(input$my_county == "Los Angeles County"){
      la_varb_imp
    }
    else if(input$my_county == "Riverside County"){
      rs_varb_imp
    }
    else if(input$my_county == "San Bernardino County"){
      sb_varb_imp
    }
    else if(input$my_county == "San Diego County"){
      sd_varb_imp
    }
    else if(input$my_county == "Ventura County"){
      vc_varb_imp
    }
  })
  google_url <- a("https://www.google.com/covid19/mobility/", href="https://www.google.com/covid19/mobility/")
  apple_url <- a("https://covid19.apple.com/mobility", href="https://covid19.apple.com/mobility")
  usaFacts_url <- a("https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/", href="https://usafacts.org/visualizations/coronavirus-covid-19-spread-map/")
  output$google_link <- renderUI({
    tagList("Google Mobility data:", google_url)
  })
  output$apple_link <- renderUI({
    tagList("Apple Mobility data:", apple_url)
  })
  output$usaFacts_link <- renderUI({
    tagList("USAFacts data:", usaFacts_url)
  })
  covid_data <- covid_data %>% mutate(date = all_covid_dates)
  covid_data <- select(covid_data, county,date,cases,deaths,cases_sum,deaths_sum,apple_avg,retail_avg,grocery_avg,parks_avg,transit_avg,work_avg,residential_avg)
  oc_data <- covid_data[covid_data$county %in% oc,]
  oc_data <- as_tibble(oc_data)
  oc_data$date <- format(oc_data$date, '%Y-%m-%d')
  rs_data <- covid_data[covid_data$county %in% rs,]
  rs_data <- as_tibble(rs_data)
  rs_data$date <- format(rs_data$date,'%Y-%m-%d')
  sb_data <- covid_data[covid_data$county %in% sb,]
  sb_data<- as_tibble(sb_data)
  sb_data$date <- format(sb_data$date,'%Y-%m-%d')
  sd_data <- covid_data[covid_data$county %in% sd,]
  sd_data <- as_tibble(sd_data)
  sd_data$date <- format(sd_data$date,'%Y-%m-%d')
  la_data <- covid_data[covid_data$county %in% la,]
  la_data <- as_tibble(la_data)
  la_data$date <- format(la_data$date,'%Y-%m-%d')
  vc_data <- covid_data[covid_data$county %in% vc,]
  vc_data <- as_tibble(vc_data)
  vc_data$date <- format(vc_data$date,'%Y-%m-%d')
  ic_data <- covid_data[covid_data$county %in% ic,]
  ic_data <- as_tibble(ic_data)
  ic_data$date <- format(ic_data$date,'%Y-%m-%d')
  output$county_data <- renderTable({
    if(input$my_county == "Orange County"){
      oc_data
    } 
    else if(input$my_county == "Riverside County"){
      rs_data
    }
    else if(input$my_county == "San Bernardino County"){
      sb_data
    }
    else if(input$my_county == "San Diego County"){
      sd_data
    }
    else if(input$my_county == "Los Angeles County"){
      la_data
    }
    else if(input$my_county == "Ventura County"){
      vc_data
    }
    else if(input$my_county == "Imperial County"){
      ic.data
    }
  })
}


###### Load Application ######
shinyApp(ui, server)



