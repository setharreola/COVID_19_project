library("keras")
library("devtools")
library("reticulate")
library(caret)


source("crossvalidation.R")

# DATA PREP
data = read.csv("../data/output/socal_counties_imputed.csv")

covid_data = data

unique_counties = unique(covid_data$county)
covid_data$county = match(covid_data$county, unique_counties)
covid_data$county = covid_data$county - 1

dates = covid_data[,match("date",names(covid_data))]
labels = strtoi(covid_data[,match("county",names(covid_data))])
labels = match(covid_data$county, unique(covid_data$county))
labels = labels-1

to_remove = -match(c("date","county",
                     "households","population","income"),names(covid_data))



data = as.matrix(covid_data[,to_remove])

train = sample(nrow(data), round(0.8*nrow(data)))

train.data = data[train,]
test.data = data[-train,]

train.dates = dates[train]
test.dates = dates[-train]

train.labels = labels[train]
test.labels = labels[-train]

train.data = as.data.frame(scale(train.data))
test.data = as.data.frame(scale(test.data))

input_shape1 = ncol(train.data)
input_shape2 = round((2/3)*input_shape1)
output_shape = length(unique(labels))


epochs = 5000
batch_size = 32

model4 <- keras_model_sequential()

initializer = initializer_random_normal(mean=0.0,
                                        stddev=sqrt(1/input_shape1), 
                                        seed=1)
model4 %>%
  layer_dense(units = input_shape1, activation=activation_linear,
              input_shape=input_shape1,
              kernel_initializer=initializer ) %>%
  layer_dense(units = input_shape1, activation=activation_relu,
              input_shape=input_shape1, 
              kernel_initializer=initializer ) %>%
  layer_dense(units = output_shape, activation = activation_softmax)

model4 %>% compile(loss=loss_sparse_categorical_crossentropy,
                   optimizer=optimizer_adam(),
                   metrics=c('accuracy'))

cv.acc = crossvalidate5(model4, train.data, train.labels, epochs, batch_size)