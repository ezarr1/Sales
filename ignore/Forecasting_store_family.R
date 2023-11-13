source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
train <- train.original
test <- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train <- train %>% filter(date<="2017-08-01")

#------------- Mean baseline: 0.54 ------------------------------------------####
train <- train  %>% group_by(store_nbr, family)  %>% mutate(mean_sale = mean(sales))
mean_store_family <- train  %>% select(store_nbr, family, mean_sale) %>% distinct()
test <- test %>% left_join(mean_store_family, by=c("store_nbr", "family"))

model <- lm(sales ~ 0+ mean_sale, data=train)
prevision <- predict(model, newdata = test) %>% as.data.frame()
prevision <- test %>% cbind(prevision)

#We calculate for each combination of family and store the error
dist_families_stores <- train %>% select(family, store_nbr) %>% distinct()
result <- dist_families_stores %>%
  group_by(family, store_nbr) %>%
  summarize(errore = rmsle(prevision$sales[prevision$family == family & prevision$store_nbr == store_nbr],
                           prevision$mean_sale[prevision$family == family & prevision$store_nbr == store_nbr]))

error_13th_method <- mean(result$errore)
print(paste0("The minimun error is: ", min(result$errore)))
print(paste0("The maximun error is: ", max(result$errore)))

#------------- Prophet: Takes too long, can't process ------------------------------------------####
# Prophet
# dist_families_stores <- train %>% select(family, store_nbr) %>% distinct()
# result <- dist_families_stores %>% mutate(errore =0)
# train <- train %>% rename(ds = date, y = sales)
# for(fam in dist_families_stores$family){
#   for(store in dist_families_stores$store_nbr){
#     train_filtered <- train %>% filter(family == fam & store_nbr == store) 
#     train_prophet <- prophet(train_filtered)
#     forecasting <- make_future_dataframe(train_prophet, periods = 14)# date + 14 giorni 
#     forecasting <- predict(train_prophet, forecasting)
#     prevision <- left_join(forecasting %>% select(ds,yhat), test, by = c('ds'='date'))
#     result$errore[result$family == fam & result$store_nbr == store]  <-  rmsle(prevision$sales, prevision$yhat)
#   }
# }
library(purrr)
# Rename columns for compatibility with prophet
train <- train %>% rename(ds = date, y = sales)

# Define a function to apply Prophet to a subset of data
prophet_function <- function(subset) {
  model <- prophet(subset)
  future <- make_future_dataframe(model, periods = 14)
  forecast <- predict(model, future)
  return(forecast %>% select(ds, yhat))
}

# Apply the function to each combination of 'family' and 'store_nbr'
forecasts_list <- dist_families_stores %>%
  pmap(function(family, store_nbr) {
    subset <- train %>% filter(family == family & store_nbr == store_nbr)
    prophet_function(subset)
  })

# Combine the forecasts
all_forecasts <- bind_rows(forecasts_list, .id = "index")

# Merge with test data
prevision <- left_join(all_forecasts, test, by = c('ds' = 'date'))

# Calculate RMSLE for each combination of 'family' and 'store_nbr'
result <- dist_families_stores %>%
  mutate(errore = pmap_dbl(list(family, store_nbr), function(family, store_nbr) {
    rmsle(prevision$sales[prevision$family == family & prevision$store_nbr == store_nbr],
          prevision$yhat[prevision$family == family & prevision$store_nbr == store_nbr])
  }))


#------------- Lineal regression: that also takes too long  ------------------------------------------####
library(purrr)
train <- train.original
test <- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train <- train %>% filter(date<="2017-08-01")

linear_regression_function <- function(subset, test_subset) {
  model <- lm(sales ~ onpromotion, data = subset)
  predictions <- predict(model, newdata = test_subset)
  return(data.frame(date = test_subset$date, yhat = predictions))
}

# Apply the function to each combination of 'family' and 'store_nbr'
linear_reg_forecasts_list <- dist_families_stores %>%
  pmap(function(family, store_nbr) {
    subset <- train %>% filter(family == family & store_nbr == store_nbr)
    test_subset <- test %>% filter(family == family & store_nbr == store_nbr)
    linear_regression_function(subset, test_subset)
  })

# Combine the forecasts
linear_reg_all_forecasts <- bind_rows(linear_reg_forecasts_list, .id = "index")

# Merge with test data
prevision_linear_reg <- left_join(linear_reg_all_forecasts, test, by = c('ds' = 'date'))

# Calculate RMSLE for each combination of 'family' and 'store_nbr'
result_linear_reg <- dist_families_stores %>%
  mutate(errore_linear_reg = pmap_dbl(list(family, store_nbr), function(family, store_nbr) {
    rmsle(prevision_linear_reg$sales[prevision_linear_reg$family == family & prevision_linear_reg$store_nbr == store_nbr],
          prevision_linear_reg$yhat[prevision_linear_reg$family == family & prevision_linear_reg$store_nbr == store_nbr])
  }))



#------------- Lineal regression: that breaks  ------------------------------------------####

install.packages("data.table")
library(data.table)
train <- train.original
test <- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train <- train %>% filter(date<="2017-08-01")

setDT(train)

fit_model <- function(sales, onpromotion) {  
  model <- lm(sales ~ onpromotion)  
  return(list(model))
}# Apply the function by group
models <- train[, fit_model(sales, onpromotion), by = .(family, store_nbr)]

# Function to make predictions using the appropriate model
predict_sales <- function(family, store_nbr) {
  model <- models[family == family & store_nbr == store_nbr]
  if (nrow(model) > 0) {
    newdata = test %>% filter(family == family & store_nbr == store_nbr)
    return(predict(model, newdata = newdata))
  } else {
    # Handle the case where there's no corresponding model
    return(NA)
  }
}

# Assuming rmsle is correctly implemented
rmsle_values <- test[, rmsle(sales, predict_sales(family, store_nbr)), by = .(family, store_nbr)]
mean_rmsle <- mean(rmsle_values$V1, na.rm = TRUE)
