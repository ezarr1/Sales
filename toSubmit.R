source("Library.R")
#------------- Reading of files ------------------------------------------------####

oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')
transactions <- transactions.original
train.original = read_csv('Data/train.csv')
train.original <- train.original %>% filter(date>"2015-01-01")

#Cleaning the oil DF / giving NAs the values of the following day 
oil <- oil.original
oil <- oil %>% rename(oil_price= dcoilwtico)
oil <- oil %>%  fill(oil_price)
oil[1,2] <- oil[2,2] 

#Cleaning the holiday DF
holiday <- holiday.original 
holiday <- holiday %>% mutate(flag_holiday= case_when(
  transferred==FALSE & type=="Holiday" ~ 1,
  transferred==TRUE & type=="Holiday" ~ 0,
  type=="Transfer" ~ 1,
  type=="Event" ~ 1,
  type=="Bridge" ~ 1,
  type=="Work Day" ~ 0,
  type=="Additional" ~ 2
))
holiday <- holiday %>% group_by(date) %>% summarise(flag_holiday= ifelse(1 %in% flag_holiday, 1, ifelse(2 %in% flag_holiday,2,0)))



train <- train.original
train <- train %>% arrange(family, store_nbr, date)
train <- train %>% group_by(family, store_nbr) %>% mutate(lag_14 = lag(sales, 14)) %>% ungroup()
train <- train %>% group_by(family, store_nbr) %>% mutate(lag_21 = lag(sales, 21)) %>% ungroup()
train <- train %>%
  mutate(dayofweek = weekdays(train$date),
         monthofyear = months(train$date),
         year = year(train$date),
         day= as.numeric(format(train$date, format = "%d")),
         payingDays = case_when(
           str_detect(monthofyear, "novembre|aprile|giugno|settembre") & day == 30 ~ 1,
           str_detect(monthofyear, "febbraio") & day == 28 ~ 1,
           day==31 ~ 1,
           TRUE ~ 0   
         )
  )
train <- train %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
train <- train %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

train_df <- train_df %>% left_join(oil, by='date')
train_df <- train_df %>%  fill(oil_price)
test_df <- test_df %>% mutate(oil_price=0)

oil_test <- test_df %>% select(date) %>% distinct() %>% left_join(oil, by="date")
oil_test  <- oil_test %>% fill(oil_price)
oil_train <- oil %>% filter(date<="2017-08-01")

#We change the oil_price column into predicted values insted of "known" ones
train_prophet <- oil_train %>% rename(ds=date, y=oil_price)
train_prophet <- prophet(train_prophet, daily.seasonality=TRUE)
forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
forecasting <- predict(train_prophet, forecasting)
oil_test <- left_join(oil_test,forecasting %>% select(ds,yhat), by = c("date"='ds'))
#rmsle(oil_test$oil_price, oil_test$yhat) #0.040
oil_test <- oil_test %>% select(-oil_price) %>% rename(oil_price = yhat)
test_df <- test_df %>% select(-oil_price) %>% left_join(oil_test, by="date")

# options(repr.plot.width = 60, repr.plot.height = 30)
# c1 <- cor(train_df %>%
#             select(sales, onpromotion, year, oil_price, flag_holiday, payingDays, day))
# corrplot(c1,
#          method = c('color'),
#          addCoef.col = "black",
#          addgrid.col = "black",
#          tl.col = "black",
#          order = 'hclust')


#Function to fit a linear model for a specific family and store combination
fit_lm <- function(subset_df) {
  model <- lm(sales ~  lag_14 + lag_21 + onpromotion + oil_price + flag_holiday + payingDays, data = subset_df)
  return(list(model = model))
}

# Use the by function to split the data and apply the function to each group
models <- lapply(split(train_df, train_df[c('family', 'store_nbr')]), fit_lm)

predictions <- list()# Iterate over the models and make predictions on the test data

family_store_names <- names(models)
dist_store_fam <- train %>% select(family, store_nbr) %>% distinct()
prevision <- data.frame(sales=numeric(0), p=numeric(0))
for (name in family_store_names) {  
  fam <- strsplit(name, "\\.")[[1]][1]
  store <- strsplit(name, "\\.")[[1]][2]    # Filter the test data for the specific family and store  
  store=as.numeric(store)
  subset_test_df <- subset(test_df, family == fam & store_nbr == store)    # Use the corresponding model to make predictions  
  predictions <- predict(models[[name]]$model, newdata = subset_test_df) %>%
    as.data.frame() %>%
    cbind(subset_test_df) %>%
    rename(p = '.') %>%
    mutate(p = ifelse(p < 0, 0, p))
  prevision <- prevision %>% rbind(predictions %>% select(sales, p))
}

error_17th_method <- rmsle(prevision$sales, prevision$p)
message("17th method: One linear model for each store and family that considers lags, oil price, and holidays. \n Mean error: ", error_17th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )




