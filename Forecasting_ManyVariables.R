source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')

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

transactions <- transactions %>% group_by(date) %>% summarise(transactions= sum(transactions))

#Adding all the info to train
train <- train.original
train <- train %>% group_by(date) %>% summarise(tot_sales = sum(sales), tot_promotions = sum(onpromotion)) %>% ungroup()
train <- train %>% left_join(oil, by='date')
train <- train %>%  fill(oil_price)
train <- train %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
train <- train %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))
train <- train %>% left_join(transactions, by=c('date')) 
train <- train %>% mutate(transactions= ifelse(is.na(transactions) & tot_sales==0, 0, transactions))
mean_transactions <- mean(train$transactions, na.rm = TRUE)
train <- train %>% mutate(transactions= ifelse(is.na(transactions), mean_transactions, transactions))

#Adding more date info
train <- train %>%
  mutate(dayofweek = weekdays(train$date),
         monthofyear = months(train$date),
         year = year(train$date),
         day= as.numeric(format(train$date, format = "%d")),
         payingDays = ifelse(day == 15 | day == 30 | day == 31, 1, 0))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

#Understanding the correlations between variables
options(repr.plot.width = 60, repr.plot.height = 30)
c1 <- cor(train_df %>%
            select(tot_sales, tot_promotions, year, oil_price, flag_holiday, payingDays, day,transactions))
corrplot(c1,
         method = c('color'),
         addCoef.col = "black",
         addgrid.col = "black",
         tl.col = "black",
         order = 'hclust')

#Error: 0.080 - no intercept
model <- lm(tot_sales ~ 0 + tot_promotions + year+ transactions + oil_price + flag_holiday + payingDays, data=train_df)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
error_tenth_method <- rmsle(prevision$tot_sales, prevision$`.`)

#Error: 0.082 - considering each variable * day
model <- lm(tot_sales ~ tot_promotions*day + transactions*day + oil_price*day + flag_holiday*day, data=train_df)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
error_eleventh_method <- rmsle(prevision$tot_sales, prevision$`.`)

#Error: 0.070 - with LAGs
train_df <- train_df  %>% mutate(lag_14 = lag(tot_sales, 14))
train_df <- train_df  %>% mutate(lag_21 = lag(tot_sales, 21))
test_df$lag_14 = tail(train_df$tot_sales, 14)
test_df$lag_21 = train_df$tot_sales[(nrow(train_df) - 20):(nrow(train_df) - 7)]

train_ts <- ts(train_df)
train_ts <- ts(train_df$tot_sales, start = c(2013, 1), frequency = 365)
fit_train_ts <- tslm(train_ts ~ 0+ tot_promotions + year+ transactions + oil_price + flag_holiday + payingDays +lag_21+lag_14, data = train_df)
test_ts <- ts(test_df$tot_sales, start = c(2013, 1), frequency = 365)
fcast <- forecast(fit_train_ts, newdata = test_df)

autoplot(fcast) +
  labs(title = "Sales Forecast", x = "Date", y = "Sales") +
  theme_minimal()

predicted_values <- as.numeric(fcast$mean)  # Extract the predicted values
error_12th_method <- rmsle(test_df$tot_sales, predicted_values)
