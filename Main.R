source("Library.R")
#------------- Reading of files ------------------------------------------------####

oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')
transactions <- transactions.original
train.original = read_csv('Data/train.csv')
train.original <- train.original %>% filter(date>"2015-01-01")
train <- train.original

#--Data grouped by date
#--Data grouped by date                       #### 
print("The following analysis were made with data grouped by date:")
#------Dataset creation         --#####
train_full_summarised <- train.original %>%
  group_by(date)%>%
  summarize(tot_sales=sum(sales))
test_full_summarised<- train_full_summarised %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_full_summarised <- train_full_summarised %>% filter(date<="2017-08-01")
#-------- 1st method: We'll sell today what we sold yesterday. Error: 0.23               -####
# We take into consideration the 01/08/2017
first_august<-train_full_summarised[[nrow(train_full_summarised),2]]
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=first_august)

# Plot 
# plot_first_method<-prevision%>%
#   ggplot(aes(x = date , y = tot_sales)) +
#   geom_line(aes(y = tot_sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
#   geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
#   scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
#   labs(title="Prevision last day 01/08/2017", x="Date", y="Total Sales (M)")
# print(plot_first_method)
error_1st_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)
message("1st method: We'll sell today what we sold yesterday. Error: ", error_1st_method)

#-------- 2nd method: Mean of all previous sales. Error: 0.14                            ----------------####
# We take into consideration the mean of the previous days
mean_sale<-mean(train_full_summarised$tot_sales)
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=mean_sale)

# Plot 
# plot_x_method<-prevision%>%
#   ggplot(aes(x = date , y = tot_sales)) +
#   geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
#   geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
#   scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
#   labs(title="Prevision average days", x="Date", y="Total Sales (M)")
# print(plot_x_method)
error_2nd_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)
message("2nd method: Mean of all previous sales. Error: ", error_2nd_method)
#-------- 3rd method: Mean of last 14 days. Error: 0.13                                  ------------------####
# We take into consideration the mean of the previous 14 days
mean_sale<-mean(tail(train_full_summarised$tot_sales,14))
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=mean_sale)

# Plot 
# plot_x_method<-prevision%>%
#   ggplot(aes(x = date , y = tot_sales)) +
#   geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
#   geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
#   scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
#   labs(title="Prevision average previous 14 days", x="Date", y="Total Sales (M)")
# print(plot_x_method)
error_3rd_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)
message("3rd method: Mean of last 14 days. Error: ", error_3rd_method)
#-------- 4th method: Lineal R with prophet - just sales. Error: 0.10                    -----####
train_full_summarised <- train_full_summarised %>% rename(ds = date, y = tot_sales) 
train_prophet <- prophet(train_full_summarised, daily.seasonality=TRUE)

forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 

forecasting <- predict(train_prophet, forecasting)
tail(forecasting[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],14)

# plot(train_prophet, forecast)
# prophet_plot_components(train_prophet, forecast)

prevision <- left_join(test_full_summarised,forecasting %>% select(ds,yhat), by = c('date'='ds' ))

# plot_x_method<-prevision%>%
#   ggplot(aes(x = date , y = yhat)) +
#   geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
#   geom_line(aes(y = yhat), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
#   scale_y_continuous(labels = scales::comma_format(scale = 1e-3, big.mark = ","))
# print(plot_x_method)
error_4th_method <- rmsle(prevision$tot_sales, prevision$yhat)
message("4th method: Lineal R with prophet - just sales. Error: ", error_4th_method)
#--Data modification            ---####
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

#We change the tot_promotion column into predicted values insted of "known" ones
train_prophet <- train_df %>% rename(ds=date, y=tot_promotions)
train_prophet <- prophet(train_prophet)
forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
forecasting <- predict(train_prophet, forecasting)
test_df <- left_join(test_df,forecasting %>% select(ds,yhat), by = c("date"='ds'))
#rmsle(test_df$tot_promotions, test_df$yhat)
test_df <- test_df %>% select(-tot_promotions) %>% rename(tot_promotions = yhat)

#We change the oil_price column into predicted values insted of "known" ones
train_prophet <- train_df %>% rename(ds=date, y=oil_price)
train_prophet <- prophet(train_prophet)
forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
forecasting <- predict(train_prophet, forecasting)
test_df <- left_join(test_df,forecasting %>% select(ds,yhat), by = c("date"='ds'))
#rmsle(test_df$oil_price, test_df$yhat)
test_df <- test_df %>% select(-oil_price) %>% rename(oil_price = yhat)

#We change the transactions column into predicted values insted of "known" ones
train_prophet <- train_df %>% rename(ds=date, y=transactions)
train_prophet <- prophet(train_prophet)
forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
forecasting <- predict(train_prophet, forecasting)
test_df <- left_join(test_df,forecasting %>% select(ds,yhat), by = c("date"='ds'))
#rmsle(test_df$transactions, test_df$yhat)
test_df <- test_df %>% select(-transactions) %>% rename(transactions = yhat)

#----Data correlation graph     -####
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

#-------- 5th method: Linear Regression. Error: 0.09 - no intercept                      --------------------####
model <- lm(tot_sales ~ 0 + tot_promotions + year+ transactions, data=train_df)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
error_5th_method <- rmsle(prevision$tot_sales, prevision$`.`)
message("5th method: Linear Regression grouped by date with promotion, year, and transaction. Error: ", "\n", error_5th_method)
#-------- 6th method: Linear Regression. - Considers vars * day. Error: 0.12             --####
model <- lm(tot_sales ~ tot_promotions*day + transactions*day + oil_price*day + flag_holiday*day, data=train_df)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
error_6th_method <- rmsle(prevision$tot_sales, prevision$`.`)
message("6th method: Linear Regression. - Considers tot_promotions,transactions,oil_price and flag_holiday * day. Error: \n ", error_6th_method)
#-------- 7th method: Linear Regression (tslm). Error: 0.11 - with LAGs                  --####
train_df <- train_df  %>% mutate(lag_14 = lag(tot_sales, 14))
train_df <- train_df  %>% mutate(lag_21 = lag(tot_sales, 21))
test_df$lag_14 = tail(train_df$tot_sales, 14)
test_df$lag_21 = train_df$tot_sales[(nrow(train_df) - 20):(nrow(train_df) - 7)]

train_ts <- ts(train_df)
train_ts <- ts(train_df$tot_sales, start = c(2013, 1), frequency = 365)
fit_train_ts <- tslm(train_ts ~ 0+ tot_promotions + year+ transactions + oil_price + flag_holiday + payingDays +lag_21+lag_14, data = train_df)
test_ts <- ts(test_df$tot_sales, start = c(2013, 1), frequency = 365)
fcast <- forecast(fit_train_ts, newdata = test_df)

# autoplot(fcast) +
#   labs(title = "Sales Forecast", x = "Date", y = "Sales") +
#   theme_minimal()

predicted_values <- as.numeric(fcast$mean)  # Extract the predicted values
error_7th_method <- rmsle(test_df$tot_sales, predicted_values)
message("7th method: Linear Regression(tslm) with lags, tot_promotions, transactions, oil_price, flag_holiday and payingDays. Error:  \n", error_7th_method)

#--Data grouped by date and family            #### 
print("The following analysis were made with data grouped by date and family:")
#------Dataset creation         --#####
train_family <- train.original
train_family <- train_family %>%
  group_by(date,family) %>%
  summarize(tot_sales=sum(sales)) %>% ungroup()
test_family<- train_family %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_family <- train_family %>% filter(date<="2017-08-01")
#-------- 8th method: Linear regression: grouped by family and date. Error: 0.66         ------------------------------------------####
model <- lm(tot_sales ~ family, data = train_family)
prevision <- predict(model, newdata = test_family) %>% as.data.frame()
prevision <- test_family %>% cbind(prevision)
error_8th_method <- rmsle(prevision$tot_sales, prevision$`.`)
message("8th method: Linear regression: grouped by family and date. Error: ", error_8th_method)
#-------- 9th method: mean per family and mean of errors. Error: 0.40                    ----####
mean_sale<- train_family  %>% group_by(family) %>% summarise(mean_sales = mean(tot_sales))
prevision <- merge(test_family , mean_sale, by = "family", all.x = TRUE)

# Plot
dist_families <- train_family %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  x <- prevision %>% filter(family == fam) 
  # plot<-x  %>%
  #   ggplot(aes(x = date , y = tot_sales)) +
  #   geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  #   geom_line(aes(y = mean_sales), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  #   scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  #   labs(title=paste0("Prevision  ",fam ), x="Date", y="Total Sales (M)")
  # print(plot)
  result$errore[result$family == fam]  <-  rmsle(x$tot_sales, x$mean_sales)
}

error_9th_method <- mean(result$errore)
message("9th method: Mean per family. \n Mean error: ", error_9th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )

#-------- 10th method: Prophet. Mean error: 0.275                                        ------------------------------------------####
train <- train.original
train <- train %>% group_by(date, family) %>% summarise(sales= sum(sales), onpromotion=sum(onpromotion)) %>% ungroup()
train <- train %>% rename(ds = date, y = sales)
test <- train %>% filter(ds<="2017-08-15" & ds>"2017-08-01")
train <- train %>% filter(ds<="2017-08-01")

dist_families <- train %>% select(family) %>% distinct()
# Define a function to apply Prophet to a subset of data

result <- dist_families %>% mutate(errore =0)
message("Prophet is now running. It takes about 3min to finish. You may now take a little break, get some air, or some coffe, maybe even read a chapter of a book if your computer is slow, like mine. May your waiting period not be too long. Good luck.")
for(fam in dist_families$family){
  train_fam <- train %>% filter(family == fam) 
  test_fam <- test %>% filter(family == fam)
  train_prophet <- prophet(train_fam, daily.seasonality=TRUE)
  forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
  forecasting <- predict(train_prophet, forecasting)
  prevision <- left_join(test_fam,forecasting %>% select(ds,yhat), by = 'ds')
  prevision <- prevision %>% mutate(yhat = ifelse(yhat <0, 0, yhat))
  result$errore[result$family == fam]  <-  rmsle(prevision$y, prevision$yhat)
}
error_10th_method <- mean(result$errore)
message("10th method: One prophet model for each family. \n Mean error: ", error_10th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )

#-------- 11th method: Linear Regression. Dataset: Train.  Mean error: 0.24              ------------------------------------------####
train <- train.original
train <- train %>% group_by(date, family) %>% summarise(sales= sum(sales), onpromotion=sum(onpromotion)) %>% ungroup()
train <- train %>%
  mutate(Dayofweek = weekdays(train$date),
         Monthofyear = months(train$date),
         Year = year(train$date),
         YrMonth = format(train$date, "%Y-%m"),
         day= as.numeric(format(train$date, format = "%d")),
         payingDays = ifelse(day == 15 | day == 30 | day == 31, 1, 0))
train <- train %>% group_by(family) %>% mutate(lag_15 = lag(sales, 15)) %>% ungroup()
train <- train %>% group_by(family) %>% mutate(lag_21 = lag(sales, 21)) %>% ungroup()

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam <- train_df %>% filter(family == fam) 
  test_fam <- test_df %>% filter(family == fam)
  model <- lm(sales ~ onpromotion + lag_15 + lag_21, data = train_fam)
  prevision <- predict(model, newdata = test_fam) %>% as.data.frame()
  prevision <- test_fam %>% cbind(prevision)
  result$errore[result$family == fam]  <-  rmsle(prevision$sales, prevision$`.`)
}
error_11th_method <- mean(result$errore)
message("11th method: One linear model for each family that considers onpromotion and lags. \n Mean error: ", error_11th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )


#-------- 12th method: Linear Regression. Dataset: oil + holiday.  Mean error: 0.31      ------------------------------------------####

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


train_df <- train_df %>% left_join(oil, by='date')
train_df <- train_df %>%  fill(oil_price)
train_df <- train_df %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
train_df <- train_df %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

test_df <- test_df %>% left_join(oil, by='date')
test_df <- test_df %>%  fill(oil_price)
test_df <- test_df %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
test_df <- test_df %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

#We change the oil_price column into predicted values insted of "known" ones
train_prophet <- train_df %>% rename(ds=date, y=oil_price)
message("Now another prophet model is running. We're predicting the oil prices. You guessed it! It also takes long to finish: about 3min")
train_prophet <- prophet(train_prophet, daily.seasonality=TRUE)
forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
forecasting_oil <- predict(train_prophet, forecasting)
test_df <- left_join(test_df,forecasting_oil %>% select(ds,yhat), by = c("date"='ds'))
#rmsle(test_df$oil_price, test_df$yhat)
test_df <- test_df %>% select(-oil_price) %>% rename(oil_price = yhat)


#Understanding the correlations between variables
# train_cor <- train_df %>% filter(!is.na(lag_21))
# options(repr.plot.width = 70, repr.plot.height = 40)
# c1 <- cor(train_cor %>%
#             select(sales, onpromotion, oil_price, flag_holiday, payingDays, day, lag_15, lag_21))
# corrplot(c1,
#          method = c('color'),
#          addCoef.col = "black",
#          addgrid.col = "black",
#          tl.col = "black",
#          order = 'hclust')

dist_families <- train_df %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam <- train_df %>% filter(family == fam) 
  test_fam <- test_df %>% filter(family == fam)
  model <- lm(sales ~ onpromotion + Year + Dayofweek+ lag_15 + lag_21 + flag_holiday + oil_price, data = train_fam)
  prevision <- predict(model, newdata = test_fam) %>% as.data.frame()
  prevision <- test_fam %>% cbind(prevision)
  result$errore[result$family == fam]  <-  rmsle(prevision$sales, prevision$`.`)
}
error_12th_method <- mean(result$errore)
message("12th method: One linear model for each family that considers onpromotion, oil_price, holidays and lags. \n Mean error: ", error_12th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )

#--Data by date, family, and store_nbr        ####
print("The following analysis were made with data grouped by date, family and store:")
#------Dataset creation         --#####
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

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")
#-------- 13th method: Linear regression with Lags . Error: 0.82                         -----####


model <- lm(sales ~ family + store_nbr + onpromotion + lag_14 + lag_21, data=train_df)

prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
prevision <- prevision %>% rename(p = '.')
prevision <- prevision %>% mutate(p= ifelse(p<0, 0, p))
  
error_13th_method <- rmsle(prevision$sales, prevision$p)
message('13th method: Linear regression with Lags. Error: ',error_13th_method)

#-------- 14th method: One model for each family that consideres store_nbr. Error: 0.47  -----####
dist_families <- train %>% select(family) %>% distinct()

result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam_store <- train_df %>% filter(family == fam)
  model <- lm(sales ~ store_nbr + onpromotion + lag_14 + lag_21, data = train_fam_store)
  
  test_fam_store <- test_df %>% filter(family == fam)
  predictions <- predict(model, newdata = test_fam_store) %>%
    as.data.frame() %>%
    cbind(test_fam_store) %>%
    rename(p = '.') %>%
    mutate(p = ifelse(p < 0, 0, p))
  
  result$errore[result$family == fam ]  <-  rmsle(predictions$sales, predictions$p)
}

error_14th_method <- mean(result$errore)
message("14th method: One linear model for each family that considers onpromotion, store_nbr and lags. \n Mean error: ", error_14th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )


#-------- 15th method: One model for each store_nbr that consideres family. Error: 0.46  -----####
dist_store <- train %>% select(store_nbr) %>% distinct()

result <- dist_store %>% mutate(errore =0)
for(store in dist_store$store_nbr){
  train_fam_store <- train_df %>% filter(store_nbr== store)
  model <- lm(sales ~ family + onpromotion + lag_14 + lag_21, data = train_fam_store)
  
  test_fam_store <- test_df %>% filter(store_nbr== store)
  predictions <- predict(model, newdata = test_fam_store) %>%
    as.data.frame() %>%
    cbind(test_fam_store) %>%
    rename(p = '.') %>%
    mutate(p = ifelse(p < 0, 0, p))
  
  result$errore[result$store_nbr== store]  <-  rmsle(predictions$sales, predictions$p)
}

error_15th_method <- mean(result$errore)
message("15th method: One linear model for each store that considers onpromotion, family and lags. \n Mean error: ", error_15th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )




#-------- 16th method: One model for each store_nbr + family. Error: 0.40                -----####
#Function to fit a linear model for a specific family and store combination
fit_lm <- function(subset_df) {
  model <- lm(sales ~  lag_14 + lag_21 + onpromotion, data = subset_df)
  return(list(model = model))
}

# Use the by function to split the data and apply the function to each group
models <- lapply(split(train_df, train_df[c('family', 'store_nbr')]), fit_lm)

predictions <- list()# Iterate over the models and make predictions on the test data

family_store_names <- names(models)
dist_store_fam <- train %>% select(family, store_nbr) %>% distinct()
result <- dist_store_fam %>% mutate(errore =0)
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
  
  result$errore[result$store_nbr== store & result$family== fam]  <-  rmsle(predictions$sales, predictions$p)
}
error_16th_method <- mean(result$errore)
message("16th method: One linear model for each store and family that considers lags. \n Mean error: ", error_16th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )

#-------- 17th method: One lm for each store and family, + $oil + holidays. Error: 0.37  -----####

test_df <- left_join(test_df,forecasting_oil %>% select(ds,yhat), by = c("date"='ds'))
test_df <- test_df %>% rename(oil_price = yhat)
test_df <- test_df %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
test_df <- test_df %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

train_df <- train_df %>% left_join(oil, by='date')
train_df <- train_df %>% fill(oil_price)
train_df <- train_df %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
train_df <- train_df %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

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
result <- dist_store_fam %>% mutate(errore =0)
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
  result$errore[result$store_nbr== store & result$family== fam]  <-  rmsle(predictions$sales, predictions$p)
}
error_17th_method <- mean(result$errore)
message("17th method: One linear model for each store and family that considers lags, oil price, and holidays. \n Mean error: ", error_17th_method, "\n The minimun error is: ", min(result$errore), "\n The maximun error is: ", max(result$errore) )

