source("Library.R")
library(purrr)
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
train <- train.original
train <- train %>% group_by(date, family) %>% summarise(sales= sum(sales), onpromotion=sum(onpromotion)) %>% ungroup()
train <- train %>% rename(ds = date, y = sales)
test <- train %>% filter(ds<="2017-08-15" & ds>"2017-08-01")
train <- train %>% filter(ds<="2017-08-01")

#------------- Prophet. Mean error: 0.31 ------------------------------------------####

dist_families <- train %>% select(family) %>% distinct()
# Define a function to apply Prophet to a subset of data

result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam <- train %>% filter(family == fam) 
  test_fam <- test %>% filter(family == fam)
  train_prophet <- prophet(train_fam)
  forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 
  forecasting <- predict(train_prophet, forecasting)
  prevision <- left_join(test_fam,forecasting %>% select(ds,yhat), by = 'ds')
  result$errore[result$family == fam]  <-  rmsle(prevision$y, prevision$yhat)
}
error_14th_method <- mean(result$errore)
print(paste0("The minimun error is: ", min(result$errore)))
print(paste0("The maximun error is: ", max(result$errore)))



#------------- Linear Regression. Dataset: Train.  Mean error: 0.25  ------------------------------------------####
train <- train.original
train <- train %>% group_by(date, family) %>% summarise(sales= sum(sales), onpromotion=sum(onpromotion)) %>% ungroup()
train <- train %>%
  mutate(Dayofweek = weekdays(train$date),
         Monthofyear = months(train$date),
         Year = year(train$date),
         YrMonth = format(train$date, "%Y-%m"),
         day= as.numeric(format(train$date, format = "%d")),
         payingDays = ifelse(day == 15 | day == 30 | day == 31, 1, 0))
train <- train %>% group_by(family) %>% mutate(lag_15 = lag(sales, 15))
train <- train %>% group_by(family) %>% mutate(lag_21 = lag(sales, 21))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

dist_families <- train_df %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam <- train_df %>% filter(family == fam) 
  test_fam <- test_df %>% filter(family == fam)
  model <- lm(sales ~ onpromotion + Year + Dayofweek+ payingDays+ lag_15 + lag_21, data = train_fam)
  prevision <- predict(model, newdata = test_fam) %>% as.data.frame()
  prevision <- test_fam %>% cbind(prevision)
  result$errore[result$family == fam]  <-  rmsle(prevision$sales, prevision$`.`)
}
error_15th_method <- mean(result$errore)
print(paste0("The minimun error is: ", min(result$errore)))
print(paste0("The maximun error is: ", max(result$errore)))



#------------- Linear Regression. Dataset: All.  Mean error: 0.26  ------------------------------------------####
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


#Adding all the info to train
train <- train.original
train <- train %>% group_by(date, family) %>% summarise(sales= sum(sales), onpromotion=sum(onpromotion)) %>% ungroup()
train <- train %>% left_join(oil, by='date')
train <- train %>%  fill(oil_price)
train <- train %>% left_join(holiday %>% select(date, flag_holiday), by='date') 
train <- train %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))

train <- train %>%
  mutate(dayofweek = weekdays(train$date),
         monthofyear = months(train$date),
         year = year(train$date),
         day= as.numeric(format(train$date, format = "%d")),
         payingDays = ifelse(day == 15 | day == 30 | day == 31, 1, 0))

train <- train %>% group_by(family) %>% mutate(lag_15 = lag(sales, 15)) %>% ungroup()
train <- train %>% group_by(family) %>% mutate(lag_21 = lag(sales, 21)) %>% ungroup()

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")  


#Understanding the correlations between variables
train_cor <- train_df %>% filter(!is.na(lag_21))
options(repr.plot.width = 70, repr.plot.height = 40)
c1 <- cor(train_cor %>%
            select(sales, onpromotion, year, oil_price, flag_holiday, payingDays, day, lag_15, lag_21))
corrplot(c1,
         method = c('color'),
         addCoef.col = "black",
         addgrid.col = "black",
         tl.col = "black",
         order = 'hclust')

dist_families <- train_df %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  train_fam <- train_df %>% filter(family == fam) 
  test_fam <- test_df %>% filter(family == fam)
  model <- lm(sales ~ onpromotion + year + dayofweek+ lag_15 + lag_21 + flag_holiday, data = train_fam)
  prevision <- predict(model, newdata = test_fam) %>% as.data.frame()
  prevision <- test_fam %>% cbind(prevision)
  result$errore[result$family == fam]  <-  rmsle(prevision$sales, prevision$`.`)
}
error_16th_method <- mean(result$errore)
error_16th_method
print(paste0("The minimun error is: ", min(result$errore)))
print(paste0("The maximun error is: ", max(result$errore)))