source("Library.R")
#------------- Reading of files ------------------------------------------------####

oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')
transactions <- transactions.original
train.original = read_csv('Data/train.csv')
train.original <- train.original %>% filter(date>"2016-07-01")
stores.original <- read_csv("Data/stores.csv")

#Cleaning the oil DF / giving NAs the values of the following day 
oil <- oil.original
oil <- oil %>% rename(oil_price= dcoilwtico)
oil <- oil %>%  fill(oil_price, .direction = "down")
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
  type=="Additional" ~ 1
))
holiday <- holiday %>% filter(type!="Work Day" & flag_holiday != 0) %>% select(-type) %>% distinct()
holiday_locals <- holiday %>% filter(locale== "Local")
holiday_regional <- holiday %>% filter(locale== "Regional")
holiday_national <- holiday %>% filter(locale== "National")

stores_with_holidays <- stores.original %>% select(store_nbr, city, state)
stores_with_holidays <- stores_with_holidays %>% mutate(nation = "Ecuador")
stores_with_holidays_nat <- stores_with_holidays %>% right_join(holiday_national %>% select(date, locale_name, flag_holiday, description), by=c("nation"= "locale_name"))
stores_with_holidays_reg <- stores_with_holidays %>% right_join(holiday_regional %>% select(date, locale_name, flag_holiday, description), by=c("state"= "locale_name"))
stores_with_holidays_loc <- stores_with_holidays %>% right_join(holiday_locals %>% select(date, locale_name, flag_holiday, description), by=c("city"= "locale_name"))
stores_with_holidays <- rbind(stores_with_holidays_nat, stores_with_holidays_reg, stores_with_holidays_loc) %>% distinct()
stores_with_holidays <- stores_with_holidays %>% group_by(date, store_nbr) %>% summarise(description=first(description), flag_holiday=1)

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
train <- train %>% left_join(stores_with_holidays %>% select(date, store_nbr, flag_holiday), by=c('date', 'store_nbr')) 
train <- train %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))


test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

train_df <- train_df %>% left_join(oil, by='date') %>%  fill(oil_price, .direction = "down")
test_df <- test_df %>% left_join(oil, by='date') %>%  fill(oil_price, .direction = "down")

train_df <- train_df %>% rename(ds=date, y=sales)
test_df <- test_df %>% rename(ds=date, y=sales)

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

# dist_fam <- train %>% select(family) %>% distinct()
# dist_store <- train %>% select(store_nbr) %>% distinct()
# previsions <- data.frame(y=numeric(0), yhat=numeric(0))
# for (fam in dist_fam$family) {  
#   for (store in dist_store$store_nbr) {
#     train_prophet <- train_df %>% filter(family==fam & store_nbr==store)
#     test_prophet <- test_df %>% filter(family==fam & store_nbr==store)
#     train_prophet <- prophet(train_prophet)
#     forecasting <- make_future_dataframe(train_prophet, periods = 14)  
#     forecasting <- predict(train_prophet, forecasting)
#     prevision <- left_join(test_prophet,forecasting %>% select(ds,yhat), by = 'ds' )
#     previsions <- previsions %>% rbind(prevision %>% select(y,yhat))
#   }
# }




family_store_names <- train_df %>% group_by(family,store_nbr) %>% summarise(sales = sum(sales)) %>% filter(sales !=0) %>% ungroup()
family_store_names <- family_store_names %>% mutate(combination = paste0(family,'.',store_nbr))
family_store_names <- family_store_names %>% select(combination) %>% distinct()
previsions <- data.frame(y=numeric(0), yhat=numeric(0))
for(famstore in family_store_names$combination){
  fam <- strsplit(famstore, "\\.")[[1]][1]
  store <- strsplit(famstore, "\\.")[[1]][2]    # Filter the test data for the specific family and store  
  store=as.numeric(store)
  train_prophet <- train_df %>% filter(family==fam & store_nbr==store)
  test_prophet <- test_df %>% filter(family==fam & store_nbr==store)
  train_prophet <- prophet(train_prophet)
  forecasting <- make_future_dataframe(train_prophet, periods = 14)  
  forecasting <- predict(train_prophet, forecasting)
  prevision <- left_join(test_prophet,forecasting %>% select(ds,yhat), by = 'ds' )
  previsions <- previsions %>% rbind(prevision %>% select(y,yhat))
  message(famstore)
}
family_store_withoutSales <- train_df %>% group_by(family,store_nbr) %>% summarise(sales = sum(sales)) %>% filter(sales ==0) %>% ungroup()
family_store_withoutSales <- family_store_withoutSales %>% mutate(yhat=0) 
date_range <- seq(as.Date("2017-08-02"), as.Date("2017-08-15"), by = "1 day")
family_store_withoutSales <- family_store_withoutSales %>% crossing(date = date_range)
previsions <- previsions %>% rbind(family_store_withoutSales)
previsions <- previsions %>% mutate(yhat= ifelse(yhat<0, 0, yhat))
  
error_18th_method <- rmsle(previsions$y, previsions$yhat)
message("18th method: One prophet model for each store and family that considers sales. \n Error: ", error_18th_method)
#Error: 0.582489582441013


