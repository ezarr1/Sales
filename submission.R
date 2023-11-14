source("Library.R")
#------------- Reading of files ------------------------------------------------####

oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
train.original = read_csv('Data/train.csv')
train.original <- train.original %>% filter(date>"2015-01-01")
test.original = read_csv('Data/test.csv')
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
holiday <- holiday %>% filter(type!="Work Day" & flag_holiday != 0) %>% select(-description, -type) %>% distinct()
holiday_locals <- holiday %>% filter(locale== "Local")
holiday_regional <- holiday %>% filter(locale== "Regional")
holiday_national <- holiday %>% filter(locale== "National")

stores_with_holidays <- stores.original %>% select(store_nbr, city, state)
stores_with_holidays <- stores_with_holidays %>% mutate(nation = "Ecuador")
stores_with_holidays_nat <- stores_with_holidays %>% left_join(holiday_national %>% select(date, locale_name, flag_holiday), by=c("nation"= "locale_name"))
stores_with_holidays_reg <- stores_with_holidays %>% left_join(holiday_regional %>% select(date, locale_name, flag_holiday), by=c("state"= "locale_name"))
stores_with_holidays_loc <- stores_with_holidays %>% left_join(holiday_locals %>% select(date, locale_name, flag_holiday), by=c("city"= "locale_name"))
stores_with_holidays <- rbind(stores_with_holidays_nat, stores_with_holidays_reg, stores_with_holidays_loc) %>% distinct()

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
train_df <- train_df %>% left_join(oil, by='date') %>%  fill(oil_price, .direction = "down")
train_df <- train
train_df <- train_df %>% arrange(date, family, store_nbr)

test_df <- test.original
test_df <- test_df %>% arrange(date, family, store_nbr)
test_df$lag_14 <- tail(train_df$lag_14, 28512) 
test_df$lag_21 <- tail(train_df$lag_21, 28512) 

test_df <- test_df %>%
  mutate(dayofweek = weekdays(test_df$date),
         monthofyear = months(test_df$date),
         year = year(test_df$date),
         day= as.numeric(format(test_df$date, format = "%d")),
         payingDays = case_when(
           str_detect(monthofyear, "novembre|aprile|giugno|settembre") & day == 30 ~ 1,
           str_detect(monthofyear, "febbraio") & day == 28 ~ 1,
           day==31 ~ 1,
           TRUE ~ 0   
         )
  )
test_df <- test_df %>% left_join(stores_with_holidays %>% select(date, store_nbr, flag_holiday), by=c('date', 'store_nbr')) 
test_df <- test_df %>% mutate(flag_holiday= ifelse(is.na(flag_holiday), 0, flag_holiday))
test_df <- test_df %>% left_join(oil, by='date') %>%  fill(oil_price, .direction = "down")


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

#family_store_names <- names(models)
family_store_names <- train_df %>% group_by(family,store_nbr) %>% summarise(sales = sum(sales)) %>% filter(sales !=0) %>% ungroup()
family_store_names <- family_store_names %>% mutate(combination = paste0(family,'.',store_nbr))
family_store_names <- family_store_names %>% select(combination) %>% distinct()
prevision <- data.frame(sales=numeric(0), p=numeric(0))
for (name in family_store_names$combination) {  
  fam <- strsplit(name, "\\.")[[1]][1]
  store <- strsplit(name, "\\.")[[1]][2]    # Filter the test data for the specific family and store  
  store=as.numeric(store)
  subset_test_df <- subset(test_df, family == fam & store_nbr == store)    # Use the corresponding model to make predictions  
  predictions <- predict(models[[name]]$model, newdata = subset_test_df) %>%
    as.data.frame() %>%
    cbind(subset_test_df) %>%
    rename(sales = '.') %>%
    mutate(sales = ifelse(sales < 0, 0, sales))
  prevision <- prevision %>% rbind(predictions %>% select(id, sales))
}
# family_store_withoutSales <- train_df %>% group_by(family,store_nbr) %>% summarise(sales = sum(sales)) %>% filter(sales ==0) %>% ungroup()
# family_store_withoutSales <- family_store_withoutSales %>% mutate(sales=0) 
# date_range <- seq(as.Date("2017-08-16"), as.Date("2017-08-31"), by = "1 day")
# family_store_withoutSales <- family_store_withoutSales %>% crossing(date = date_range)
# prevision <- prevision %>% rbind(family_store_withoutSales %>% select(sales, p))

submit <- test_df %>% select(id) %>% left_join(prevision, by="id")
submit <- submit %>% mutate(sales = ifelse(is.na(sales), "0.00", sprintf("%.2f", sales)))
submit <- submit %>% arrange(id)

write_csv(submit, "submission.csv")
