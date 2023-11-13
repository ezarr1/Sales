train.original = read_csv('Data/train.csv')
oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')
train <- train.original
oil <- oil.original
holiday <- holiday.original
transactions <- transactions.original

#summarise the sales
train <- train %>% group_by(date) %>% summarise(sum.sales = sum(sales), sum.promo = sum(onpromotion))

#########  linear model  #########
#variables we can use: year, promotions, holidays, family, store code
train$year <- as.numeric(format(train$date, format = "%Y"))
train$month <- as.numeric(format(train$date, format = "%m"))
train$day <- as.numeric(format(train$date, format = "%d"))
train$day_of_week <- weekdays(train$date)
train$last_day_of_month <- ifelse(day(train$date) == days_in_month(train$date), 1, 0)
train$last_day_of_month <- ifelse(train$day == 15, 1, train$last_day_of_month)
train$month_day_combined <- paste0(month(train$date), "-", day(train$date))

test <- train[(nrow(train) - 14):(nrow(train)), ]
train <- train[1:(nrow(train) - 14), ]

n <- holiday %>%  group_by(date) %>%  summarise(n_festivities = n()) %>%  ungroup()

holidays_locale <- holiday %>% select(date, locale) %>% distinct()
holidays_locale <- holidays_locale %>% left_join(n, by="date")
holidays_locale$locale <- ifelse(holidays_locale$n_festivities > 1, "combined", holidays_locale$locale)

holidays_type <- holiday %>% select(date, type) %>% distinct()
holidays_type <- holidays_type %>% left_join(n, by="date")
holidays_type$type <- ifelse(holidays_type$n_festivities > 1, "combined", holidays_type$type)

holidays_complete <- holidays_type %>% left_join(holidays_locale, by="date", relationship = "many-to-many")

holidays_complete <- holidays_complete %>% distinct()
holidays_complete$month_day_combined <- paste0(month(holidays_complete$date), "-", 
                                               day(holidays_complete$date))
holidays_complete <- holidays_complete %>% select(-c(n_festivities.x, n_festivities.y))

train <- train %>% left_join(holidays_complete, by="month_day_combined")
test <- test %>% left_join(holidays_complete, by="month_day_combined")

no_holiday <- "None"
train$locale <- ifelse(is.na(train$locale), no_holiday, train$locale)
train$type <- ifelse(is.na(train$type), no_holiday, train$type)

test$locale <- ifelse(is.na(test$locale), no_holiday, test$locale)
test$type <- ifelse(is.na(test$type), no_holiday, test$type)

train <- train %>% select(-date.y) 
train <- train %>% rename(date = date.x)
train$date <- train$date %>% as.Date()


test <- test %>% select(-date.y) 
test <- test %>% rename(date = date.x)
test$date <- test$date %>% as.Date()

#create the model
linear.model <- lm(sum.sales ~ sum.promo + day_of_week + type +last_day_of_month, data = train)
summary(linear.model)

#predict test sales
test$preds <- predict(linear.model, newdata = test)
act <- test$sum.sales
rmsle(act, test$preds) #0.066

ggplot(test, aes(x = date)) +
  geom_line(aes(y = sum.sales, color = "Actuals")) +
  geom_line(aes(y = preds, color = "Predicted")) +
  scale_color_manual(values = c("Actuals" = "blue", "Predicted" = "red"))


str(train)
options(repr.plot.width = 50, repr.plot.height = 20)
c1 <- cor(train %>%
            select(sum.sales, sum.promo, year , month, day, last_day_of_month))
corrplot(c1,
         method = c('color'),
         addCoef.col = "black",
         addgrid.col = "black",
         tl.col = "black",
         order = 'hclust')