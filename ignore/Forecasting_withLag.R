source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
train <- train.original
# train <- train %>%
#   mutate(Dayofweek = weekdays(train$date),
#          Monthofyear = months(train$date),
#          Year = year(train$date),
#          YrMonth = format(train$date, "%Y-%m"))
# train <- encode_column(train, family)
# train <- encode_column(train, Dayofweek)
# train <- encode_column(train, Monthofyear)
test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

train_df <- train_df %>% group_by(family) %>% mutate(lag_1 = lag(sales, 1))
train_df <- train_df %>% group_by(family) %>% mutate(lag_7 = lag(sales, 7))

model <- lm(sales ~ lag_1 + lag_7, data=train_df)
plot(model)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
