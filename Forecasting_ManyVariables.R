source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
oil.original = read_csv('Data/oil.csv')
holiday.original = read_csv('Data/holidays_events.csv')
transactions.original = read_csv('Data/transactions.csv')

#Cleaning the oil DF
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
train <- train %>% left_join(transactions.original, by=c('date')) 
train <- train %>% mutate(transactions= ifelse(is.na(transactions) & tot_sales==0, 0, transactions))
mean_transactions <- mean(train$transactions, na.rm = TRUE)
train <- train %>% mutate(transactions= ifelse(is.na(transactions), mean_transactions, transactions))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

#Understanding the correlations between variables
# options(repr.plot.width = 50, repr.plot.height = 20)
# c1 <- cor(train_df %>%
#             select(tot_sales, tot_promotions, oil_price, flag_holiday, transactions))
# corrplot(c1,
#          method = c('color'),
#          addCoef.col = "black",
#          addgrid.col = "black",
#          tl.col = "black",
#          order = 'hclust')

model <- lm(tot_sales ~ tot_promotions + transactions + oil_price + flag_holiday, data=train_df)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)
error_tenth_method <- rmsle(prevision$tot_sales, prevision$`.`)
