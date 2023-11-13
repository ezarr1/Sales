cat('\014')
rm(list=ls())
source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')



train <- train.original
train <- train %>%
  mutate(Dayofweek = weekdays(train$date),
         Monthofyear = months(train$date),
         Year = year(train$date),
         YrMonth = format(train$date, "%Y-%m"))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

#----------------------------####

#------------- correlation -----####

options(repr.plot.width = 14, repr.plot.height = 8)
c1 <- cor(train_df %>%
            select(sales, onpromotion,Year))
# corrplot(c1,
#          method = c('color'),
#          addCoef.col = "black",
#          addgrid.col = "black",
#          tl.col = "black",
#          order = 'hclust')


#----------------------------####

#------------- tentativo brutto -----####

model <- lm(sales ~ family + onpromotion, data = train_df)
summary(model)

prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- cbind(prevision,test_df)

rmsle(prevision$sales,prevision$.)


# Plot 
plot_fourth_method<-prevision%>%
  ggplot(aes(x = date , y = sales)) +
  geom_point(aes(y =sales), color = "red", group = 1) +
  geom_point(aes(y = `.`), color = "blue", group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision linear regression", x="Date", y="Total Sales (M)")
print(plot_fourth_method)
#error_fourth_method <- rmsle(prevision_4$tot_sales, prevision_4$tot_sales_prevision)

#-------------------------------####

#------------- prophet -----####
train_prophet <- train_df %>% rename(ds = date, y = sales) %>% as.data.frame()
train_prophet <- train_prophet %>% select(ds,y) %>% group_by(ds) %>%
  summarise(y = sum(y))
train_prophet <- prophet(train_prophet)

forecast <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 

forecast <- predict(train_prophet, forecast)
tail(forecast[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],14)

test_prophet <- test_df %>% group_by(date) %>%
  summarise(sales = sum(sales))

# plot(train_prophet, forecast)
# prophet_plot_components(train_prophet, forecast)


to_plot <- left_join(test_prophet,forecast %>% select(ds,yhat), by = c('date'='ds' ))


plot<-to_plot%>%
  ggplot(aes(x = date , y = yhat)) +
  geom_line(aes(y =sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
  geom_line(aes(y = yhat), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, big.mark = ","))

print(plot)



error_prophet <- rmsle(to_plot$sales, to_plot$yhat)


#---------------------------####

#------------- xgboost -----####
unique_mapping <- train_df %>%
  distinct(family) %>%
  mutate(family_encoded = 1:n())

train_df_xg <- train_df %>%
  left_join(unique_mapping, by = "family")

unique_mapping <- train_df %>%
  distinct(store_nbr) %>%
  mutate(store_nbr_encoded = 1:n())

train_df_xg <- train_df_xg %>%
  left_join(unique_mapping, by = "store_nbr")

# target_variable <- "sales"
# predictor_variables <- c("Year", "store_nbr_encoded",'family_encoded')
# 
# train_xg <- xgb.DMatrix(data = as.matrix(train_df_xg[, predictor_variables]), label = train_df_xg[, target_variable])
# str(train_df_xg)
# 
# 
# params <- list(
#   objective = "reg:squarederror",  # For regression tasks
#   eta = 0.1,  # Learning rate
#   max_depth = 6,  # Maximum tree depth
#   nrounds = 10  # Number of boosting rounds (trees)
# )
# 
# xgb_model <- xgboost(params = params, data = dtrain)



model <- xgboost(data = as.matrix(train_df_xg), 
                 booster = "gblinear", # Use linear booster for linear regression
                 objective = "reg:linear", # Set regression as the objective
                 eval_metric = "rmse", # Use root mean squared error for evaluation
                 max_depth = 1, # Set other hyperparameters as needed
                 nrounds = 1000, # Specify the number of boosting rounds
                 verbose = 0)








# Now you have your trained XGBoost regression model

# To make predictions for future sales, prepare a similar dataset for the future, and use the predict() function:
# Example:
future_data <- read.csv("your_future_data.csv")
dtest <- xgb.DMatrix(data = as.matrix(future_data[, predictor_variables]))
future_sales_predictions <- predict(xgb_model, dtest)














#---------------------------####

