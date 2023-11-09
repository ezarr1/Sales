source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
train <- train.original

#--------a) First method: We'll sell today what we sold yesterday. Error: 5.77 --------------------####
# We take into consideration the 01/08/2017
train_full_summarised <- train.original %>%
  group_by(date)%>%
  summarize(tot_sales=sum(sales))
test_full_summarised<- train_full_summarised %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_full_summarised <- train_full_summarised %>% filter(date<="2017-08-01")

first_august<-train_full_summarised[[1,2]]
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=first_august)

# Plot 
plot_first_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y = tot_sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision last day 01/08/2017", x="Date", y="Total Sales (M)")
print(plot_first_method)
error_first_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)



#-------------b) Second method: Mean of previous sales. Error: 0.27            ----------------####
# We take into consideration the mean of the previous days
mean_sale<-mean(train_full_summarised$tot_sales)
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=mean_sale)

# Plot 
plot_x_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision average days", x="Date", y="Total Sales (M)")
print(plot_x_method)
error_second_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)

#-------------c) third method: Mean of last 14 days. Error: 0.13               ------------------####
# We take into consideration the mean of the previous 14 days
mean_sale<-mean(tail(train_full_summarised$tot_sales,14))
prevision<-test_full_summarised%>%
  mutate(tot_sales_prevision=mean_sale)

# Plot 
plot_x_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision average previous 14 days", x="Date", y="Total Sales (M)")
print(plot_x_method)
error_third_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)

#-------------d) fourth method  Linear regression:family. Error: 0.76          ------------------------------------------####
train_family <- train.original
train_family <- train_family %>%
  group_by(date,family) %>%
  summarize(tot_sales=sum(sales)) %>% ungroup()
test_family<- train_family %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_family <- train_family %>% filter(date<="2017-08-01")

model <- lm(tot_sales ~ family, data = train_family)
prevision <- predict(model, newdata = test_family) %>% as.data.frame()
prevision <- test_family %>% cbind(prevision)

# Plot 
plot_x_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "green", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = `.`), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision linear regression", x="Date", y="Total Sales (M)")
print(plot_x_method)
error_fourth_method <- rmsle(prevision$tot_sales, prevision$`.`)

#------------ d)1. NO earthquake and no 01-01. Error: 0.77                     ----------------------####
train_family <- train_family %>% filter((date >"2016-05-07" | date <= "2016-04-15" )& date != '2014-01-01' & date != '2015-01-01' & date != '2016-01-01' & date != '2017-01-01' )

model <- lm(tot_sales ~ family, data = train_family)
prevision <- predict(model, newdata = test_family) %>% as.data.frame()
prevision <-  cbind(test_family, tot_sales_prevision = prevision$.) 
error_fourth.b_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)

#-------------e) fifth method: mean per family and mean of errors. Error: 0.53 ----####

mean_sale<- train_family  %>% group_by(family) %>% summarise(mean_sales = mean(tot_sales))
prevision <- merge(test_family , mean_sale, by = "family", all.x = TRUE)

# Plot
dist_families <- train_family %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  x <- prevision %>% filter(family == fam) 
  plot<-x  %>%
    ggplot(aes(x = date , y = tot_sales)) +
    geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
    geom_line(aes(y = mean_sales), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
    scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
    labs(title=paste0("Prevision  ",fam ), x="Date", y="Total Sales (M)")
  print(plot)
  result$errore[result$family == fam]  <-  rmsle(x$tot_sales, x$mean_sales)
}

error_fifth_method <- mean(result$errore)
print(paste0("The minimun error is: ", min(result$errore)))
print(paste0("The maximun error is: ", max(result$errore)))



#-------------Preparation for next methods---------------####
train <- train.original
train <- train %>%
  mutate(Dayofweek = weekdays(train$date),
         Monthofyear = months(train$date),
         Year = year(train$date),
         YrMonth = format(train$date, "%Y-%m"))

#Encoding string variables
unique_mapping <- train %>% distinct(family) %>% mutate(family_encoded = 1:n())
train <- train %>% left_join(unique_mapping, by = "family")
unique_mapping <- train %>% distinct(store_nbr) %>% mutate(store_nbr_encoded = 1:n())
train <- train %>% left_join(unique_mapping, by = "store_nbr")
unique_mapping <- train %>% distinct(Dayofweek) %>% mutate(Dayofweek_encoded = 1:n())
train <- train %>% left_join(unique_mapping, by = "Dayofweek")
unique_mapping <- train %>% distinct(Monthofyear) %>% mutate(Monthofyear_encoded = 1:n())
train <- train %>% left_join(unique_mapping, by = "Monthofyear")

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

#Understanding the correlations between variables
options(repr.plot.width = 50, repr.plot.height = 20)
c1 <- cor(train_df %>%
            select(sales, onpromotion,Year, family_encoded, store_nbr_encoded, Dayofweek_encoded, Monthofyear_encoded))
corrplot(c1,
         method = c('color'),
         addCoef.col = "black",
         addgrid.col = "black",
         tl.col = "black",
         order = 'hclust')

#------------f) sixth method: Lineal R. with family and onpromotion. Error:0.90 -----####
model <- lm(sales ~ family + onpromotion, data = train_df)
#summary(model)
prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- cbind(prevision,test_df) 

error_sixt_method <- rmsle(prevision$sales,prevision$.)

#------------g) seventh method: Lineal R with prophet - just sales. Error: 0.09 -----####
train_full_summarised <- train_full_summarised %>% rename(ds = date, y = tot_sales) 
train_prophet <- prophet(train_full_summarised)

forecasting <- make_future_dataframe(train_prophet, periods = 14)  # date + 14 giorni 

forecasting <- predict(train_prophet, forecasting)
tail(forecasting[c('ds', 'yhat', 'yhat_lower', 'yhat_upper')],14)

# plot(train_prophet, forecast)
# prophet_plot_components(train_prophet, forecast)

prevision <- left_join(test_full_summarised,forecasting %>% select(ds,yhat), by = c('date'='ds' ))

plot_x_method<-prevision%>%
  ggplot(aes(x = date , y = yhat)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
  geom_line(aes(y = yhat), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-3, big.mark = ","))
print(plot_x_method)
error_seventh_method <- rmsle(prevision$tot_sales, prevision$yhat)

#------------g) eight method: Forecast tslm . INCOMPLETE                        ####

train_ts <- ts(train_df)
fit_train_ts <- tslm(sales ~ family_encoded, data=train_ts)
fcast <- forecast(fit_train_ts, newdata = test_df)

#------------g) nineth method: Lags . Error: 0.50                               -----####
train <- train.original
train <- train %>% arrange(family, store_nbr, date)
train <- train %>% group_by(family) %>% mutate(lag_14 = lag(sales, 14))
train <- train %>% group_by(family) %>% mutate(lag_21 = lag(sales, 21))

test_df<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_df <- train %>% filter(date<="2017-08-01")

model <- lm(sales ~ family + onpromotion + lag_14 + lag_21, data=train_df)

prevision <- predict(model, newdata = test_df) %>% as.data.frame()
prevision <- test_df %>% cbind(prevision)

error_nineth_method <- rmsle(prevision$sales, prevision$`.`)
