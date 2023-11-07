library(Metrics)
library(readr)
library(dplyr)
library(ggplot2)
train.original = read_csv('Data/train.csv')
train <- train.original
train <- train %>%
  group_by(date)%>%
  summarize(tot_sales=sum(sales))
test<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train <- train %>% filter(date<="2017-08-01")

#-------------a) First method ------------------------------------------####
# We take into consideration the 01/08/2017
first_august<-train[[1,2]]
prevision<-test%>%
  mutate(tot_sales_prevision=first_august)

# Plot 
plot_first_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  labs(title="Prevision last day 01/08/2017", x="Date", y="Total Sales")
print(plot_first_method)
error_first_method <- rmsle(prevision$tot_sales, prevision$tot_sales_prevision)
# 0.5 could be good

#-------------b) Second method ------------------------------------------####
# We take into consideration the mean of the previous days
mean_sale<-mean(train$tot_sales)
prevision_2<-test%>%
  mutate(tot_sales_prevision=mean_sale)

# Plot 
plot_second_method<-prevision_2%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  labs(title="Prevision average days", x="Date", y="Total Sales")
print(plot_second_method)
error_second_method <- rmsle(prevision_2$tot_sales, prevision_2$tot_sales_prevision)
# 0.5 could be good

