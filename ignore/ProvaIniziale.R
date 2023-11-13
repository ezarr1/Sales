cat('\014')
rm(list=ls())
source("Library.R")
#------------- Data ------------------------------------------####
#
train.original = read_csv('Data/train.csv')
train <- train.original
train <- train %>%
  group_by(family, date)%>%
  summarize(tot_sales=sum(sales))
test<- train %>% filter(date<="2017-08-15" & date>"2017-08-01")
train <- train %>% filter(date<="2017-08-01")
test2 <- test %>% mutate(tot_sales=0)

train_family <- train.original
train_family <- train_family %>%
  group_by(date,family) %>%
  summarize(tot_sales=sum(sales)) %>% ungroup()
test_family<- train_family %>% filter(date<="2017-08-15" & date>"2017-08-01")
train_family <- train_family %>% filter(date<="2017-08-01")

#-------------a) First method ------------------------------------------####
# We take into consideration the 01/08/2017
first_august<-train[[1,2]]
prevision<-test%>%
  mutate(tot_sales_prevision=first_august)

# Plot 
plot_first_method<-prevision%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", linewidth = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", linewidth = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision last day 01/08/2017", x="Date", y="Total Sales (M)")
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
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision average days", x="Date", y="Total Sales (M)")
print(plot_second_method)
error_second_method <- rmsle(prevision_2$tot_sales, prevision_2$tot_sales_prevision)
# 0.5 could be good

#-------------c) third method ------------------------------------------####
# We take into consideration the mean of the previous 14 days
mean_sale_14<-mean(tail(train$tot_sales,14))
prevision_3<-test%>%
  mutate(tot_sales_prevision=mean_sale_14)

# Plot 
plot_third_method<-prevision_3%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision average previous 14 days", x="Date", y="Total Sales (M)")
print(plot_third_method)
error_third_method <- rmsle(prevision_3$tot_sales, prevision_3$tot_sales_prevision)


#-------------d) fourth method  Linear regression ------------------------------------------####
# 

model <- lm(tot_sales ~ family, data = train_family)
prevision_4 <- predict(model, newdata = test_family) %>% as.data.frame()

prevision_4 <-  cbind(test_family$date, prevision_4$., test_family$tot_sales ) %>% as.data.frame()
prevision_4 <- prevision_4 %>% rename(date = V1, tot_sales_prevision = V2, tot_sales = V3)
prevision_4 <- prevision_4 %>% mutate(date = as.Date(date))

# Plot 
plot_fourth_method<-prevision_4%>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = tot_sales_prevision), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title="Prevision linear regression", x="Date", y="Total Sales (M)")
print(plot_fourth_method)
error_fourth_method <- rmsle(prevision_4$tot_sales, prevision_4$tot_sales_prevision)
#-------------e) fifth method  average in family and the mean ------------------------------------------####
#



mean_sale_family<- train_family  %>% group_by(family) %>% summarise(mean_sales_family = mean(tot_sales))
prevision_family <- merge(test_family , mean_sale_family, by = "family", all.x = TRUE)


# Plot
dist_families <- train_family %>% select(family) %>% distinct()
result <- dist_families %>% mutate(errore =0)
for(fam in dist_families$family){
  x <- prevision_family %>% filter(family == fam) 
  plot<-x  %>%
  ggplot(aes(x = date , y = tot_sales)) +
  geom_line(aes(y =tot_sales), color = "slateblue4", linetype = "solid", size = 0.8, group = 1) +
  geom_line(aes(y = mean_sales_family), color = "blue", linetype = "dashed", size = 0.8, group = 2)+
  scale_y_continuous(labels = scales::comma_format(scale = 1e-6, big.mark = ","))+
  labs(title=paste0("Prevision  ",fam ), x="Date", y="Total Sales (M)")
  print(plot)
  result$errore[result$family == fam]  <-  rmsle(x$tot_sales, x$mean_sales_family)
}

error_fifth_method <- mean(result$errore)



#------------ NO earthquake  and 01-01  ----------####
train <- train %>% filter((date >"2016-05-07" | date <= "2016-04-15" )& date != '2014-01-01' & date != '2015-01-01' & date != '2016-01-01' & date != '2017-01-01' )


model <- lm(tot_sales ~ family+date, data = train)
prevision_lm_family <- predict(model, newdata = test) %>% as.data.frame()
prevision_lm_family <-  cbind(test, tot_sales_prevision = prevision_lm_family$.) %>%
  rename(date = date, family = family, tot_sales = tot_sales)


