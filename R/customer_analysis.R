library(leaflet)
library(lubridate)
library(visdat)
library(patchwork)
library(janitor)
# The dataset contains 3 months of transactions for 100 customers 
library(tidyverse)

# read data
file <- readxl::read_xlsx("data/synthesised-transaction-dataset.xlsx", 
                          trim_ws = TRUE, 
                          col_types = c("text","numeric", "text", "text", "text", "text", "text", "text", "numeric",
                                        "text", "numeric", "date", "text", "numeric", "text", "text", "text", "numeric",
                                        "text", "text", "text", "text","text"))

# Quality checks
visdat::vis_miss(file) +
  theme(
    axis.text = element_text(size = 10)
  ) +
  labs(
    title = "% of Missing Data"
  ) +
  theme(
    plot.background = element_rect(fill = "cornsilk")
  )


#vis_dat(file) + 
 # colorspace::scale_fill_discrete_qualitative()

GGally::ggscatmat(file, columns=c(12, 15, 19), color = "month") +
  scale_colour_brewer(palette="Set1")

# find duplicates
file %>% 
  janitor::get_dupes()


#file[4359,3]
#file[3051,3]

# Separate long and lat
file <- file %>% 
  separate(long_lat, into = c("long", "lat"), sep = "-") %>% 
  separate(merchant_long_lat, into = c("merch_long", "merch_lat"), sep = "-") %>% 
  mutate(long = as.numeric(long),
         lat = as.numeric(lat),
         merch_long = as.numeric(merch_long),
         merch_lat = as.numeric(merch_lat),
         date = lubridate::as_date(date),
         month = month(date),
         day = day(date),
         lat = lat * (-1),
         merch_lat = merch_lat * (-1),
         time = stringr::str_sub(extraction, start = 12, end = 19),
         hour = stringr::str_sub(time, start = 1, end = 2))



customer_map_data <- file %>% 
  select(account,long, lat) %>% 
  distinct()

merch_map_data <-  file %>% 
  select(merch_long, merch_lat, merchant_suburb) %>% 
  na.omit() %>% 
  distinct()


leaflet() %>% 
  addProviderTiles("Stamen.Terrain") %>% 
  addMarkers(lng = customer_map_data$long, lat = customer_map_data$lat, 
             label = customer_map_data$account) %>% 
  addCircleMarkers(lng = merch_map_data$merch_long, lat = merch_map_data$merch_lat, label = merch_map_data$merchant_suburb,
                   color = 'orangee', fill = T, fillColor = 'orange')
 

file %>% 
  group_by(txn_description) %>% 
  summarise(average_amaount = mean(amount))  %>% 
  arrange(desc(average_amaount))

file %>% 
  filter(amount > 100) %>% 
  ggplot(aes(as.factor(month), amount, fill = txn_description)) +
  geom_boxplot()


file %>% 
  filter(amount > 100) %>% 
  ggplot(aes(as.factor(day), amount)) +
  geom_boxplot() +
  coord_flip()



# Find: average transaction amount,  How many transactions do customers make each month, on average?, 
#segment the data by time and visualise them, look at transaction volume and 
# spending over a day or week Consider the effect of any outliers that may distort your analysis.
# what insights can you draw from the location information provided in the dataset?
# Present in 2-3 slides


## Make a Summary of which clients receive the biggest payments and which ones have the most expenses to target
## people for loans
file %>% 
  filter(account == "ACC-1598451071") %>% 
  select(txn_description, balance, amount, date) %>% 
  filter(txn_description == "PAY/SALARY") 
  
income <- file %>% 
  group_by(account, gender) %>% 
  select(txn_description, balance, amount, date, gender) %>% 
  filter(txn_description == "PAY/SALARY") %>% 
  summarise(total_payment = sum(amount)) %>% 
  arrange(desc(total_payment))

expenses <- file %>% 
  group_by(account) %>% 
  select(txn_description, balance, amount, date) %>% 
  filter(txn_description %in% c("POS", "SALES-POS", "PAYMENT")) %>% 
  summarise(total_expenses = sum(amount)) %>% 
  arrange(desc(total_expenses))

client_profile <- inner_join(income, expenses, by = "account") %>% 
  mutate(exp_perc_of_salary = total_expenses/total_payment * 100) %>% 
  arrange(desc(exp_perc_of_salary))


client_profile %>% 
  ggplot(aes(total_payment, y = ..density.., group = gender)) +
  geom_density(aes(fill = gender, color = gender )) +
  geom_histogram(alpha = 0.6, fill = "grey", binwidth = 1000) +
  ggthemes::theme_economist() +
  theme(
    legend.position = "none",
  
  ) +
  labs(
    x = "Salary"
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +
  

client_profile %>% 
  ggplot(aes(total_expenses, y = ..density..)) +
  geom_density(aes(fill = gender, color = gender )) +
  geom_histogram(alpha = 0.6, fill = "grey", binwidth = 500) +
  ggthemes::theme_economist() +
  theme(
    legend.position = "bottom"
  ) +
  labs(
    x = "Total Expenses"
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() +


client_profile %>% 
  ggplot(aes(exp_perc_of_salary, y = ..density..)) +
  geom_density(aes(fill = gender, color = gender )) +
  geom_histogram(alpha = 0.6, fill = "grey") +
  ggthemes::theme_economist() +
  theme(
    legend.position = "none"
  ) +
  labs(
    x = "Expenses as percentage of Salary"
  ) +
  scale_fill_viridis_d() +
  scale_color_viridis_d() 




# average transaction amount
DT::datatable(
file %>% 
  mutate(
    month = case_when(
      month == '8' ~ 'August',
      month == '9' ~ 'September',
      month == '10' ~ 'October'
    )
  ) %>% 
  group_by(txn_description, month) %>% 
  summarise(average_transaction = round(mean(amount, na.rm = TRUE),2)) %>% 
  arrange(txn_description)

)
  



file %>% 
  count(month)

# fraudulent paymenrs as payments occur only business hours

file %>% 
  group_by(txn_description, hour) %>% 
  summarise(total_amount = sum(amount)) %>% 
  ggplot(aes(hour, total_amount, color = txn_description, group = txn_description)) +
  geom_line() 




## Transaction volumes and effect of outliers



install.packages("OutlierDetection")
library(OutlierDetection)


dens(file[,"amount"])



library(FNN)

amount_knn <- get.knn(file[, 'amount'])



file_amount <- file %>% 
  select(date, amount)


file_amount$knn_score <- rowMeans(amount_knn$nn.dist)


file_amount %>% 
  ggplot(aes(date, amount, size = knn_score, color = knn_score)) +
  geom_point()





file %>% 
  ggplot(aes(date, balance, group = account, color = amount)) +
  geom_line()


balance_changes <- file %>%
  select(account, date, amount, balance) %>% 
  group_by(account) %>% 
  arrange(date, .by_group = TRUE) %>%
  mutate(pct_change = (balance/lead(balance) - 1) * 100)



balance_changes %>% 
  filter(account == "ACC-1222300524") %>% 
  arrange(date) 



library(DataCombine)


balance_changes <- file %>% 
  select(date, account, txn_description,  amount, balance) %>% 
PercChange(Var = 'balance',
           type = 'proportion',
           NewVar = 'PercentChange',
           GroupVar = 'account')


balance_changes %>% 
  filter(account == "ACC-559365433") %>% 
  arrange(date)


balance_changes %>% 
  ggplot(aes(date, PercentChange, group = account, color = PercentChange)) +
  geom_line() + 
  geom_text(data = balance_changes %>% filter(PercentChange > 5000), aes(label = account)) 

balance_changes %>% 
  filter(account == "ACC-1598451071" ) %>% 
  ggplot(aes(date, amount, color = txn_description)) +
  geom_point() 
  

balance_changes %>% 
  filter(balance < 100000) %>% 
  ggplot(aes(date, balance, group = account)) +
  geom_line() 
  



file %>% 
  group_by(account) %>% 
  count(txn_description) %>% 
  filter(txn_description == "PAY/SALARY") %>% 
  arrange(desc(n))



# who got an increase

file %>% 
  group_by(account) %>% 
  filter(txn_description == "PAY/SALARY") %>% 
  ggplot(aes(date, amount, group = account)) +
  geom_line() 





file %>% 
  group_by(hour, txn_description) %>% 
  count() %>% 
  ggplot(aes(hour, n, fill = txn_description)) +
  geom_col() + 
  facet_wrap(~txn_description) +
  theme_classic() +
  ggthemes::scale_fill_economist() +
  labs(
    x = 'Hour',
    y = "Transaction Volume"
  )






library(ggalluvial)


file %>% 
  mutate(
    month = case_when(
      month == '8' ~ 'August',
      month == '9' ~ 'September',
      month == '10' ~ 'October'
    )
  ) %>% 
ggplot(
       aes(axis1 = gender, axis2 = month, axis3 = txn_description,
           y = log(balance))) +
  scale_x_discrete(limits = c("Gedner", "Month", "Transaction Type"), expand = c(.2, .05)) +
  xlab("Demographic") +
  geom_flow(width = 1/4) +
  geom_alluvium(aes(fill = amount)) +
  geom_stratum() +
  geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
  theme_minimal() +
  ggtitle("Transaction Route",
          "stratified by demographics and transaction type") +
  theme(
    text = element_text(size = 12),
    axis.text.y = element_blank(),
    axis.title = element_blank()
  ) +
  labs(
    fill = "Amount"
  ) +
  scale_fill_continuous()
  


hist(file$balance)



          