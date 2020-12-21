# 01 Load Libraries
library("tidyverse")
library("lubridate")
library("readxl")
bikes_tbl <- read_excel("Project Files/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("Project Files/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("Project Files/00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")
bike_orderlines_joined_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>%
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))
bike_orderline_wrangled_tbl <- bike_orderlines_joined_tbl%>% 
  separate(col = category, into = c("category.1", "category.2", "category.3"), sep = "-") %>% 
  mutate(total_price = price*quantity)%>%
  select(-...1,-gender) %>%
  select(-ends_with(".id"))%>%
  bind_cols(bike_orderlines_joined_tbl%>%select(order.id))%>%
  select(order.id, contains("order"), contains("model"), contains("category"),
         price, quantity, total_price,
         everything()) %>%
  rename(bikeshop = name) %>%
  set_names(names(.) %>% str_replace_all("\\.", "_"))

sales_by_year_tbl <- bike_orderline_wrangled_tbl%>%select(order_date, total_price)%>%
  mutate(year = year(order_date))%>%
  group_by(year)%>%summarise(sales = sum(total_price))%>%
  mutate(sales_text = scales::dollar(sales, big.mark = ".", 
                                     decimal.mark = ",", 
                                     prefix = "", 
                                     suffix = " ???"))
sales_by_year_tbl%>% ggplot(aes(x = year, y= sales)) + 
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) + 
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", 
                                                    decimal.mark = ",", 
                                                    prefix = "", 
                                                    suffix = " ???")) +
  labs(
    title    = "Revenue by year",
    subtitle = "Upward Trend",
    x = "", # Override defaults for x and y
    y = "Revenue"
  )

sales_by_year_n_cat = bike_orderline_wrangled_tbl %>% 
  select(order_date,category_1,total_price)%>%
  mutate(year = year(order_date))%>%
  group_by(year, category_1)%>%
  summarise(sales = sum(total_price))%>%
  ungroup() %>%
  mutate(sales_txt = scales::dollar(sales, big.mark = ".", decimal.mark = ",",
                                    prefix = "", suffix = "$"))
sales_by_year_n_cat %>% ggplot(aes(x= year, y = sales, fill = category_1)) + 
  geom_col() +
  facet_wrap(~ category_1)+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".", decimal.mark = ",",
                                                    prefix = "", suffix = "$"))+
  labs(title = "Revenue by Yr and Main Cat",
       subtitle = "Each Cat has an Upward trend",
       fill = "Main Category")
install.packages("writexl")
library("writexl")
bike_orderline_wrangled_tbl%>% write_xlsx("Project Files/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.xlsx")
bike_orderline_wrangled_tbl%>% write_csv("Project Files/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.csv")
bike_orderline_wrangled_tbl%>% write_rds("Project Files/00_data/01_bike_sales/02_wrangled_data/bike_orderlines.rds")
bike_orderline_wrangled_new_table <- bike_orderlines_joined_tbl%>%
  separate(col = location, into = c("city", "state"), sep = ",")%>%
  select(-...1, -gender, -ends_with(".id"))%>%
  bind_cols(bike_orderlines_joined_tbl%>%select(order.id))%>%
  mutate(total_price = quantity * price)%>%
  select(order.id,contains ("order"),
         contains("model"), contains("state"),
         price, quantity, total_price, everything())
sales_by_state <-bike_orderline_wrangled_new_table %>%
  select(state, total_price)%>%
  group_by(state)%>%
  summarise(Total_sales = sum(total_price))%>%
  mutate(sales_text = scales::dollar(Total_sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = "$"))
sales_by_state%>% ggplot(aes(x= state, y= Total_sales)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text))+
  geom_smooth(method = "lm", se =FALSE)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",",
                                                    decimal.mark = ".",
                                                    prefix = "",
                                                    suffix = "$"))+
  labs(title = "Statewise Sales",
       fill = "Main Category")

sales_by_year_state <- bike_orderline_wrangled_new_table%>%
  select(order.date, state, total_price)%>%
  mutate(year = year(order.date))%>%
  group_by(year, state)%>%
  summarise(tot_sales = sum(total_price))%>%
  ungroup()%>%
  mutate(sales_text = scales::dollar(tot_sales, big.mark = ",", 
                                     decimal.mark = ".",
                                     prefix = "",
                                     suffix = "$"))

sales_by_year_state %>% ggplot(aes(x= year, y = tot_sales, fill = state)) +
  geom_col()+
  facet_wrap(~state)+
  theme(axis.text.x = element_text(angle = 60, hjust = 1))+
  scale_y_continuous(labels = scales::dollar_format(big.mark = ",",
                                                    decimal.mark = ".",
                                                    prefix = "",
                                                    suffix = "$"))+
  labs(title = "Sales by year and state",
       fill = "State")
  
  