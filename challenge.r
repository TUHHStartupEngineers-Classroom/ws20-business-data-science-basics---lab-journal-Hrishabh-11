library("tidyverse")
library("lubridate")
?library("readxl")
bikes_tbl <- read_excel("Project Files/00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("Project Files/00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl  <- read_excel("Project Files/00_data/01_bike_