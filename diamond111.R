diamonds2 <- readRDS("diamonds2.rds")
diamonds2 %>% pivot_longer(cols = c('2008','2009'), names_to = 'year', 
                           values_to = 'price')%>%
head(n=5)
library("tidyverse")
diamonds3 <- readRDS("diamonds3.rds")
diamonds3
diamonds3 %>% pivot_wider(names_from='dimension', values_from ='measurement') %>%
head(n=5)

diamonds4 <- readRDS("diamonds4.rds")
diamonds4
diamonds4 %>% separate(col = 'dim', into = c('x', 'y', 'z'), sep = "[/]")

diamonds5 <- readRDS("diamonds5.rds")
diamonds5
diamonds5 %>% unite(col = clarity, clarity_prefix:clarity_suffix, remove = T)