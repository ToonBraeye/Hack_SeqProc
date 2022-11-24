
library(tidyverse)

Data <- readRDS("C:/Users/LoVa3397/Downloads/test_df (3).Rdata")

Startperiod <- "2022-01-01"
Endperiod   <- "2022-07-31"

Data_long <- gather(Data, vaccin, day,date.vacc1:date.vacc2, factor_key=TRUE)
Data_long <- Data_long[complete.cases(Data_long),]
