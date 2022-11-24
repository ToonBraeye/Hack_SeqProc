
library(tidyverse)
library(dplyr)
library(tictoc)

tic() 

Data <- readRDS("C:/Users/LoVa3397/Downloads/test_df (3).Rdata")
 
Startperiod2   <- as.Date("2022-01-01")
Startperiod <- as.Date("2022-01-01") - 90 
Endperiod   <- as.Date("2022-07-31")

Data_long <- gather(Data, vaccin, day, date.vacc1:date.vacc2, factor_key=TRUE)
Data_long <- Data_long[complete.cases(Data_long),]

Data_long_select <- Data_long[Data_long$day >= Startperiod & Data_long$day <= Endperiod, ]


Data_long_select$date.90 <- if_else(Data_long_select$day + 90 <= Endperiod, 
                                      Data_long_select$day + 90, as.Date(NA))
Data_long_select$date.60 <- if_else(Data_long_select$day + 60 <= Endperiod, 
                                      Data_long_select$day + 60, as.Date(NA))
Data_long_select$date.30 <- if_else(Data_long_select$day + 30 <= Endperiod, 
                                       Data_long_select$day + 30, as.Date(NA))
Data_long_select_long <- gather(Data_long_select, status, day2,date.30:date.90, factor_key=FALSE)

Data_long_select_long$status2 <- paste0(Data_long_select_long$vaccin,substr(Data_long_select_long$status,5,7))

Data_part1 <- Data_long_select[,c(1,2,3)]
Data_part2 <- Data_long_select_long[,c(1,5,6)] 
colnames(Data_part2) <- c("id","day","vaccin")

Data_complete <- bind_rows(Data_part1,Data_part2)

Data_complete_select <- Data_complete[Data_complete$day >= Startperiod2 & Data_complete$day <= Endperiod, ]

Longterm <- Data[,c(1)]
Longterm$Startperiod <- Startperiod2
Longterm$Endperiod <- Endperiod

Final <- merge(Longterm, Data_complete_select, by ="id", all.x=TRUE)
Final$status <- ifelse(is.na(Final$vaccin), "longer",Final$vaccin)
Final <- Final[,c(1,2,3,6,5)]

toc()