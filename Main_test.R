
library(tidyverse)
library(dplyr)
library(tictoc)
library(data.table)
library(DataCombine)

tic() 

Data <- readRDS("C:/Users/LoVa3397/Downloads/test_df (3).Rdata")
 
Startperiod2   <- as.Date("2022-01-01")
Startperiod <- as.Date("2022-01-01") - 90 
Endperiod   <- as.Date("2022-07-31")

Data$date.vacc1_ <- if_else(Data$date.vacc1 >= Startperiod & Data$date.vacc1 <= Endperiod, Data$date.vacc1, as.Date(NA))
Data$date.vacc2_ <- if_else(Data$date.vacc2 >= Startperiod & Data$date.vacc2 <= Endperiod, Data$date.vacc2, as.Date(NA))

Data <- Data[!(is.na(Data$date.vacc1_) & is.na(Data$date.vacc2_)),c(1,4,5)] 
colnames(Data) <- c("id", "date.vacc1", "date.vacc2")

Data_coca <- Data[complete.cases(Data),]
Data_nococa <- Data[!complete.cases(Data),]

Data_coca$date.vacc1_90 <- if_else(Data_coca$date.vacc1 + 90 <= Endperiod & Data_coca$date.vacc1 + 90 < Data_coca$date.vacc2, 
                                   Data_coca$date.vacc1 + 90, as.Date(NA))
Data_coca$date.vacc1_60 <- if_else(Data_coca$date.vacc1 + 60 <= Endperiod & Data_coca$date.vacc1 + 60 < Data_coca$date.vacc2, 
                                   Data_coca$date.vacc1 + 60, as.Date(NA))
Data_coca$date.vacc1_30 <- if_else(Data_coca$date.vacc1 + 30 <= Endperiod & Data_coca$date.vacc1 + 30 < Data_coca$date.vacc2, 
                                   Data_coca$date.vacc1 + 30, as.Date(NA))

Data_coca$date.vacc2_90 <- if_else(Data_coca$date.vacc2 + 90 <= Endperiod , 
                                   Data_coca$date.vacc2 + 90, as.Date(NA))
Data_coca$date.vacc2_60 <- if_else(Data_coca$date.vacc2 + 60 <= Endperiod , 
                                   Data_coca$date.vacc2 + 60, as.Date(NA))
Data_coca$date.vacc2_30 <- if_else(Data_coca$date.vacc2 + 30 <= Endperiod , 
                                   Data_coca$date.vacc2 + 30, as.Date(NA))


Data_nococa$date.vacc2_90 <- if_else(Data_nococa$date.vacc2 + 90 <= Endperiod , 
                                   Data_nococa$date.vacc2 + 90, as.Date(NA))
Data_nococa$date.vacc2_60 <- if_else(Data_nococa$date.vacc2 + 60 <= Endperiod , 
                                   Data_nococa$date.vacc2 + 60, as.Date(NA))
Data_nococa$date.vacc2_30 <- if_else(Data_nococa$date.vacc2 + 30 <= Endperiod , 
                                   Data_nococa$date.vacc2 + 30, as.Date(NA))

Data_nococa$date.vacc1_90 <- if_else(Data_nococa$date.vacc1 + 90 <= Endperiod , 
                                     Data_nococa$date.vacc1 + 90, as.Date(NA))
Data_nococa$date.vacc1_60 <- if_else(Data_nococa$date.vacc1 + 60 <= Endperiod , 
                                     Data_nococa$date.vacc1 + 60, as.Date(NA))
Data_nococa$date.vacc1_30 <- if_else(Data_nococa$date.vacc1 + 30 <= Endperiod , 
                                     Data_nococa$date.vacc1 + 30, as.Date(NA))

Data_full <- bind_rows(Data_coca, Data_nococa)
Data_full$Startperiod <- Startperiod2
Data_full$Endperiod <- Endperiod

Data_full_select <- data.frame(id=Data_full$id)
Data_full_select$date.vacc1 <- if_else(Data_full$date.vacc1 >= Startperiod2 & Data_full$date.vacc1 <= Endperiod, Data_full$date.vacc1, as.Date(NA))
Data_full_select$date.vacc1_30 <- if_else(Data_full$date.vacc1_30 >= Startperiod2 & Data_full$date.vacc1_30 <= Endperiod, Data_full$date.vacc1_30, as.Date(NA))
Data_full_select$date.vacc1_60 <- if_else(Data_full$date.vacc1_60 >= Startperiod2 & Data_full$date.vacc1_60 <= Endperiod, Data_full$date.vacc1_60, as.Date(NA))
Data_full_select$date.vacc1_90 <- if_else(Data_full$date.vacc1_90 >= Startperiod2 & Data_full$date.vacc1_90 <= Endperiod, Data_full$date.vacc1_90, as.Date(NA))

Data_full_select$date.vacc2 <- if_else(Data_full$date.vacc2 >= Startperiod2 & Data_full$date.vacc2 <= Endperiod, Data_full$date.vacc2, as.Date(NA))
Data_full_select$date.vacc2_30 <- if_else(Data_full$date.vacc2_30 >= Startperiod2 & Data_full$date.vacc2_30 <= Endperiod, Data_full$date.vacc2_30, as.Date(NA))
Data_full_select$date.vacc2_60 <- if_else(Data_full$date.vacc2_60 >= Startperiod2 & Data_full$date.vacc2_60 <= Endperiod, Data_full$date.vacc2_60, as.Date(NA))
Data_full_select$date.vacc2_90 <- if_else(Data_full$date.vacc2_90 >= Startperiod2 & Data_full$date.vacc2_90 <= Endperiod, Data_full$date.vacc2_90, as.Date(NA))
Data_full_select$startperiod <- Startperiod2
Data_full_select$endperiod <- Endperiod

Data_long <- gather(Data_full_select, status, day, date.vacc1:endperiod, factor_key=FALSE)
Data_long <- Data_long[complete.cases(Data_long),]

Data_long <- Data_long[order(Data_long$id, Data_long$day),]
Data_long$status2 <- shift(Data_long$status,1,"id")
Data_long$day2 <- shift(Data_long$day,1,"id")

Data_long <- Data_long[!(Data_long$status == "endperiod" & Data_long$status2 == "startperiod"),]

Data_long$state <- paste0(Data_long$status, "_",Data_long$status2)

Data_long <- Data_long[c(1,6,3,5)]
colnames(Data_long) <- c("id","status","startdate","enddate")

# Data <- readRDS("C:/Users/LoVa3397/Downloads/test_df (3).Rdata")

Longterm <- data.frame(id=seq(1,1000,by=1))
# Longterm$startdate <- Startperiod2
# Longterm$enddate <- Endperiod

Final <- merge(Longterm, Data_long, by ="id", all.x=TRUE)
Final$status <- ifelse(is.na(Final$status), "longer",Final$status)
Final$enddate <- if_else(is.na(Final$enddate), Endperiod,Final$enddate)
Final$startdate <- if_else(is.na(Final$startdate), Startperiod2,Final$startdate)
# Final <- Final[,c(1,2,3,6,5)]

toc()