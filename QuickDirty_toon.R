
library(tictoc)

tic()

Startperiod <- as.Date("2022-01-01")
Endperiod   <- as.Date("2022-07-31")
Studyperiod <- seq(from=Startperiod, to=Endperiod, by='day')

## Starting from test.df: Every date for every person
test.df2 <- CJ('id'=test.df$id, Studyperiod)

test.df3 <- merge(test.df2, test.df, by='id')

## Add the vaccination status

test.df3[, vacc.status1:=(ifelse(date.vacc1<Studyperiod & !is.na(date.vacc1), 1, 0) + ifelse(date.vacc2<Studyperiod & !is.na(date.vacc2), 1, 0))]
test.df3[, last.vacc.date:=ifelse(vacc.status1==2, date.vacc2, ifelse(vacc.status1==1, date.vacc1, NA))]
test.df3[, vacc2study:=cut(as.numeric(Studyperiod-last.vacc.date), c(0,30,60,90, Inf))]
test.df3[, vacc.status2:=paste(vacc.status1, vacc2study, sep='_')]

## Remove duplicates 
test.df4 <- test.df3[!duplicated(test.df3[,c('id', 'vacc.status2')])]

## Add an end date to the records
test.df4[, end.date:=shift(Studyperiod, type='lead', fill=NA), by=id]
test.df4[is.na(end.date), end.date:=Endperiod]

toc()