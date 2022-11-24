library(data.table)

## VEBIS - simulated data
set.seed(123)
test.df <- data.table(id=c(1:1000), date.vacc1=as.Date('2020-01-01')+sample(c(1:(2*365)), 1000, replace=T))

#Add an additional vaccination
test.df[, date.vacc2:=as.Date(ifelse(sample(1:2, 1000, replace=T) %% 2, 
                                     as.Date(date.vacc1) + sample(c(1:365), replace=T, 1000), NA), origin='1970-01-01')]

saveRDS(test.df, 'data/test_df.Rdata')
write.csv(test.df, 'data/test_df.csv')
