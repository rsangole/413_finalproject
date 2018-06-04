head(dengueTS.sj,30)
x <- ts(dengueTS.sj$reanalysis_avg_temp_k)
plot(x)

x.train <- ts(x[1:700], frequency = 52)
plot(x.train)
x.test <- ts(x[701:900])
plot(x.test)

# Model 1
x.train %>%
    forecast::nnetar(repeats = 10,size = 6) %>%
    forecast::forecast(h=200) %>%
    autoplot()+
    forecast::autolayer(ts(x.test, start=201))

# Model 2
par('mar'=c(1.2,3,1,1))
stl(x.train, s.window = 'p') %>% plot
forecast::tsdisplay(x.train)
forecast::tsdisplay(diff(x.train))
x.train %>%
    forecast::stlf(y = ., h = 200, s.window = 'periodic', robust = T) %>%
    autoplot()
