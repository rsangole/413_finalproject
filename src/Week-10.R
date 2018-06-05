head(dengueTS.sj,30)
x <- ts(dengueTS.sj$reanalysis_avg_temp_k)
plot(x)

x.train <- ts(x[1:700], frequency = 52)
plot(x.train)
x.test <- ts(x[701:900], frequency = 52)
plot(x.test)

xtest = ts(x.test, start = 14.46154, frequency = 52)

# Model 1
# wk_num = rep(1:52,length = length(dengueTS.sj))
wk_num = dengueTS.sj$weekofyear
wk_num.train = wk_num[1:700]
wk_num.test = wk_num[701:900]

size_ <- 2:30
p_ <- 1:2
grid <- expand.grid(size_,p_, NA)
names(grid) <- c('size_','p_','test_mase')
ITX <- iter(grid,by = 'row')
while(1){
    X = nextElem(ITX)
    nnetFit <- x.train %>%
        forecast::nnetar(repeats = 10,size = X[[1]], p = X[[2]], P = 1, xreg = wk_num.train) %>%
        forecast::forecast(h=200, xreg = wk_num.test)
    test_MASE <- accuracy(nnetFit, x = x.test)[2,6]
    grid[as.numeric(rownames(X)),3] <- test_MASE
}
grid
xyplot(test_mase~size_|p_, groups=p_,grid)

nnetFit <- x.train %>%
    forecast::nnetar(repeats = 10,size = 11, p = 1, P = 1, xreg = wk_num.train) %>%
    forecast::forecast(h=200, xreg = wk_num.test)
accuracy(nnetFit, x = ts(x.test,frequency = 52, start = c(14,25)))
nnetFit %>%
    autoplot() +
    forecast::autolayer(xtest)

# Model 2
xtest = ts(x.test, start = 14.46154, frequency = 52)
par('mar'=c(1.2,3,1,1))
stl(x.train, s.window = 'p') %>% plot
forecast::ggtsdisplay(x.train)
forecast::ggtsdisplay(diff(x.train))
arimaFit <- auto.arima(x.train)
arimaFit
arimaFit %>%
    forecast(h=200) %>%
    accuracy(x = ts(x.test,frequency = 52, start = c(14,25)))
arimaFit %>%
    forecast(h=200) %>%
    autoplot()+
    forecast::autolayer(xtest,alpha=0.8)

# Model 3
stlfFit <- x.train %>%
    forecast::stlf(y = ., h = 200, s.window = 'periodic', robust = T)
stlfFit %>%
    autoplot()+
    forecast::autolayer(xtest,alpha=0.8)
accuracy(stlfFit, xtest)

