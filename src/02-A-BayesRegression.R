predictors <- names(dengueDF.sj.train)[c(-1,-3,-19)]
response <- 'total_cases'

# bridgeFit <- train(x = dengueDF.sj.train[predictors],
#       y = dengueDF.sj.train[[response]],
#       method = 'blassoAveraged',
#       trControl = trainControl(method = 'none'))
#
# bridgeFit
#
# bridgePreds <- predict(bridgeFit, dengueDF.sj.test)
#
# plot(dengueDF.sj.test[[response]],
#      bridgePreds,
#      xlab = 'Y actual',
#      ylab = 'Y hat')
#
# RMSE(pred = bridgePreds,
#      obs = dengueDF.sj.test[[response]])
# MAE(pred = bridgePreds,
#     obs = dengueDF.sj.test[[response]])
#
# plot(bridgeFit$finalModel)
#

# Add lagged variables

add_lagged <- function(x, l, name){
    map(.x = l, .f = ~make_cols(.x, x, name)) %>%
        bind_cols()
}
make_cols <- function(l, x, name){
    t <- tibble(lag(x, l))
    colnames(t) <- name
    t
}

lags <- c(4,6,8,51,52)
to_lag <- dengueDF.sj.train[,c(-1:-3,-19)]
lagged_df <- map2(.x = to_lag, .y = names(to_lag), ~add_lagged(x = .x, l = lags, name = .y)) %>% bind_cols()

dengueDF.sj.train <- dengueDF.sj.train[,c(1,2,3,19)] %>%
    bind_cols(lagged_df)

dengueDF.sj.train <- dengueDF.sj.train[-1:-55,]

bridgeFit <- train(x = as.matrix(dengueDF.sj.train[,c(-1,-3,-4)]),
                   y = dengueDF.sj.train[['total_cases']],
                   method = 'blasso',
                   trControl = trainControl(method = 'none'))

bridgeFit

to_lag <- dengueDF.sj.test[,c(-1:-3,-19)]
lagged_df <- map2(.x = to_lag, .y = names(to_lag), ~add_lagged(x = .x, l = lags, name = .y)) %>% bind_cols()
dengueDF.sj.test <- dengueDF.sj.test[,c(1,2,3,19)] %>%
    bind_cols(lagged_df)
dengueDF.sj.test <- dengueDF.sj.test[-1:-55,]
bridgePreds <- predict(bridgeFit, dengueDF.sj.test)

plot(dengueDF.sj.test[['total_cases']],
     bridgePreds,
     xlab = 'Y actual',
     ylab = 'Y hat')
abline(a = 0, b = 1, col='red')

RMSE(pred = bridgePreds,
     obs = dengueDF.sj.test[[response]])
MAE(pred = bridgePreds,
    obs = dengueDF.sj.test[[response]])

plot(bridgeFit$finalModel)

names(bridgeFit$finalModel$.betas) <- names(dengueDF.sj.train[,c(-1,-3,-4)])

bridgeFit$finalModel$.betas %>% sort() %>% dotplot(panel = function(...){
    panel.dotplot(...)
    panel.abline(v = 0, col='gray')
})




lassoFit <- train(x = as.matrix(dengueDF.sj.train[,c(-1,-3,-4)]),
      y = dengueDF.sj.train[['total_cases']],
      method = 'xgbLinear',
      trControl = trainControl(method = 'cv'))
plot(lassoFit)
lassoFit$finalModel
lassoPreds <- predict(lassoFit, dengueDF.sj.test)

RMSE(pred = lassoPreds,
     obs = dengueDF.sj.test[[response]])
MAE(pred = lassoPreds,
    obs = dengueDF.sj.test[[response]])
