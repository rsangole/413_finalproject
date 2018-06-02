dengueTS

map_df(dengueDF, ~lag(.x)) %>%
    select(-city, -weekofyear) -> lagDF
names(lagDF) <- paste0(names(lagDF),'_lag1')
map_df(dengueDF, ~lag(.x,2)) %>%
    select(-city, -weekofyear) -> lagDF2
names(lagDF2) <- paste0(names(lagDF2),'_lag2')
lagDF

lagTS <- xts(lagDF, order.by = index(dengueTS))
lag2TS <- xts(lagDF2, order.by = index(dengueTS))

nnetTS <- xts::cbind.xts(dengueTS, lagTS, lag2TS)

nnetTS <- nnetTS %>% zoo::na.approx()

nnet::nnet(formula = total_cases~., data = as_tibble(nnetTS),
           size = c(6,6), maxit = 1000)
