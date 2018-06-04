master_result <- tibble(
    V = as.numeric(),
    L = as.numeric(),
    h = as.numeric(),
    RMSE = as.numeric(),
    MAE = as.numeric()
)

moa_df <- dengueDF.sj %>% select(week_start_date, total_cases, station_precip_mm)
moa_ts <- dengueTS.sj[, c('total_cases', 'station_precip_mm')]

moa_ts$station_precip_lag4 <- lag(moa_ts$station_precip_mm, n = 4)
moa_ts <- moa_ts[-1:-4, -2]

zoo::plot.zoo(moa_ts)

# Number of parameter sequences to use
V <- 8
# Length of parameter sequence
L <- 4
# Forecast horizon
h <- 1

moa_ts <- cbind(moa_ts,moa_ts[,'total_cases'])
colnames(moa_ts) <- c(colnames(moa_ts[,1:2]),'total_cases_original')
head(moa_ts)

week_index <- index(moa_ts)
first_target_index <- 760
last_target_index <- length(week_index) - h
last_target_date <- week_index[last_target_index]
# itx <- iter(first_target_index:last_target_index)
for (now_index in first_target_index:last_target_index) {
    t_now <- week_index[now_index]
    t_now_minus_L = week_index[now_index - L + 1]
    target_sequence <- moa_ts[paste0(t_now_minus_L, '/', t_now)]

    moa_search_ts <-
        moa_ts[paste0('/', t_now_minus_L - 1)]
    search_index <-
        index(moa_search_ts)
    result_length <-
        length(search_index) - (L + h)
    result_matrix <-
        matrix(nrow = result_length,
               ncol = 3,
               dimnames = list(NULL, c('i_val', 'euc_dist', 'h')))
    for (i in 1:result_length) {
        search_from_date <- search_index[i]
        search_end_date <-
            search_index[i + L - 1]
        search_sequence <-
            moa_search_ts[paste0(search_from_date, '/', search_end_date)]
        dist <- sum((rowSums((
                coredata(search_sequence) - target_sequence
            ) ^ 2)) ^ 0.5)
        h_date <- search_index[i + L - 1 + h]
        h_vals <- moa_search_ts[h_date, 'total_cases'][[1]]
        result_matrix[i, ] <- c(i, dist, h_vals)
    }
    result_subset <- result_matrix[order(result_matrix[, 'euc_dist']),][1:V,]
    h_hat <- mean(result_subset[, 'h'])
    h_actual <- moa_ts[now_index + h, 'total_cases'][[1]]
    moa_ts[now_index + h, 'total_cases'] <- h_hat
    # c(as.character(week_index[now_index + h]), h_actual, h_hat)
}

resultTS <- moa_ts[first_target_index:last_target_index,c(1,3)]
plot.xts(resultTS, legend.loc = 'topright')

master_result %<>%
    bind_rows(c(
        V = V,
        L = L,
        h = h,
        RMSE = RMSE(resultTS$total_cases, resultTS$total_cases_original),
        MAE = MAE(resultTS$total_cases, resultTS$total_cases_original)
    ))
master_result

RMSE(resultTS$total_cases, lag(resultTS$total_cases_original,3),na.rm=T)

resultTS_lagged <- resultTS
resultTS_lagged$total_cases_original = lag(resultTS_lagged$total_cases_original,3)
plot.xts(resultTS_lagged, legend.loc = 'topright')


# ------------------------ TEST SET -----------------------
testTS <- testingSubmission %>% filter(city=='sj') %>% select(week_start_date, station_precip_mm)
testTS <- xts(testTS$station_precip_mm, order.by = as.Date(testTS$week_start_date))
table(is.na(testTS[,1]))
testTS <- zoo::na.approx(testTS)
colnames(testTS) <- 'station_precip_mm'

moa_ts <- dengueTS.sj[, c('total_cases', 'station_precip_mm')]

moa_ts$station_precip_lag4 <- lag(moa_ts$station_precip_mm, n = 4)


rbind(testTS, moa_ts    )

testTS <- cbind(testTS,NA)
colnames(testTS) <- c('station_precip_mm','total_cases')
head(testTS)
plot(testTS)

test_start_date <- index(first(testTS))

testTS <- rbind(moa_ts[,2:3], testTS)

# Number of parameter sequences to use
V <- 8
# Length of parameter sequence
L <- 4
# Forecast horizon
h <- 1

testTS$station_precip_lag4 <- lag(testTS$, n = 4)








# moa_result <- matrix(nrow = length(first_target_index:last_target_index), ncol = 3,
#                      dimnames = list(as.character(week_index[first_target_index:last_target_index]), c('now_index','h','h_hat')))
# moa_result[,'now_index'] <- first_target_index:last_target_index
# for (now_index in first_target_index:last_target_index) {
#     cat('\n*** Processing now_index', now_index)
#     t_now <- week_index[now_index]
#     last_target_index <- length(week_index) - h
#     last_target_date <- week_index[last_target_index]
#     cat('Now is', as.character(t_now))
#
#     t_now_minus_L = week_index[now_index - L]
#     target_sequence <- moa_ts[paste0(t_now_minus_L, '/', t_now)]
#     moa_search_ts <- moa_ts[paste0('/', t_now)]
#     search_index <- index(moa_search_ts)
#     result_length <- length(search_index) - L - h - 1
#     result_matrix <-
#         matrix(nrow = result_length,
#                ncol = 3,
#                dimnames = list(NULL, c('i_val', 'euc_dist', 'h')))
#
#     for (i in 1:result_length) {
#         search_from_date <- search_index[i]
#         search_end_date <- search_index[i + L]
#         cat(
#             '\nSeq Number:',
#             i,
#             'Searching in sequence',
#             as.character(search_from_date),
#             'to',
#             as.character(search_end_date)
#         )
#         search_sequence <-
#             moa_search_ts[paste0(search_from_date, '/', search_end_date)]
#         dist <-
#             sum((rowSums((
#                 coredata(search_sequence) - target_sequence
#             ) ^ 2)) ^ 0.5)
#         h_date <- search_index[i + L + h]
#         h_vals <- moa_search_ts[h_date, 'total_cases'][[1]]
#         result_matrix[i,] <- c(i, dist, h_vals)
#     }
#     # head(result_matrix)
#     result_subset <- result_matrix[order(result_matrix[, 'euc_dist']), ][1:V, ]
#     # result_subset
#     h_hat <- mean(result_subset[, 'h'])
#     # h_hat
#     t_h <- week_index[now_index + h]
#     h_actual <- moa_ts[t_h][,'total_cases'][[1]]
#     moa_result[moa_result[,'now_index']==now_index,2:3] <- c(h_actual, h_hat)
# }