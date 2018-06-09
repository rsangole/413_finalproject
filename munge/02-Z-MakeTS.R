if(config$model_number == 2) {
    dengueTS <- dengueDF %>%
        mutate(city = ifelse(city == 'sj', 1, 0)) #SJ is 1, IQ is 0

    dengueTS <-
        xts(x = dengueTS[, c(-2, -4)], order.by = dengueTS$week_start_date)
    head(dengueTS)

    dengueTS.sj <- dengueTS[dengueTS$city == 1, -1]
    dengueTS.iq <- dengueTS[dengueTS$city == 0, -1]

    map(ls(pattern = 'TS'),  ~ cache(.x))
}