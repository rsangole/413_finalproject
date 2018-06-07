if(config$model_number == 1){

    dengueFeatures$week_start_date <- as.Date(dengueFeatures$week_start_date)

    dengueFeatures %>% skimr::skim()
    dengueLabels %>% skimr::skim()

    dengueDF <- dengueFeatures %>%
        left_join(dengueLabels)

    dengueTS <- dengueDF %>%
        mutate(city = ifelse(city=='sj',1,0)) #SJ is 1, IQ is 0

    dengueTS <- xts(x = dengueTS[,c(-2,-4)], order.by = dengueTS$week_start_date)
    head(dengueTS)

    predictors <- c('weekofyear','ndvi_ne','ndvi_nw','ndvi_se','ndvi_sw','precipitation_amt_mm','reanalysis_air_temp_k','reanalysis_avg_temp_k','reanalysis_dew_point_temp_k','reanalysis_max_air_temp_k','reanalysis_min_air_temp_k','reanalysis_precip_amt_kg_per_m2','reanalysis_relative_humidity_percent','reanalysis_sat_precip_amt_mm','reanalysis_specific_humidity_g_per_kg','reanalysis_tdtr_k','station_avg_temp_c','station_diur_temp_rng_c','station_max_temp_c','station_min_temp_c','station_precip_mm')
    y <- 'total_cases'

    cache('dengueDF')
    cache('dengueTS')

    dengueDF.sj <- dengueDF %>% filter(city == 'sj') %>% select(-city)
    dengueDF.iq <- dengueDF %>% filter(city == 'iq') %>% select(-city)
    dengueTS.sj <- dengueTS[dengueTS$city==1,-1]
    dengueTS.iq <- dengueTS[dengueTS$city==0,-1]

    map(ls(pattern = 'DF'),~cache(.x))
    map(ls(pattern = 'TS'),~cache(.x))
}
