dengueFeatures$week_start_date <- as.Date(dengueFeatures$week_start_date)

dengueFeatures %>% skimr::skim()
dengueLabels %>% skimr::skim()

df <- dengueFeatures %>%
    left_join(dengueLabels)
df

df <- dengueFeatures %>%
    left_join(dengueLabels) %>%
    mutate(city = ifelse(city=='sj',1,0))

dengueTS <- xts(x = df[,c(-2,-4)], order.by = df$week_start_date)
head(dengueTS)

# dengueTS$y_class = ifelse(dengueTS$total_cases>0,1,0)

as_tibble(dengueTS) %>%
    mutate(time = index(dengueTS)) %>%
    write_csv(path = 'data/dengueTS.csv',col_names = T)

predictors <- c('city','weekofyear','ndvi_ne','ndvi_nw','ndvi_se','ndvi_sw','precipitation_amt_mm','reanalysis_air_temp_k','reanalysis_avg_temp_k','reanalysis_dew_point_temp_k','reanalysis_max_air_temp_k','reanalysis_min_air_temp_k','reanalysis_precip_amt_kg_per_m2','reanalysis_relative_humidity_percent','reanalysis_sat_precip_amt_mm','reanalysis_specific_humidity_g_per_kg','reanalysis_tdtr_k','station_avg_temp_c','station_diur_temp_rng_c','station_max_temp_c','station_min_temp_c','station_precip_mm')
y <- 'total_cases'


dengueDF <- as_tibble(dengueTS) %>%
    mutate(city = ifelse(city==1,'sj','iq'))
