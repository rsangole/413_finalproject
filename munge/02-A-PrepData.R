if(config$model_number == 2) {
    cat('\nRunning Model 2\n')

    dengueFeatures$week_start_date <- as.Date(dengueFeatures$week_start_date)

    dengueFeatures %>% skimr::skim()
    dengueLabels %>% skimr::skim()

    dengueDF <- dengueFeatures %>% left_join(dengueLabels)

    dengueDF.sj <- dengueDF %>% filter(city == 'sj') %>% dplyr::select(-city)
    dengueDF.iq <- dengueDF %>% filter(city == 'iq') %>% dplyr::select(-city)

    dengueDF.sj <- convert_K_to_C(dengueDF.sj)
    dengueDF.iq <- convert_K_to_C(dengueDF.iq)

    map(ls(pattern = 'DF'),  ~ cache(.x))
}