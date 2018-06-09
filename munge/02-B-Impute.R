if(config$model_number == 2) {

    sj_mice <- dengueDF.sj %>% dplyr::select(-total_cases) %>% mice()
    dengueDF.sj <- tbl_df(complete(sj_mice)) %>%
        bind_cols(dengueDF.sj['total_cases'])

    iq_mice <- dengueDF.iq %>% dplyr::select(-total_cases) %>% mice()
    dengueDF.iq <- tbl_df(complete(iq_mice)) %>%
        bind_cols(dengueDF.iq['total_cases'])

    dengueDF.sj$reanalysis_sat_precip_amt_mm <- zoo::na.approx(dengueDF.sj$reanalysis_sat_precip_amt_mm)
    dengueDF.iq$reanalysis_sat_precip_amt_mm <- zoo::na.approx(dengueDF.iq$reanalysis_sat_precip_amt_mm)

    map(ls(pattern = 'DF'),  ~ cache(.x))
}