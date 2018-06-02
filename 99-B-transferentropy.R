# Try out a single x-y
computeTE(X = dengueDF.sj$ndvi_ne+rnorm(nrow(dengueDF.sj),mean = 0,sd = 0.1),
          Y = dengueDF.sj$total_cases,
          embedding = 3,
          k = 1,
          safetyCheck = T)[[1]]

# Function to automate calculation for use in purrr::map
calc_TE <- function(x, y, e = 3, k = 1, method = 'MI_diff'){
    sd_x = sd(x)/1000
    x_nrows = length(x)
    x = x + stats::rnorm(n = x_nrows, mean = 0, sd = sd_x)
    computeTE(x,y,e,k,method = method, safetyCheck = T)[[1]]
}

# What's the result for all columns, using correlation method, for SJ?
dengueDF.sj %>%
    select(-year, -week_start_date, -total_cases) %>%
    map_dbl(~calc_TE(.x, dengueDF.sj$total_cases, 4, 3, method = 'Correlation')) %>%
    sort() %>%
    dotplot(main = 'TE for SJ')

# Confirming that if x=total_cases, the expected value of TE should be ~0
dengueDF.sj %>%
    select(-year, -week_start_date, -total_cases) %>%
    map_dbl(~calc_TE(dengueDF.sj$total_cases, .x, 4, 3, method = 'Correlation')) %>%
    sort() %>%
    dotplot(main = 'Corr Coef for SJ')

# What do I get if I just run simple linear correlation?
dengueDF.sj %>%
    select(-year, -week_start_date, -total_cases) %>%
    map_dbl(~cor(.x, dengueDF.sj$total_cases)) %>%
    sort() %>%
    dotplot()

# What's the result for all columns, using correlation method, for IQ?
dengueDF.iq %>%
    select(-year, -week_start_date, -total_cases) %>%
    map_dbl(~calc_TE(.x, dengueDF.iq$total_cases, 4, 3, method = 'Correlation')) %>%
    sort() %>%
    dotplot(main = 'Corr Coef for IQ')

# What happens if I manually insert a lagged series?
calc_TE(dengueDF.sj$reanalysis_sat_precip_amt_mm,dengueDF.sj$total_cases,4,3)
calc_TE(lag(dengueDF.sj$reanalysis_sat_precip_amt_mm,4,default = 0)[-1:-4],dengueDF.sj$total_cases[-1:-4],4,3)

tibble(e=1:20) %>%
    map_dbl(~calc_TE(dengueDF.sj$reanalysis_sat_precip_amt_mm,dengueDF.sj$total_cases,e = .x,k = 3)) %>%
    dotplot()
