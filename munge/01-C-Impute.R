if(config$model_number == 1) {
    dengueDF.sj[, c(-1, -2, -3)] <-
        map_df(dengueDF.sj[, c(-1, -2, -3)], ~ zoo::na.approx(.x))
    dengueDF.iq[, c(-1, -2, -3)] <-
        map_df(dengueDF.iq[, c(-1, -2, -3)], ~ zoo::na.approx(.x))
    dengueTS.sj <- zoo::na.approx(dengueTS.sj)
    dengueTS.iq <- zoo::na.approx(dengueTS.iq)


    map(ls(pattern = 'DF'),  ~ cache(.x))
    map(ls(pattern = 'TS'),  ~ cache(.x))
}