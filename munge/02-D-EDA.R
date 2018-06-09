if(config$model_number == 2) {
    dengueDF.sj.train[,c(-1,-3)] %>%
        cor() %>%
        corrplot::corrplot(
            diag = T,
            type = 'upper',
            order = 'FPC',
            # tl.pos = 'l',
            method = 'sq'
        )

    dengueDF.iq.train[,c(-1,-3)] %>%
        cor() %>%
        corrplot::corrplot(
            diag = T,
            type = 'upper',
            order = 'hclust',
            # tl.pos = 'l',
            method = 'sq'
        )

    # skimr::skim(dengueDF.sj)
    # skimr::skim(dengueDF.iq)
}
