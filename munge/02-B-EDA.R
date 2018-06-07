if(config$model_number == 2) {
    dengueTS[complete.cases(dengueTS), ] %>%
        cor() %>%
        corrplot::corrplot(
            order = 'hclust',
            hclust.method = 'ward.D2',
            tl.pos = 'l',
            method = 'sq'
        )

    dengueTS.sj[complete.cases(dengueTS.sj), ] %>%
        cor() %>%
        corrplot::corrplot(
            order = 'hclust',
            hclust.method = 'ward.D2',
            tl.pos = 'l',
            method = 'sq'
        )

    dengueTS.iq[complete.cases(dengueTS.iq), ] %>%
        cor() %>%
        corrplot::corrplot(
            order = 'hclust',
            hclust.method = 'ward.D2',
            tl.pos = 'l',
            method = 'sq'
        )

    skimr::skim(dengueDF.sj)
    skimr::skim(dengueDF.iq)

    #What happens if I scale the variables?



}
