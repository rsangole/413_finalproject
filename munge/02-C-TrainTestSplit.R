if(config$model_number == 2) {

    toTrain <- 1:(dim(dengueDF.sj)[1]*.7)
    dengueDF.sj.train <- dengueDF.sj[toTrain,]
    dengueDF.sj.test <- dengueDF.sj[-toTrain,]

    toTrain <- 1:(dim(dengueDF.iq)[1]*.7)
    dengueDF.iq.train <- dengueDF.iq[toTrain,]
    dengueDF.iq.test <- dengueDF.iq[-toTrain,]

    # preProcValues.sj <- preProcess(dengueDF.sj[,c(-1,-2,-3,-24)], method = c("center", "scale"), )
    # preProcValues.iq <- preProcess(dengueDF.iq[,c(-1,-2,-3,-24)], method = c("center", "scale"))

    preProcValues.sj <- preProcess(dengueDF.sj[,c(-1,-2,-3,-24)], method = c('range', 'corr'))
    preProcValues.iq <- preProcess(dengueDF.iq[,c(-1,-2,-3,-24)], method = c('range','corr'))

    dengueDF.sj.train <- predict(preProcValues.sj, dengueDF.sj.train)
    dengueDF.sj.test <- predict(preProcValues.sj, dengueDF.sj.test)

    dengueDF.iq.train <- predict(preProcValues.sj, dengueDF.iq.train)
    dengueDF.iq.test <- predict(preProcValues.sj, dengueDF.iq.test)

    map(ls(pattern = 'DF'),  ~ cache(.x))
}