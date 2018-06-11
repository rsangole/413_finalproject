# data <- data.matrix(data[,-1])
data <- dengueDF.sj %>% arrange(week_start_date) %>% select(-year,-week_start_date)
train_data <- data[(1:nrow(data)*0.7),]
mean <- apply(train_data, 2, mean)
std <- apply(train_data, 2, sd)
data <- scale(data, center = mean, scale = std)

generator <- function(data, lookback, delay, min_index, max_index,
                      shuffle = FALSE, batch_size = 128, step = 1) {
    if (is.null(max_index))
        max_index <- nrow(data) - delay - 1
    i <- min_index + lookback
    function() {
        if (shuffle) {
            rows <- sample(c((min_index+lookback):max_index), size = batch_size)
        } else {
            if (i + batch_size >= max_index)
                i <<- min_index + lookback
            rows <- c(i:min(i+batch_size-1, max_index))
            i <<- i + length(rows)
        }

        samples <- array(0, dim = c(length(rows),
                                    lookback / step,
                                    (dim(data)[[-1]]-1)))
        targets <- array(0, dim = c(length(rows)))

        for (j in 1:length(rows)) {
            indices <- seq(rows[[j]] - lookback, rows[[j]]-1,
                           length.out = dim(samples)[[2]])
            samples[j,,] <- data[indices, -22]
            targets[[j]] <- data[rows[[j]] + delay,22]
        }

        list(samples, targets)
    }
}

lookback <- 10
step <- 1
delay <- 1
batch_size <- 1

train_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 1,
    max_index = 550,
    shuffle = FALSE,
    step = step,
    batch_size = batch_size
)
# head(data); train_gen()

val_gen = generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 551,
    max_index = 700,
    step = step,
    batch_size = batch_size
)

test_gen <- generator(
    data,
    lookback = lookback,
    delay = delay,
    min_index = 701,
    max_index = NULL,
    step = step,
    batch_size = batch_size
)

# How many steps to draw from val_gen in order to see the entire validation set
val_steps <- (701 - 551 - lookback) / batch_size
val_steps
# How many steps to draw from test_gen in order to see the entire test set
test_steps <- (nrow(data) - 701 - lookback) / batch_size
test_steps

model <- keras_model_sequential() %>%
    layer_gru(units = 10,
              input_shape = list(NULL, (dim(data)[[-1]]-1)),
              dropout = 0.1,
              recurrent_dropout = 0.1) %>%
    # layer_gru(units = 64, activation = 'relu', dropout = 0.1,
    #           recurrent_dropout = 0.5) %>%
    layer_dense(units = 1)

model %>% compile(
    optimizer = optimizer_rmsprop(),
    loss = "mae"
)

# tensorboard("logs/run_c", reload_interval = 10)
history <- model %>% fit_generator(
    train_gen,
    steps_per_epoch = 200,
    epochs = 20,
    validation_data = val_gen,
    validation_steps = val_steps
    # callbacks = callback_tensorboard("logs/run_c")
)
# plot(history)
