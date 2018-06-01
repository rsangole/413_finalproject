# What's missing

map_int(as_tibble(dengueTS), ~sum(is.na(.x)))

# Saving plots for all time series
# for (i in 3:ncol(dengueTS)) {
#     ts1 = dengueTS[dengueTS$city==1,i]
#     ts2 = dengueTS[dengueTS$city==0,i]
#     p1 = xyplot.ts(ts1, main = colnames(ts1), sub = 'City SJ')
#     p2 = xyplot.ts(ts2, sub = 'City IQ')
#     png(filename = paste0('graphs/',colnames(ts1),'.png'),width = 900, height = 500)
#     grid.arrange(p1, p2)
#     dev.off()
# }


# How do the correlations look?
dengueTS[complete.cases(dengueTS),] %>%
    cor() %>%
    corrplot::corrplot(type = 'lower',order = 'hclust')

densityplot(~reanalysis_relative_humidity_percent, groups = city, dengueDF, plot.points = T, cex=0.2, auto.key = T)
densityplot(~reanalysis_min_air_temp_k, groups = city, dengueDF, plot.points = T, cex=0.2, auto.key = T)

