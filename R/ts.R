
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.clean <- function(df) {
    tmp <- df
    tmp$tipo_elem <- NULL
    tmp$error <- ifelse(tmp$error == "N", 0, 1)
    tmp[tmp$error == 1, c('intensidad','ocupacion','carga','vmed','periodo_integracion')] <- NA
    tmp
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.pad <- function(df) {
    df %>% pad
}

#' Title
#'
#' @param ts
#'
#' @return
#' @export
#'
#' @examples
tff.na.interp <- function(ts) {
    imputeTS::na.interpolation(ts)
}

#' Title
#'
#' @param df
#' @param date.col
#'
#' @return
#' @export
#'
#' @examples
tff.as.xts <- function(df, date.col="fecha") {
    tmp <- df
    rownames(tmp) <- tmp[[date.col]]
    tmp[[date.col]] <- NULL
    xts(tmp, order.by = as.POSIXct(rownames(tmp)))
}

#' Title
#'
#' @param df
#' @param freq
#' @param vals.col
#'
#' @return
#' @export
#'
#' @examples
tff.as.ts <- function(df, freq, vals.col="carga") {
    ts(df[[vals.col]], frequency = freq)
}

#' Title
#'
#' @param df
#' @param seasonal.periods
#' @param vals.col
#'
#' @return
#' @export
#'
#' @examples
tff.as.msts <- function(df, seasonal.periods, vals.col="carga") {
    msts(df[[vals.col]], seasonal.periods = seasonal.periods)
}

#' Title
#'
#' @param df
#' @param test.size
#'
#' @return
#' @export
#'
#' @examples
tff.tts <- function(df, test.size=10) {
    l <- nrow(df)
    list(train = df[1:(l-test.size),],
         test = df[(l - test.size + 1):l,])
}

#' Title
#'
#' @param df
#' @param model.f
#' @param test.size
#'
#' @return
#' @export
#'
#' @examples
tff.fit <- function(df, model.f, test.size=10){
    tts <- tff.tts(df, test.size=test.size)
    set.seed(0)
    model <- model.f(tts$train)
    list(tts = tts,
         model = model,
         forecast = forecast(model, h=nrow(tts$test)))
}

#' Title
#'
#' @param fit
#' @param vals.col
#'
#' @return
#' @export
#'
#' @examples
tff.accuracy.fit <- function(fit, vals.col="carga") {
    fit$forecast %>% accuracy(fit$tts$test[[vals.col]])
}

#' Title
#'
#' @param fit
#' @param include
#' @param vals.col
#' @param date.col
#'
#' @return
#' @export
#'
#' @examples
tff.plot.fit <- function(fit, include = nrow(fit$tts$test) * 5, vals.col="carga", date.col="fecha") {
    forecast.ts <- as.ts(fit$forecast)
    real.layer <- ts(fit$tts$test[[vals.col]], end = end(forecast.ts), frequency = frequency(forecast.ts))
    autoplot(fit$forecast, include = include) +
        autolayer(real.layer, series = 'real')

    pred.ts <- as.ts(fit$forecast)
    freq <- frequency(pred.ts)
    real.ts <- ts(fit$tts$test[[vals.col]], end = end(pred.ts), frequency = freq)

    x.max.val <- end(pred.ts)[1] + end(pred.ts)[2]/freq
    x.min.val <- x.max.val - (nrow(pred.ts) + 1 + include)/freq
    x.med.val <- (x.min.val + x.max.val)/2

    tmp.df <- fit$tts$train %>% tail(include) %>% rbind(fit$tts$test)
    x.min.lab <- format(tmp.df[[date.col]][1], "%b %d %H:%M")
    x.max.lab <- format(tmp.df[[date.col]][nrow(tmp.df)], "%b %d %H:%M")
    x.med.lab <- format(tmp.df[[date.col]][nrow(tmp.df)/2], "%b %d %H:%M")

    autoplot(fit$forecast, include = include) +
        autolayer(real.ts, series = 'real') +
        scale_x_continuous(breaks=c(x.min.val, x.med.val, x.max.val),
                           labels=c(x.min.lab, x.med.lab, x.max.lab)) +
        labs(x=date.col, y=vals.col)
}

#' Title
#'
#' @param fits
#' @param vals.col
#' @param date.col
#'
#' @return
#' @export
#'
#' @examples
tff.plot.fits <- function(fits, vals.col="carga", date.col="fecha") {
    fits %>% lapply(tff.plot.fit, vals.col=vals.col, date.col=date.col) %>% grid.arrange(grobs=., ncol=2)
}

#' @export
tff.hourly.mean <- function(df) {
    d <- df %>% tff.as.xts %>% period.apply(endpoints(., "hours"), mean, na.rm = T) %>% as.data.frame
    names <- as.POSIXct(rownames(d))
    rownames(d) <- NULL
    cbind(names,d)
}
