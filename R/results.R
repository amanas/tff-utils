
#' @export
tff.get.accuracy <- function(e, h) {
  expected  <- e[['expected']] %>% fromJSON
  predicted <- e[['predicted']] %>% fromJSON
  accs <-accuracy(predicted[1:h], expected[1:h])
  accs[accs== Inf] <- NA
  accs[accs==-Inf] <- NA
  accs[accs== NaN] <- NA
  accs
}

#' @export
tff.get.accuracies <- function(exps,horizons=1:(4*24*2)) {
  mclapply(1:nrow(exps), function (i) {
    lapply(horizons, function(h) {
      e <- exps[i,]
      tryCatch(list(Device=e[['d']], Metodo=e[['name']], Horizonte=h, tff.get.accuracy(e,h)) %>%
                 as.data.frame(stringsAsFactors=F, row.names = NULL),
               error = function(error) NULL)
    }) %>% rbindlist
  }, mc.cores = tff.estimate.cores()) %>% rbindlist
}


#' @export
tff.get.accuracies.new <- function(exps,horizons=1:(4*24*2)) {
    mclapply(1:nrow(exps), function (i) {
        lapply(horizons, function(h) {
            e <- exps[i,]
            tryCatch(list(Device=e[['Device']], Nombre=e[['Nombre']], Horizonte=h, tff.get.accuracy(e,h)) %>%
                         as.data.frame(stringsAsFactors=F, row.names = NULL),
                     error = function(error) NULL)
        }) %>% rbindlist
    }, mc.cores = tff.estimate.cores()) %>% rbindlist
}

#' @export
tff.update.device.accs <- function(d) {
  cat("\nupdating accs for Device ", d)
  exps <- paste("select d, name, expected, predicted from experiments_new where d = ", d) %>%
    dbGetQuery(tff.get.conn(), .)
  accs <- tff.get.accuracies(exps)
  paste("delete from accuracies where Device = ", d) %>% dbGetQuery(tff.get.conn(), .)
  accs %>% dbWriteTable(tff.get.conn(), value = ., name = "accuracies", append = TRUE, row.names=F)
}

#' @export
tff.update.metodo.accs <- function(m) {
  cat("\nupdating accs for Metodo ", m)
  exps <- paste0("select d, name, expected, predicted from experiments_new where name = '", m,"'") %>%
    dbGetQuery(tff.get.conn(), .)
  max.exp     <- nrow(exps)
  max.horizon <- 4*24*2
  accs <- mclapply(1:max.exp, function (i) {
    lapply(1:max.horizon, function(h) {
      e <- exps[i,]
      tryCatch(list(Device=e[['d']], Metodo=e[['name']], Horizonte=h, tff.get.accuracy(e,h)) %>%
                 as.data.frame(stringsAsFactors=F, row.names = NULL),
               error = function(error) NULL)
    }) %>% rbindlist
  }, mc.cores = tff.estimate.cores()) %>% rbindlist
  paste0("delete from accuracies where Metodo = '", m,"'") %>% dbGetQuery(tff.get.conn(), .)
  accs %>% dbWriteTable(tff.get.conn(), value = ., name = "accuracies", append = TRUE, row.names=F)
}


#' @export
tff.update.all.accs <- function() {
  devices <- "select distinct d from experiments_new order by d" %>% dbGetQuery(tff.get.conn(), .)
  for (d in devices$d)
    tff.update.device.accs(d)
}


#' @export
tff.exps.by.metodo <- function() {
  "select Metodo, count(*) as 'Total experimentos' from accuracies where Horizonte = 1 group by Metodo" %>% dbGetQuery(tff.get.conn(), .)
}

#' #' @export
#' tff.get.or.build.accs <- function(exps) {
#'     if(file.exists("data/accs.RData")) {
#'         load("data/accs.RData")
#'         return(get("accs"))
#'     } else {
#'         stop("Ya debería estar calculado")
#'         max.exp     <- nrow(exps)
#'         max.horizon <- 4*24*2
#'         accs <- mclapply(1:max.exp, function (i) {
#'             lapply(1:max.horizon, function(h) {
#'                 e <- exps[i,]
#'                 tryCatch(list(d=e[['d']], Metodo=e[['name']], h=h, tff.get.accuracy(e,h)) %>%
#'                              as.data.frame(stringsAsFactors=F, row.names = NULL),
#'                          error = function(error) NULL)
#'             }) %>% rbindlist
#'         }, mc.cores = tff.estimate.cores()) %>% rbindlist
#'         save(accs, file = "data/accs.RData")
#'         return(tff.get.or.build.accs(exps))
#'     }
#' }


# tff.rename.exps <- function(exps) {
#     exps$name[exps$name=="tff.stl.d"] <- "STL d"
#     exps$name[exps$name=="tff.stl.d.tail"] <- "STL d (reciente)"
#     exps$name[exps$name=="tff.stl.w"] <- "STL w"
#     exps$name[exps$name=="tff.stl.w.tail"] <- "STL w (reciente)"
#     exps$name[exps$name=="tff.stl.m"] <- "STL m"
#     exps$name[exps$name=="tff.stl.m.tail"] <- "STL m (reciente)"
#     exps$name[exps$name=="tff.stl.y"] <- "STL y"
#     exps$name[exps$name=="tff.stl.y.tail"] <- "STL y (reciente)"
#     exps$name[exps$name=="tff.stlm.dwm"] <- "STLM dwm"
#     exps$name[exps$name=="tff.stlm.dwm.tail"] <- "STLM dwm (reciente)"
#     exps$name[exps$name=="tff.stlm.dwy"] <- "STLM dwy"
#     exps$name[exps$name=="tff.sarima.tail.101.111"] <- "SARIMA"
#     exps$name[exps$name=="tff.lstm"] <- "ANN LSTM"
#     exps
# }

#' @export
tff.rename.accs <- function(accs) {
    accs$Metodo[accs$Metodo=="tff.stl.w"              ] <- "STL W"
    accs$Metodo[accs$Metodo=="tff.stl.w.tail"         ] <- "STL W reciente"
    accs$Metodo[accs$Metodo=="tff.stlm.dw"            ] <- "MSTL D W "
    accs$Metodo[accs$Metodo=="tff.stlm.dw.tail"       ] <- "MSTL D W reciente"
    accs$Metodo[accs$Metodo=="tff.stlm.dwm"           ] <- "MSTL D W M"
    accs$Metodo[accs$Metodo=="tff.stlm.dwm.tail"      ] <- "MSTL D W M reciente"
    accs$Metodo[accs$Metodo=="tff.stlm.dwy"           ] <- "MSTL D W Y"
    accs$Metodo[accs$Metodo=="tff.sarima.tail.101.111"] <- "SARIMA"
    accs$Metodo[accs$Metodo=="tff.lstm"               ] <- "LSTM"
    accs$Metodo[accs$Metodo=="tff.stl.d"              ] <- "STL D"
    accs$Metodo[accs$Metodo=="tff.stl.d.tail"         ] <- "STL D reciente"
    accs$Metodo[accs$Metodo=="tff.stl.m"              ] <- "STL M"
    accs$Metodo[accs$Metodo=="tff.stl.m.tail"         ] <- "STL M reciente"
    accs$Metodo[accs$Metodo=="tff.stl.y"              ] <- "STL Y"
    accs
}

#' @export
tff.prepare.accs <- function(accs.in) {
    accs <- accs.in
    accs<-accs[!(accs$Metodo=="tff.stlm.dw"),]
    accs<-accs[!(accs$Metodo=="tff.stlm.dw.tail"),]
    accs <- tff.rename.accs(accs)
    accs
}

#' @export
tff.to.json <- function(o) jsonlite::toJSON(o, dataframe='columns', null="null", na="null", auto_unbox=T)

#' @export
tff.from.json <- function(o) jsonlite::fromJSON(o)

#' @export
tff.exp.to.xts <- function(exp) {
    vals <- rep(NA, length(exp[['expected']] %>% tff.from.json))
    if(!is.null(exp[['predicted']] %>% tff.from.json))
        vals  <- exp[['predicted']] %>% tff.from.json
    dates <- exp[['fecha.end']] %>% ymd_hms %>% seq(length.out=length(vals), by = '-15 mins') %>% rev
    df    <- data.frame(vals=vals)
    colnames(df) <- c(exp[['name']])
    xts(df, order.by = dates)
}

#' @export
tff.plot.exps <- function(exps) {
    expected <- exps[1,]
    expected$name <- "expected"
    expected$predicted <- expected$expected
    mseries <- cbind(tff.exp.to.xts(expected),
                     if(1<nrow(exps)) apply(exps, 1, tff.exp.to.xts) else tff.exp.to.xts(exps[1,]))
    names(mseries) <- c("expected", exps$name)
    tidy(mseries) %>% ggplot(aes(x=index,y=value, color=series)) + geom_line()
}

# tff.lstm.scale.mean y tff.lstm son lo mismo
#' @export
tff.metodos <- tibble::tribble(~ Familia, ~ Funcion, ~ Nombre, ~ Descripcion,
                       "ARIMA", "tff.auto.arima.tail", "AUTO ARIMA", "Método ARIMA ajustado automáticamente en cada serie",
                       "ARIMA", "tff.sarima.tail.101.111", "SARIMA", "ARIMA estacional",
                       # "LSTM", "tff.lstm", "LSTM Agg4 Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de una hora y escalando con centro y escala la media",
                       "LSTM", "tff.lstm.diff", "LSTM Agg4 Diff", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de una hora y diferenciando",
                       "LSTM", "tff.lstm.diff.scale", "LSTM Agg4 Diff Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de una hora, diferenciando y escalando con centro y escala la media",
                       # "LSTM", "tff.lstm.resample.1.scale.mean", "LSTM Raw Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes y escalando con centro y escala la media",
                       # "LSTM", "tff.lstm.resample.3.scale.mean", "LSTM Agg3 Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de 45 minutos y escalando con centro y escala la media",
                       # "LSTM", "tff.lstm.resample.5.scale.mean", "LSTM Agg5 Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de 75 minutos y escalando con centro y escala la media",
                       "LSTM", "tff.lstm.scale", "LSTM Agg4 Scale SD", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de una hora y escalando con centro la media y escala la desviación típica de la serie",
                       "LSTM", "tff.lstm.scale.mean", "LSTM Agg4 Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, transformando la serie a granularidad de una hora y escalando con centro y escala la media",
                       "LSTM Exógeno", "tff.lstm.exogeno.scale.mean", "LSTM-Exo DH Raw Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, incorporando la hora del día y el día de la semana como variables exógenas y escalando con centro y escala la media",
                       "LSTM Exógeno", "tff.lstm.exogeno.scale.mean.resample.5", "LSTM-Exo DH Agg5 Scale Mean", "LSTM entrenado con los datos de las 8 semanas más recientes, incorporando la hora del día y el día de la semana como variables exógenas, transformando la serie a granularidad de 75 minutos y escalando con centro y escala la media",
                       "MIXTO STL LSTM", "tff.stl.w.tail.lstm.resample.5.identity", "STL+LSTM Agg5 Scale Mean", "Método mixto. Ajuste STL a 6 meses de datos más recientes de la serie considerando estacionalidad semanal. Porteriormente, los residuos de STL se modelan con LSTM  transformando la serie a granularidad de 75 minutos",
                       "MIXTO STL LSTM", "tff.stl.w.tail.lstm.scale.mean", "STL+LSTM Raw Scale Mean", "Método mixto. Ajuste STL a 6 meses de datos más recientes de la serie considerando estacionalidad semanal. Porteriormente, los residuos de STL se modelan con LSTM  transformando la serie a granularidad de 1 hora y escalando con centro y escala la media",
                       # "NAIVE", "tff.naive.total", "Naive", "TODO: eliminar",
                       "STL", "tff.stl.d", "STL D", "STL con estacionalidad diaria ajustado en toda la serie",
                       "STL", "tff.stl.d.tail", "STL D Reciente", "STL con estacionalidad diaria ajustado en los 6 meses de datos previos al punto de pronóstico",
                       "STL", "tff.stl.m", "STL M", "STL con estacionalidad mensual ajustado en toda la serie",
                       "STL", "tff.stl.m.tail", "STL M Reciente", "STL con estacionalidad mensual ajustado en los 6 meses de datos previos al punto de pronóstico",
                       "STL", "tff.stl.w", "STL W", "STL con estacionalidad semanal ajustado en toda la serie",
                       "STL", "tff.stl.w.tail", "STL W Reciente", "STL con estacionalidad semanal ajustado en los 6 meses de datos previos al punto de pronóstico",
                       "STL", "tff.stl.y", "STL Y", "STL con estacionalidad anual ajustado en toda la serie",
                       "STLM", "tff.stlm.dw", "STLM DW", "STLM con estacionalidades diaria y semanal ajustado en toda la serie",
                       "STLM", "tff.stlm.dwm", "STLM DWM", "STLM con estacionalidades diaria, semanal y mensual ajustado en toda la serie",
                       "STLM", "tff.stlm.dwm.tail", "STLM DWM Reciente", "STLM con estacionalidades diaria, semanal y mensual ajustado en los 6 meses de datos previos al punto de pronóstico",
                       # "STLM", "tff.stlm.dw.tail", "STLM DW Reciente", "STLM con estacionalidades diaria y semanal ajustado en los 6 meses de datos previos al punto de pronóstico",
                       "STLM", "tff.stlm.dwy", "STLM DWY", "STLM con estacionalidades diaria, semanal y anual ajustado en toda la serie")



#' @export
tff.best.by.family <- function(exps, horizons, filter.s) {
    accs <- exps %>% filter(str_detect(Nombre, filter.s)) %>%
        group_by(Device) %>% filter(n() == length(unique(.$Nombre))) %>%
        tff.get.accuracies.new(horizons) %>%
        select(-c(Device)) %>% group_by(Nombre,Horizonte) %>% summarise_all(mean, na.rm=T)

    errs <- cbind(accs %>% select(Nombre, Horizonte, RMSE) %>%  spread(Horizonte,RMSE) %>% arrange(Nombre),
                  accs %>% select(Nombre, Horizonte, MAPE) %>%  spread(Horizonte,MAPE) %>% arrange(Nombre))[,-c(7)] %>%
        arrange(`192`)
    colnames(errs) <- c("Nombre",rep(paste(horizons/4,"horas"),2))
    errs
}

#' @export
tff.best.by.name <- function(exps, horizons, Nombres) {
    accs <- exps %>% filter(Nombre %in% Nombres) %>%
        group_by(Device) %>% filter(n() == length(unique(.$Nombre))) %>%
        tff.get.accuracies.new(horizons) %>%
        select(-c(Device)) %>% group_by(Nombre,Horizonte) %>% summarise_all(mean, na.rm=T)

    errs <- cbind(accs %>% select(Nombre, Horizonte, RMSE) %>%  spread(Horizonte,RMSE) %>% arrange(Nombre),
                  accs %>% select(Nombre, Horizonte, MAPE) %>%  spread(Horizonte,MAPE) %>% arrange(Nombre))[,-c(7)] %>%
        arrange(`192`)
    colnames(errs) <- c("Nombre",rep(paste(horizons/4,"horas"),2))
    errs
}

#' @export
tff.get.accs.top <- function(exps, Nombres) {
    fname <- "data/accs.top.RData"
    if(file.exists(fname)){
        load(fname)
        accs.top
    } else {
        accs.top <- exps %>% filter(Nombre %in% Nombres) %>%
            group_by(Device) %>% filter(n() == length(unique(.$Nombre))) %>%
            tff.get.accuracies.new
        save(accs.top, file = fname)
        get.accs.top()
    }
}
