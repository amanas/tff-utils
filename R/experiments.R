

#' @export
tff.random.generator <- function(data, batch.size, time.steps, look.ahead, features, reset.steps,
                             dim.X=c(batch.size,time.steps,features), dim.y=c(batch.size,look.ahead)) {
    data <- data
    call <- 0
    idx  <- c()
    function() {
        if(call%%reset.steps==0)
            idx <<- sample(length(data) - look.ahead - time.steps - reset.steps, batch.size)
        else
            idx <<- idx + 1
        call <<- call + 1
        X <- lapply(idx, function(i) data[i:(i + time.steps - 1)]) %>%
            unlist %>% matrix(nrow=batch.size, byrow=T) %>% array(dim=dim.X)
        y <- lapply(idx, function(i) data[(i + time.steps):(i + time.steps + look.ahead - 1)]) %>%
            unlist %>% matrix(nrow=batch.size, byrow=T) %>% array(dim=dim.y)
        list(X, y)
    }
}


#' @export
tff.early.stop <- function(history, patience, tolerance) {
    if (length(history$metrics$val_loss) < patience + 1) {
        FALSE
    } else {
        head.losses <- history$metrics$val_loss %>% head(length(history$metrics$val_loss) - patience)
        tail.losses <- history$metrics$val_loss %>% tail(patience)
        min(head.losses) < min(tail.losses)*(1+tolerance)
    }
}


#' @export
tff.combine.histories <- function(h1, h2) {
    if(length(h1)==0) {
        h2
    } else {
        h1$params$epochs <-h1$params$epochs + h2$params$epochs
        h1$metrics$loss <- c(h1$metrics$loss, h2$metrics$loss)
        h1$metrics$val_loss <- c(h1$metrics$val_loss, h2$metrics$val_loss)
        h1
    }
}


#' @export
tff.unscale <- function(data, scaled.data, src.data) data * attr(scaled.data, 'scaled:scale') + attr(scaled.data, 'scaled:center')

#' @export
tff.undiff <- function(data, diff.data, src.data) diffinv(data, xi=src.data$carga %>% tail(1)) %>% head(.,length(.)-1)

#' @export
tff.run.with.timeout <- function(f,..., timeout = 60) {
    myfork <- parallel::mcparallel(f(...), silent = FALSE)
    myresult <- parallel::mccollect(myfork, wait = FALSE, timeout = timeout)
    tools::pskill(myfork$pid, tools::SIGKILL)
    tools::pskill(-1 * myfork$pid, tools::SIGKILL)
    parallel::mccollect(myfork, wait = FALSE)
    if (is.null(myresult))
        stop("reached elapsed time limit")
    myresult[[1]]
}

#' @export
tff.run.experiment <- function(d, model.f, exp.name, timeout, data=NULL) {

    cat('\n', 'running', d, exp.name, '\n')

    if(is.null(data)) {
        data <- tff.get.parsed.metrics(d=d) %>% tff.clean %>% tff.pad %>% na.interpolation %>% .[, c("fecha","carga")]
    }

    exp.look.ahead <- 4*24*2
    set.seed(d)
    exp.point.end  <- sample(as.integer(nrow(data)*3/4):nrow(data), 1)
    exp.data       <- head(data, exp.point.end)
    exp.fecha.end  <- exp.data$fecha[exp.point.end]
    exp.tts        <- tff.tts(exp.data, test.size=exp.look.ahead)
    exp.expected   <- exp.tts$test$carga
    exp.fit        <- NA
    exp.time       <- NA
    exp.error      <- ""
    exp.predicted  <- NA
    exp.acc        <- list(ME=NA, RMSE=NA, MAE=NA, MPE=NA, MAPE=NA)

    if (exp.point.end < 0 || nrow(exp.tts$train) < (20000 + exp.look.ahead)) {
        exp.fecha.end <- NA
        exp.error     <- "less than 20000 + exp.look.ahead data points to train"
    } else if(mean(exp.tts$train$carga) < 0.01) {
        exp.error     <- "carga mean is < 0.01"
    } else {
        exp.start    <- Sys.time()
        exp.fit      <- tryCatch(tff.run.with.timeout(model.f, exp.tts$train, exp.look.ahead, timeout = timeout), error = toString)
        exp.time     <- as.numeric(Sys.time() - exp.start, units="secs")
    }

    if(is.list(exp.fit)) {
        exp.predicted <- exp.fit$predicted
        exp.acc       <- accuracy(exp.tts$test$carga, exp.fit$predicted)[1,]
    } else if(is.character(exp.fit)) {
        exp.error     <- exp.fit
    }

    list(d=d, name=exp.name, point.end=exp.point.end, fecha.end=exp.fecha.end,
         look.ahead=exp.look.ahead, time=exp.time, error=exp.error) %>%
        append(exp.acc) %>%
        append(list(expected=(exp.expected %>% tff.to.json %>% toString),
                    predicted=(exp.predicted %>% tff.to.json %>% toString),
                    model.f=(model.f %>% deparse %>% tff.to.json %>% toString)))
}


#' @export
tff.lstm <- function(data, look.ahead, f.scale, f.unscale) {
    resample    <- 4
    N           <- 4*24*7*4*2/resample
    look.ahead  <- look.ahead/resample
    time.steps  <- 4*24*2/resample
    valid.steps <- 4*24/resample
    features    <- 1
    batch.size  <- 64
    train.steps <- 4*24*7/resample
    epochs      <- 100
    patience    <- 3
    tolerance   <- 0.1

    data.agg <- data %>% tff.as.ts(resample) %>% aggregate(FUN=mean, na.rm=T) %>% tail(N) %>% f.scale
    data.gen <- tff.random.generator(data.agg, batch.size, time.steps, look.ahead, features, train.steps + valid.steps)
    model <- keras_model_sequential() %>%
        layer_lstm(input_shape=c(time.steps,features), batch_size=batch.size,
                   units=50, stateful=TRUE, return_sequences=TRUE) %>%
        layer_lstm(units=50, stateful=TRUE, return_sequences=FALSE) %>%
        layer_dense(units = look.ahead) %>%
        compile(loss = 'mae', optimizer = 'adam')
    history <- list()
    epoch   <- 1
    while (epoch <= epochs && !tff.early.stop(history, patience, tolerance)) {
        # cat(epoch, "- ")
        h <- fit_generator(model, data.gen, validation_data = data.gen,
                           steps_per_epoch = train.steps, validation_steps = valid.steps,
                           epochs = 1, verbose = 0)
        model %>% reset_states()
        history <- tff.combine.histories(history, h)
        epoch <- epoch + 1
    }
    predicted <- data.agg %>% tail(time.steps) %>% rep(batch.size) %>%
        matrix(nrow=batch.size, byrow=T) %>%
        array(dim=c(batch.size, time.steps, features)) %>%
        predict(model, ., batch_size = batch.size) %>% .[1,] %>%
        f.unscale(data.agg, data) %>%
        lapply(function(v) c(v, rep(NA, resample-1))) %>% unlist %>% na.interpolation

    list(model=model, history=history, predicted=predicted)
}


#'
#' Utiliza el resample correctamente en la predicción.
#' En los otros experimentos LSTM con resample, la predicción sale movida.
#'
#'  @param data
#'
#' @param look.ahead.in
#' @param f.scale
#' @param f.unscale
#' @param resample
#'
#' @export
tff.lstm.resample.odd <- function(data, look.ahead.in, f.scale, f.unscale, resample) {
  if(resample %% 2 == 0)
    stop("Resample not odd")
  N           <- resample*24*7*4*2/resample
  look.ahead  <- (look.ahead.in + resample)/resample
  time.steps  <- resample*24*2/resample
  valid.steps <- resample*24/resample
  features    <- 1
  batch.size  <- 64
  train.steps <- resample*24*7/resample
  epochs      <- 100
  patience    <- 3
  tolerance   <- 0.1

  data.agg <- data %>% tff.as.ts(resample) %>% aggregate(FUN=mean, na.rm=T) %>% tail(N) %>% f.scale
  data.gen <- tff.random.generator(data.agg, batch.size, time.steps, look.ahead, features, train.steps + valid.steps)
  model <- keras_model_sequential() %>%
    layer_lstm(input_shape=c(time.steps,features), batch_size=batch.size,
               units=50, stateful=TRUE, return_sequences=TRUE) %>%
    layer_lstm(units=50, stateful=TRUE, return_sequences=FALSE) %>%
    layer_dense(units = look.ahead) %>%
    compile(loss = 'mae', optimizer = 'adam')
  history <- list()
  epoch   <- 1
  while (epoch <= epochs && !tff.early.stop(history, patience, tolerance)) {
    # cat(epoch, "- ")
    h <- fit_generator(model, data.gen, validation_data = data.gen,
                       steps_per_epoch = train.steps, validation_steps = valid.steps,
                       epochs = 1, verbose = 0)
    model %>% reset_states()
    history <- tff.combine.histories(history, h)
    epoch <- epoch + 1
  }
  predicted <- data.agg %>% tail(time.steps) %>% rep(batch.size) %>%
    matrix(nrow=batch.size, byrow=T) %>%
    array(dim=c(batch.size, time.steps, features)) %>%
    predict(model, ., batch_size = batch.size) %>% .[1,] %>%
    f.unscale(data.agg, data) %>%
    lapply(function(v) c(rep(NA,resample/2), v, rep(NA,resample/2))) %>%
    unlist %>% na.interpolation %>% head(look.ahead.in)

  list(model=model, history=history, predicted=predicted)
}

#' @export
tff.stl.d <- function(data, look.ahead) {
    model     <- data %>% tff.as.ts(4*24) %>% stl(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stl.d.tail <- function(data, look.ahead) {
    tff.stl.d(data %>% tail(20000), look.ahead)
}

#' @export
tff.stl.w <- function(data, look.ahead) {
    model     <- data %>% tff.as.ts(4*24*7) %>% stl(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stl.w.tail <- function(data, look.ahead) {
    tff.stl.w(data %>% tail(20000), look.ahead)
}

#' @export
tff.stl.m <- function(data, look.ahead) {
    model     <- data %>% tff.as.ts(4*24*365.25/12) %>% stl(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stl.m.tail <- function(data, look.ahead) {
    tff.stl.m(data %>% tail(20000), look.ahead)
}

#' @export
tff.stl.y <- function(data, look.ahead) {
    model     <- data %>% tff.as.ts(4*24*365.25) %>% stl(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stl.y.tail <- function(data, look.ahead) {
    tff.stl.y(data %>% tail(20000), look.ahead)
}


#' @export
tff.stlm.dw <- function(data, look.ahead) {
    model     <- data %>% tff.as.msts(c(4*24, 4*24*7)) %>% stlm(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stlm.dw.tail <- function(data, look.ahead) {
    tff.stlm.dw(data %>% tail(20000), look.ahead)
}

tff.stlm.dwm <- function(data, look.ahead) {
    model     <- data %>% tff.as.msts(c(4*24, 4*24*7, 4*24*365.25/12)) %>% stlm(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.stlm.dwm.tail <- function(data, look.ahead) {
    tff.stlm.dwm(data %>% tail(60000), look.ahead)
}

#' @export
tff.stlm.dwy <- function(data, look.ahead) {
    model     <- data %>% tff.as.msts(c(4*24, 4*24*7, 4*24*365.25)) %>% stlm(s.window = "periodic")
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}


#' @export
tff.sarima.tail.101.111 <- function(data, look.ahead) {
    model     <- data %>% tail(4000) %>% tff.as.ts(4*24) %>%
        Arima(method = "CSS", order = c(1,0,1), seasonal = list(order = c(1,1,1), period = 4*24))
    predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
    list(model=model, history=NA, predicted=predicted)
}


#' @export
tff.auto.arima.tail <- function(data, look.ahead) {
  model <- data$carga %>% tail(4000) %>% ts %>% auto.arima(stepwise=FALSE, approximation=FALSE)
  predicted <- model %>% forecast(h=look.ahead) %>% .$mean %>% as.vector
  list(model=model, history=NA, predicted=predicted)
}

#' @export
tff.lstm.identity <- function(data, look.ahead) {
  tff.lstm.resample.odd(data, look.ahead, function(x) x, function(x,y,z) x, 1)
}

#' @export
tff.lstm.scale <- function(data, look.ahead) {
    tff.lstm(data, look.ahead, scale, tff.unscale)
}

#' @export
tff.lstm.scale.mean <- function(data, look.ahead) {
    tff.lstm(data, look.ahead, function(dat.2) dat.2 %>% scale(mean(.), mean(.)), tff.unscale)
}

#' @export
tff.lstm.diff <- function(data, look.ahead) {
    tff.lstm(data, look.ahead, diff, tff.undiff)
}

#' @export
tff.lstm.diff.scale <- function(data, look.ahead) {
    tff.lstm(data, look.ahead,
             function(dat.2)
                 dat.2 %>% diff %>% scale,
             function(dat.2, scaled.data, src.data)
                 tff.undiff(tff.unscale(dat.2, scaled.data, NULL),NULL,src.data))
}

#' @export
tff.lstm.resample.5.scale.mean <- function(data, look.ahead) {
  tff.lstm.resample.odd(data, look.ahead, function(dat.2) dat.2 %>% scale(mean(.), mean(.)), tff.unscale, 5)
}

#' @export
tff.lstm.resample.3.scale.mean <- function(data, look.ahead) {
  tff.lstm.resample.odd(data, look.ahead, function(dat.2) dat.2 %>% scale(mean(.), mean(.)), tff.unscale, 3)
}

#' @export
tff.lstm.resample.1.scale.mean <- function(data, look.ahead) {
  tff.lstm.resample.odd(data, look.ahead, function(dat.2) dat.2 %>% scale(mean(.), mean(.)), tff.unscale, 1)
}


#' @export
tff.stl.w.tail.lstm.scale.mean <- function(data, look.ahead) {
  model.stl      <- tff.stl.w.tail(data, look.ahead)
  data.remainder <- data.frame(carga=model.stl$model$time.series[,"remainder"])
  model.lstm     <- tff.lstm.scale.mean(data.remainder, look.ahead)
  list(model=list(stl=model.stl, lstm=model.lstm), history=NA, predicted=model.stl$predicted+model.lstm$predicted)
}


#' @export
tff.stl.w.tail.40000 <- function(data, look.ahead) {
  tff.stl.w(data %>% tail(40000), look.ahead)
}

#' @export
tff.lstm.resample.5.identity <- function(data, look.ahead) {
  tff.lstm.resample.odd(data, look.ahead, function(x) x, function(x,y,z) x, 5)
}

#' @export
tff.stl.w.tail.lstm.resample.5.identity <- function(data, look.ahead) {
  model.stl      <- tff.stl.w.tail(data, look.ahead)
  data.remainder <- data.frame(carga=model.stl$model$time.series[,"remainder"])
  model.lstm     <- tff.lstm.resample.5.identity(data.remainder, look.ahead)
  list(model=list(stl=model.stl, lstm=model.lstm), history=NA, predicted=model.stl$predicted+model.lstm$predicted)
}


#' @export
tff.stl.w.tail.40000.lstm.resample.5.identity <- function(data, look.ahead) {
  model.stl      <- tff.stl.w.tail.40000(data, look.ahead)
  data.remainder <- data.frame(carga=model.stl$model$time.series[,"remainder"])
  model.lstm     <- tff.lstm.resample.5.identity(data.remainder, look.ahead)
  list(model=list(stl=model.stl, lstm=model.lstm), history=NA, predicted=model.stl$predicted+model.lstm$predicted)
}

#' @export
gen.exogeno <- function(series, batch.size, time.steps, look.ahead, features, steps.per.epoch, validation.steps) {

  if(nrow(series) < time.steps + look.ahead + steps.per.epoch + validation.steps)
    stop("not enough data for batching")

  call  <- 1
  dim.X <- c(batch.size,time.steps,features)
  dim.y <- c(batch.size,look.ahead)
  idx   <- sample(nrow(series) - time.steps - look.ahead - steps.per.epoch - validation.steps, batch.size, replace=T)

  function() {
    # cat(ifelse(call<=steps.per.epoch, "+","-"))
    X <- lapply(idx, function(i) series[i:(i + time.steps - 1),]) %>%
      unlist %>% matrix(nrow=batch.size, byrow=T) %>% array(dim=dim.X)
    # Asume que la variable objetvo es la primera columna
    y <- lapply(idx, function(i) series[(i + time.steps):(i + time.steps + look.ahead - 1),1]) %>%
      unlist %>% matrix(nrow=batch.size, byrow=T) %>% array(dim=dim.y)
    call <<- call + 1
    idx  <<- idx + 1
    list(X, y)
  }
}

#' #' @export
#' tff.reset.tf <- function() {
#'   require(tensorflow)
#'   k_clear_session()
#'   config <- tf$ConfigProto(intra_op_parallelism_threads = as.integer(tff.estimate.cores()),
#'                            inter_op_parallelism_threads = as.integer(tff.estimate.cores()))
#'   session = tf$Session(config = config)
#'   k_set_session(session)
#' }

#' @export
lstm.exogeno <- function(series, look.ahead,
                         time.steps=4*24*2, steps.per.epoch=4*24*2, validation.steps=10,
                         batch.size=64, epochs=100, patience=3, tolerance=0.1) {
  features  <- ncol(series)
  epoch     <- 1
  history   <- list()

  # tff.reset.tf()
  model <- keras_model_sequential() %>%
    layer_lstm(input_shape=c(time.steps,features), batch_size=batch.size,
               units=50, stateful=TRUE, return_sequences=TRUE) %>%
    layer_lstm(units=50, stateful=TRUE, return_sequences=FALSE) %>%
    layer_dense(units = look.ahead) %>%
    compile(loss = 'mae', optimizer = 'adam')


  while (epoch <= epochs && !tff.early.stop(history, patience, tolerance)) {
    cat("Epoch", epoch,": ")
    gen <- gen.exogeno(series, batch.size, time.steps, look.ahead, features, steps.per.epoch, validation.steps)
    # TODO: lo mismo sirve con un unico generator para modelos multientrada
    # TODO: revisar https://github.com/keras-team/keras/issues/3177
    h <- fit_generator(model, gen,
                       validation_data  = gen,
                       steps_per_epoch  = steps.per.epoch,
                       validation_steps = validation.steps,
                       epochs = 1, verbose = 1)
    history <- tff.combine.histories(history, h)
    model %>% reset_states()
    epoch <- epoch + 1
  }

  dim.X <- c(batch.size,time.steps,features)
  X <- lapply(1:batch.size, function(i) tail(series, time.steps)) %>%
    unlist %>% matrix(nrow=batch.size, byrow=T) %>%
    array(dim=dim.X)
  predicted <- model %>% predict(X, batch_size = batch.size) %>% .[1,]

  list(model=model, history=history, predicted=predicted)
}


#' @export
tff.scale.mean   <- function(series) series %>% scale(colMeans(.), colMeans(.))

#' @export
tff.unscale.mean <- function(scaled,unscaled) {
  series.scale <- unscaled %>% tff.scale.mean
  scale_ <- attr(series.scale, 'scaled:scale')
  center_ <- attr(series.scale, 'scaled:center')
  for(i in 1:length(scale_))
    scaled[,i] <- scaled[,i]*scale_[i] + center_[i]
  scaled
}

#' @export
tff.lstm.exogeno.scale.mean <- function(data, look.ahead) {
  N        <- 4*24*7*8 # 8 semanas
  data     <- data %>% tail(N) %>% mutate(wday=sapply(fecha, wday)) %>% mutate(hour=hour(fecha)) %>%
    as.data.table %>% as.xts
  lstm.fit <- lstm.exogeno(data %>% tff.scale.mean, look.ahead,
                           time.steps=4*24, steps.per.epoch=4*24, validation.steps=10, tolerance=0.05)
  lstm.fit$predicted <- lstm.fit$predicted %>% data.frame %>% tff.unscale.mean(data$carga) %>% .[,1]
  lstm.fit
}


#' @export
tff.lstm.exogeno.scale.mean.resample.5 <- function(data, look.ahead) {
  data <- data %>% period.apply(seq(1,nrow(data),5) , mean)
  data <- data.frame(fecha=index(data), carga=data$carga)
  lstm.fit <- tff.lstm.exogeno.scale.mean(data, (look.ahead+5)/5)
  lstm.fit$predicted <- lstm.fit$predicted %>%
    lapply(function(v) c(NA,NA,v,NA,NA)) %>% unlist %>% na.interpolation %>% head(look.ahead)
  lstm.fit
}


#' @export
tff.naive.total <- function(data, look.ahead) {
  predicted <- data$carga %>% tail(4*24*7) %>% head(look.ahead)
  list(model="lo que paso la semana anterior", history=NA, predicted=predicted)
}


#' @export
tff.already.done <- function(t, d, n) {
    "select count(1) from %s where d=%d and name='%s'" %>%
        sprintf(t, d, n) %>% dbGetQuery(tff.get.conn(),.) %>% .[1, 1] > 0
}

#' @export
tff.already.done.all <- function(t, d) {
    # "select count(distinct name) from %s where d=%d" %>%
    #     sprintf(t, d) %>% dbGetQuery(tff.get.conn(),.) %>% .[1, 1] == 17
  FALSE
}

#' @export
tff.save.exp.new <- function(exp) {
    dbWriteTable(tff.get.conn(), name="experiments_new", value=as.data.frame(exp), append=T, row.names=F)
}

#' @export
tff.run.experiment.subset <- function(i) {
    # devices <- "select distinct device from metrics order by device" %>% dbGetQuery(tff.get.conn(), .) %>% .$device
    devices <- "select distinct(Device) from devices_sd order by sd desc" %>% dbGetQuery(tff.get.conn(),.)  %>% .$Device
    for(d in devices) {
        if(d%%2==i) {
            tryCatch({
                if(!tff.already.done.all("experiments_new", d)) {
                    cat('\n', 'running', d, '\n')

                    tff.release.pool()

                    data <- tff.get.parsed.metrics(d=d) %>% tff.clean %>% tff.pad %>% na.interpolation %>% .[, c("fecha","carga")]

                    # if(!tff.already.done("experiments_new", d, "tff.stl.d"))
                    #     tff.run.experiment(d, tff.stl.d, "tff.stl.d", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.d.tail"))
                    #     tff.run.experiment(d, tff.stl.d.tail, "tff.stl.d.tail", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.w"))
                    #     tff.run.experiment(d, tff.stl.w, "tff.stl.w", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.w.tail"))
                    #     tff.run.experiment(d, tff.stl.w.tail, "tff.stl.w.tail", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.m"))
                    #     tff.run.experiment(d, tff.stl.m, "tff.stl.m", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.m.tail"))
                    #     tff.run.experiment(d, tff.stl.m.tail, "tff.stl.m.tail", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.y"))
                    #     tff.run.experiment(d, tff.stl.y, "tff.stl.y", 60, data=data) %>% tff.save.exp.new
                    #
                    # if(!tff.already.done("experiments_new", d, "tff.stlm.dw"))
                    #     tff.run.experiment(d, tff.stlm.dw, "tff.stlm.dw", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stlm.dw.tail"))
                    #     tff.run.experiment(d, tff.stlm.dw.tail, "tff.stlm.dw.tail", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stlm.dwm"))
                    #     tff.run.experiment(d, tff.stlm.dwm, "tff.stlm.dwm", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stlm.dwm.tail"))
                    #     tff.run.experiment(d, tff.stlm.dwm.tail, "tff.stlm.dwm.tail", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stlm.dwy"))
                    #     tff.run.experiment(d, tff.stlm.dwy, "tff.stlm.dwy", 60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.sarima.tail.101.111"))
                    #     tff.run.experiment(d, tff.sarima.tail.101.111, "tff.sarima.tail.101.111", 60, data=data) %>% tff.save.exp.new
                    #
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.scale"))
                    #     tff.run.experiment(d, tff.lstm.scale, "tff.lstm.scale", 10*60, data=data)  %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.scale.mean"))
                    #     tff.run.experiment(d, tff.lstm.scale.mean, "tff.lstm.scale.mean", 10*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.diff"))
                    #     tff.run.experiment(d, tff.lstm.diff, "tff.lstm.diff", 10*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.diff.scale"))
                    #     tff.run.experiment(d, tff.lstm.diff.scale, "tff.lstm.diff.scale", 10*60, data=data) %>% tff.save.exp.new
                    #
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.resample.5.scale.mean"))
                    #     tff.run.experiment(d, tff.lstm.resample.5.scale.mean, "tff.lstm.resample.5.scale.mean", 10*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.resample.3.scale.mean"))
                    #     tff.run.experiment(d, tff.lstm.resample.3.scale.mean, "tff.lstm.resample.3.scale.mean", 10*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.lstm.resample.1.scale.mean"))
                    #     tff.run.experiment(d, tff.lstm.resample.1.scale.mean, "tff.lstm.resample.1.scale.mean", 10*60, data=data) %>% tff.save.exp.new
                    #
                    # if(!tff.already.done("experiments_new", d, "tff.auto.arima.tail"))
                    #     tff.run.experiment(d, tff.auto.arima.tail, "tff.auto.arima.tail", 10*60, data=data) %>% tff.save.exp.new
                    #
                    # if(!tff.already.done("experiments_new", d, "tff.stl.w.tail.lstm.scale.mean"))
                    #     tff.run.experiment(d, tff.stl.w.tail.lstm.scale.mean, "tff.stl.w.tail.lstm.scale.mean", 20*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.w.tail.lstm.resample.5.identity"))
                    #     tff.run.experiment(d, tff.stl.w.tail.lstm.resample.5.identity, "tff.stl.w.tail.lstm.resample.5.identity", 20*60, data=data) %>% tff.save.exp.new
                    # if(!tff.already.done("experiments_new", d, "tff.stl.w.tail.40000.lstm.resample.5.identity"))
                    #    tff.run.experiment(d, tff.stl.w.tail.40000.lstm.resample.5.identity, "tff.stl.w.tail.40000.lstm.resample.5.identity", 20*60, data=data) %>% tff.save.exp.new

                    # if(!tff.already.done("experiments_new", d, "tff.naive.total"))
                    #     tff.run.experiment(d, tff.naive.total, "tff.naive.total", 20*60, data=data) %>% tff.save.exp.new

                    # Se está ejecutando
                    if(!tff.already.done("experiments_new", d, "tff.lstm.exogeno.scale.mean"))
                        tff.run.experiment(d, tff.lstm.exogeno.scale.mean, "tff.lstm.exogeno.scale.mean", 20*60, data=data) %>% tff.save.exp.new
                    # Todavia no está ejecutado
                    if(!tff.already.done("experiments_new", d, "tff.lstm.exogeno.scale.mean.resample.5"))
                        tff.run.experiment(d, tff.lstm.exogeno.scale.mean.resample.5, "tff.lstm.exogeno.scale.mean.resample.5", 20*60, data=data) %>% tff.save.exp.new
                }
            }, error = print)
        }
    }
}
