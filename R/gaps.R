
#' Title
#'
#' @param d device
#'
#' @return
#' @export
#'
#' @examples
tff.get.carga.gaps <- function(d) {
    x <- tff.get.parsed.metrics(d=d) %>% tff.clean %>% tff.pad
    gaps <- x$carga %>% statsNA(printOnly = F)
    gaps <- as.list(c(list(device=d), gaps))
    gaps$percentageNAs <- 100*gaps$numberNAs/gaps$lengthTimeSeries
    gaps
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tff.compute.carga.gaps <- function() {
    devices <- "select distinct device
                from metrics
                order by device asc " %>%
        dbGetQuery(tff.get.conn(), .) %>%
        magrittr::extract(,1)

    sapply(devices, function(d) {
        g <- get.carga.gaps(d)
        dbWriteTable(tff.get.conn(), value = as.data.frame(g)[1,], name = "carga_gaps", append = TRUE)
    })
}

#' #' Title
#' #'
#' #' @param device
#' #' @param y
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' tff.compute.statsNA <-   function(device, y=c()) {
#'     metrics <- tff.get.parsed.metrics(d = device, y = y)
#'     if(0 < nrow(metrics)) {
#'         metrics.xts <- metrics %>% tff.metrics.as.xts
#'         list(id = data.frame(id = zoo::coredata(metrics.xts$id)[1]),
#'              intensidad = metrics.xts$intensidad %>% imputeTS::statsNA(printOnly = F),
#'              ocupacion = metrics.xts$ocupacion %>% imputeTS::statsNA(printOnly = F),
#'              carga = metrics.xts$carga %>% imputeTS::statsNA(printOnly = F),
#'              vmed = metrics.xts$vmed %>% imputeTS::statsNA(printOnly = F),
#'              error = metrics.xts$error %>% imputeTS::statsNA(printOnly = F),
#'              periodo_integracion = metrics.xts$periodo_integracion %>% imputeTS::statsNA(printOnly = F)) %>%
#'             data.frame(stringsAsFactors = F)
#'     }
#' }
