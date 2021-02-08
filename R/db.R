
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.metric.df.to.json <- function(df) {
    jsonlite::toJSON(df, dataframe = 'columns', POSIXt = "string",
                     null = "list", na = "string", auto_unbox = F)
}
#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.metric.df.from.json <- function(df) {
    data <- as.data.frame(jsonlite::fromJSON(df), stringsAsFactors = F)
    data$carga <- as.integer(data$carga)
    data$fecha <- lubridate::ymd_hms(data$fecha)
    data
}



#' Title
#'
#' @return
#' @export
#'
#' @examples
tff.get.pool <- function() {
    if (not(exists("mySQL.pool")) || not(RMySQL::dbIsValid(get("mySQL.pool")))) {
        message("Creating MySQL pool ...\n")
        mySQL.pool <<- pool::dbPool(
            drv = RMySQL::MySQL(),
            dbname = "xxx",
            host = "xxx",
            username = "xxx",
            password = "xxx"
        )
    }
    get("mySQL.pool")
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tff.get.conn <- tff.get.pool

#' Title
#'
#' @return
#' @export
#'
#' @examples
tff.release.pool <- function() {
  pool::poolClose(tff.get.conn())
}


#' Title
#'
#' @param lst
#'
#' @return
#' @export
#'
#' @examples
tff.as.sql.in <- function(lst) {
  lst %>% unlist() %>% paste0(",", collapse = " ") %>% substr(., 1, nchar(.) - 1)
}

#' Title
#'
#' @param y
#' @param m
#' @param d
#'
#' @return
#' @export
#'
#' @examples
tff.exist.metric <- function(y, m, d) {
  on.exit(tff.release.pool)
  0 < DBI::dbGetQuery(tff.get.conn(),
                      sprintf("select count(1) from metrics where year=%d and month=%d and device=%d;", y, m, d))[1, 1]
}

#' Title
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.put.metric <- function(df) {
  on.exit(tff.release.pool)
  y <- as.integer(year(df$fecha[1]))
  m <- as.integer(month(df$fecha[1]))
  d <- ifelse(is.null(df$id), ifelse(is.null(df$idelem), df$identif[1], df$idelem[1]), df$id[1])
  if (!tff.exist.metric(y, m, d)) {
      q <- "INSERT INTO metrics (year, month, device, data) VALUES(%d, %d, %d, '%s')"
      DBI::dbGetQuery(tff.get.conn(), sprintf(q, y, m, d, tff.encode.obj(df)))
  }
}

#' Title
#'
#' @param y
#' @param m
#'
#' @return
#' @export
#'
#' @examples
tff.down.put.metrics <- function(y, m) {
  df <- tff.download.data(y, m, "metric")
  if (!is.null(df$identif)) {
    df %>% dplyr::group_by(identif) %>% dplyr::do(data.frame(tff.put.metric(.)))
  } else if (!is.null(df$idelem)) {
    df %>% dplyr::group_by(idelem) %>% dplyr::do(data.frame(tff.put.metric(.)))
  } else if (!is.null(df$id)) {
    df %>% dplyr::group_by(id) %>% dplyr::do(data.frame(tff.put.metric(.)))
  }
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
tff.estimate.cores <- function() {
  max(1, parallel::detectCores() - 1)
}

#' Title
#'
#' @param y
#' @param m
#' @param d
#'
#' @return
#' @export
#'
#' @examples
tff.get.raw.metrics <- function(y = c(), m = c(), d = c()) {
  on.exit(tff.release.pool)
  y.in <- ifelse(length(y) == 0, "-1", tff.as.sql.in(y))
  m.in <- ifelse(length(m) == 0, "-1", tff.as.sql.in(m))
  d.in <- ifelse(length(d) == 0, "-1", tff.as.sql.in(d))
  metrics <- "SELECT data
                FROM metrics
               WHERE ('%s'='-1' OR year IN (%s))
                 AND ('%s'='-1' OR month IN (%s))
                 AND ('%s'='-1' OR device IN (%s))" %>%
    sprintf(y.in, y.in, m.in, m.in, d.in, d.in) %>%
    DBI::dbGetQuery(tff.get.conn(), .)
  if (nrow(metrics) < 64) {
    metrics %>% magrittr::extract(, 1) %>% lapply(tff.decode.obj)
  } else {
    metrics %>% magrittr::extract(, 1) %>% parallel::mclapply(tff.decode.obj, mc.cores = tff.estimate.cores())
  }
}

#' Title
#'
#' @param y
#' @param m
#' @param d
#'
#' @return
#' @export
#'
#' @examples
tff.get.parsed.metrics <- function(y = c(), m = c(), d = c()) {
  metrics <- tff.get.raw.metrics(y, m, d)
  if (length(metrics) < 64) {
    metrics %>% lapply(tff.parse.raw.metric) %>% data.table::rbindlist()
  } else {
    metrics %>% parallel::mclapply(tff.parse.raw.metric, mc.cores = tff.estimate.cores()) %>% data.table::rbindlist()
  }
}

#' Title
#'
#' @param y
#' @param m
#'
#' @return
#' @export
#'
#' @examples
tff.exist.location <- function(y, m) {
  on.exit(tff.release.pool)
  0 < DBI::dbGetQuery(tff.get.conn(),
                      sprintf("select count(1) from locations where year=%d and month=%d;", y, m))[1, 1]
}

#' Title
#'
#' @param y
#' @param m
#' @param df
#'
#' @return
#' @export
#'
#' @examples
tff.put.location <- function(y, m, df) {
  on.exit(tff.release.pool)
  if (!tff.exist.location(y, m)) {
    q <- "INSERT INTO locations (year, month, data) VALUES(%d, %d, '%s')"
    DBI::dbGetQuery(tff.get.conn(), sprintf(q, y, m, tff.encode.obj(df)))
  }
}

#' Title
#'
#' @param y
#' @param m
#'
#' @return
#' @export
#'
#' @examples
tff.down.put.locations <- function(y, m) {
  df <- tff.download.data(y, m, "device")
  tff.put.location(y, m, df)
}

#' Title
#'
#' @param y
#' @param m
#'
#' @return
#' @export
#'
#' @examples
tff.get.raw.location <- function(y, m) {
  on.exit(tff.release.pool)
  sprintf("SELECT data FROM locations WHERE year=%d AND month=%d", y, m) %>%
    DBI::dbGetQuery(tff.get.conn(), .) %>% magrittr::extract(, 1) %>% tff.decode.obj()
}

#' Title
#'
#' @param y
#' @param m
#' @param with.ym
#'
#' @return
#' @export
#'
#' @examples
tff.get.parsed.location <- function(y, m, with.ym = F) {
  df <- tff.parse.raw.location(tff.get.raw.location(y, m))
  if (with.ym) {
    df$year <- y
    df$month <- m
  }
  df
}

#' Title
#'
#' @param with.ym
#'
#' @return
#' @export
#'
#' @examples
tff.get.all.parsed.locations <- function(with.ym = F) {
  on.exit(tff.release.pool)
  DBI::dbGetQuery(tff.get.conn(), "select year, month from locations") %>%
        apply(1, function(r) tff.get.parsed.location(r[1], r[2], with.ym = with.ym)) %>%
        data.table::rbindlist()
}

#' Title
#'
#' @param metrics
#'
#' @return
#' @export
#'
#' @examples
tff.metrics.as.xts <- function(metrics) {
    tmp <- metrics %>% data.frame %>% pad
    rownames(tmp) <- tmp$fecha
    tmp$fecha <- NULL
    tmp$tipo_elem <- NULL
    tmp$error <- ifelse(tmp$error == "N",0,ifelse(tmp$error == "Y",1,NA))
    xts::as.xts(tmp)
}

# tff.exist.json <- function(y,d) {
#     q <- "select count(1) from metrics_json where year=%d and device=%d;"
#     0 < dbGetQuery(tff.get.conn(), sprintf(q, y, d))[1, 1]
# }
#
# tff.put.json <- function(df) {
#     y <- as.integer(year(df$fecha[1]))
#     d <- df$id[1]
#     if (tff.exist.json(y,d)) {
#         q <- "DELETE FROM metrics_json WHERE year=%d AND device=%d"
#         dbGetQuery(tff.get.conn(), sprintf(q, y, d))
#     }
#     q <- "INSERT INTO metrics_json (year, device, data) VALUES(%d, %d, '%s')"
#     dbGetQuery(tff.get.conn(), sprintf(q, y, d, tff.metric.df.to.json(df)))
# }
#
# metrics <- tff.get.parsed.metrics(y=2018, d=4000)
# tff.put.json(metrics)




