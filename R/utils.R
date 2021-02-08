

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
tff.setup.knitr <- function() {
    knitr::opts_chunk$set(
        warning = FALSE,
        message = FALSE,
        echo = FALSE,
        cache = TRUE,
        # cache.path = "cache/",
        collapse = TRUE,
        comment = "#>"
        # formatR.indent = 2,
        # width = 55,
        # digits = digits,
        # warnPartialMatchAttr = FALSE,
        # warnPartialMatchDollar = FALSE,
    )
}

#' Title
#'
#' @return
#' @export
#'
#' @examples
#'
tff.setup.pander <- function() {
    # panderOptions("table.caption.prefix", "Tabla: ")
}

#' Title
#'
#' @param pkg
#' @param fields
#'
#' @return
#' @export
#'
#' @examples
#'
tff.show_pkg_versions <- function(pkg = ".",
                              fields = c("Depends", "Imports", "LinkingTo", "Suggests")) {
    stopifnot(purrr::is_scalar_character(pkg), pkg != "")
    fields <-
        match.arg(fields,
                  c("Depends", "Imports", "LinkingTo", "Suggests"),
                  several.ok = TRUE)

    avail <- dplyr::as_tibble(utils::available.packages())

    if (pkg == ".") {
        pkg_deps <-
            unclass(dplyr::as_tibble(read.dcf(
                file.path(devtools::package_file(), "DESCRIPTION")
            )))
        pkg <- pkg_deps$Package
        purrr::map(fields, ~ stringi::stri_split_lines(pkg_deps[[.]])) %>%
            purrr::map(function(x) {
                if (length(x) > 0) {
                    unlist(x) %>%
                        stringi::stri_replace_all_regex(" \\(.*$|,", "") %>%
                        purrr::discard(`%in%`, c("", "R"))
                } else {
                    x
                }
            }) -> pkg_deps
        names(pkg_deps) <- fields
    } else {
        pkg_deps <-
            purrr::map(fields, ~ flatten_chr((
                tools::package_dependencies(pkg,  which = .)
            )))
        names(pkg_deps) <- fields
    }

    pkg_deps <- purrr::discard(pkg_deps, function(x) {
        length(x) == 0
    })

    purrr::map(pkg_deps, function(x) {
        non_base <- dplyr::filter(avail, Package %in% x)
        base <- setdiff(x, non_base$Package)

        non_base %>%
            dplyr::mutate(pv = sprintf("%s (>= %s)", Package, Version)) %>%
            dplyr::select(pv) %>%
            purrr::flatten_chr() -> pkg_plus_version

        sort(c(pkg_plus_version, base))

    }) -> pkg_deps

    cat("Package: ", pkg, "\n", sep = "")
    purrr::walk(names(pkg_deps), function(x) {
        cat(x, ":\n", sep = "")
        sprintf("    %s", pkg_deps[[x]]) %>%
            paste0(collapse = ",\n") %>%
            cat()
        cat("\n")

    })
}

#' Title
#'
#' @param x
#' @param format
#' @param digits
#' @param row.names
#' @param col.names
#' @param align
#' @param caption
#' @param format.args
#' @param escape
#' @param longtable
#' @param booktabs
#' @param font_size
#' @param latex_options
#' @param position
#' @param full_width
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' Tabla \@ref(tab:contingencia-tipo-elem) ...
#'
#' ```{r contingencia-tipo-elem}
#' tmp <- as.data.frame(table(metrics$tipo_elem))
#' tff.kable.tiny(tmp, caption='Tabla de contingencia de los tipos de punto de medida')
#' ```
tff.kable.tiny <-
    function(x,
             format,
             digits = getOption("digits"),
             row.names = NA,
             col.names = NA,
             align,
             caption = NULL,
             format.args = list(),
             escape = T,
             longtable = T,
             booktabs = T,
             font_size = 10,
             hold_position = F,
             latex_options = c("striped", "repeat_header"),
             position = "center",
             full_width = F,
             ...) {
        k <- knitr::kable(
            x,
            format,
            digits = digits,
            row.names = row.names,
            col.names = col.names,
            align,
            caption = caption,
            format.args = format.args,
            escape = escape,
            longtable = longtable,
            booktabs = booktabs,
            ...
        )
        if(hold_position)
            latex_options <- c(latex_options, "hold_position")
        kableExtra::kable_styling(
            k,
            latex_options = latex_options,
            font_size = font_size,
            position = position,
            full_width = full_width
        )
    }

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' \@ref(tab:muestra-datos-flujo) ...
#'
#' ```{r muestra-datos-flujo}
#' metrics <- tff.get.parsed.metrics(y=2018, m=9)
#' tff.kable.scaled(head(metrics),
#'                  caption = 'Muestra de datos de flujo de tráfico de septiembre 2018 en Madrid')
#' ```
tff.kable.scaled <- function(x,
                             longtable = F,
                             font_size = NULL,
                             hold_position = F,
                             latex_options = c("striped", "scale_down", "repeat_header"),
                             ...) {
    tff.kable.tiny(
        x,
        hold_position = hold_position,
        longtable = longtable,
        font_size = font_size,
        latex_options = latex_options,
        ...
    )
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' \@ref(tab:muestra-kable-default) ...
#'
#' ```{r muestra-kable-default}
#' metrics <- tff.get.parsed.metrics(y=2018, m=9)
#' tff.kable.default(head(metrics),
#'                   caption = 'Muestra de datos de flujo de tráfico de septiembre 2018 en Madrid')
#' ```
tff.kable.default <- function(x,
                             longtable = T,
                             font_size = NULL,
                             hold_position = F,
                             latex_options = c("striped", "repeat_header"),
                             ...) {
    tff.kable.tiny(
        x,
        hold_position = hold_position,
        longtable = longtable,
        font_size = font_size,
        latex_options = latex_options,
        ...
    )
}

#' Title
#'
#' @param x
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' Tabla \ref{tabs:tabla-frecuencia-api}:
#'
#' ```{r, echo=T, results = 'asis'}
#' tff.freq(all.locations$tipo_elem,
#'          caption='\\label{tabs:tabla-frecuencia-api} Tabla de frecuencias')
#' ```
tff.freq <- function(x, caption, ...) {
    summarytools::freq(
        x,
        style = 'rmarkdown',
        headings = F,
        order = 'default',
        plain.ascii = F,
        caption = caption,
        ...
    )

}

#' Title
#'
#' @param x
#' @param y
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' Tabla \ref{tabs:tabla-contingencia-api}:
#'
#' ```{r, echo=T, results = 'asis'}
#' tff.ctable(all.locations$tipo_elem, all.locations$tipo_elem,
#'            caption='\\label{tabs:tabla-contingencia-api} Tabla de contingencia')
#' ```
tff.ctable <- function(x, y, caption = '', ...) {
    summarytools::ctable(
        x,
        y,
        style = 'rmarkdown',
        headings = F,
        plain.ascii = F,
        caption = caption,
        ...
    )
}

#' Title
#'
#' @param df
#' @param caption
#' @param digits
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' Tabla \ref{tabs:tabla-descr-api}:
#
#' ```{r, echo=T, results = 'asis'}
#' tff.descr(all.locations, digits=c(0,2,2),
#'           caption='\\label{tabs:tabla-descr-api} Tabla de descripción de variables')
#' ```
tff.descr <- function(df,
                      caption = '',
                      digits = 2,
                      hold_position = F,
                      ...) {
    summarytools::descr(
         df,
         style = 'rmarkdown',
         headings = F,
         plain.ascii = F
    ) %>%
        tff.kable.default(caption = caption, digits = digits, hold_position = hold_position, ...)
}

#' Title
#'
#' @param df
#' @param caption
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#'
#' Tabla \ref{tabs:tabla-summary-api}:
#
#' ```{r, echo=T, results = 'asis'}
#' tff.dfSummary(all.locations,
#'               caption='\\label{tabs:tabla-summary-api} Tabla de resumen de datasets')
#' ```
tff.dfSummary <- function(df, caption = '', ...) {
    summarytools::dfSummary(
        df,
        max.distinct.values = 2,
        graph.col = F,
        headings = F,
        plain.ascii = F,
        caption = caption,
        ...
    )
}
