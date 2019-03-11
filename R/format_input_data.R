#' Check Format of Input Data
#'
#' \code(check_format) checks whether input table is in long or wide format.
#'
#' @param df input \{data.frame}.
#'
#' @examples
#'
check_format <- function(df) {
  nr_value_col <- length(which(sapply(df, class) == "numeric" &
                                 sapply(sapply(df, unique), length) > 1))
  if (nr_value_col > 1 & input == "long") {
    stop("provided data is not in 'long' format.")
  }
  if (nr_value_col == 1 & input == "wide") {
    stop("provided data is not in 'wide' format.")
  }
}


#' Format Input Data
#'
#' \code{format_input} formats input data. Wide format is convertet to long
#' and correct column classes are assigned.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams process_dendro
#'
#' @return \code{data.frame}
#'
#' @examples
#'
format_input <- function(df, input) {
  if (input == "wide") {
    nc <- ncol(df)
    nr <- nrow(df)
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      transform(ts = as.POSIXct(as.character(ts),
                                format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      dplyr::mutate(id = 1:nr) %>%
      reshape(., timevar = "series", idvar = "id", varying = c(2:nc),
              direction = "long", v.names = "value", times = col_names) %>%
      dplyr::select(-id) %>%
      transform(value = as.numeric(value)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  if (input == "long") {
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      transform(ts = as.POSIXct(as.character(ts),
                                format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      transform(value = as.numeric(value)) %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  return(df)
}


#' Check Time Resolution of Input Data
#'
#' \code{reso_check} checks time resolution of input \code{data.frame}.
#'
#' @param df input \code{data.frame}.
#'
#' @examples
#'
reso_check <- function(df) {
  reso_check <- df %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(reso_check = as.numeric(difftime(ts, dplyr::lag(ts, 1),
                                                   units = "mins"))) %>%
    dplyr::filter(!is.na(reso_check)) %>%
    dplyr::summarise(reso_check = unique(reso_check)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-series) %>%
    unlist(use.names = FALSE)

  if (length(unique(reso_check)) > 1) {
    stop("please provide time-aligned data.")
  } else {
    reso <- unique(reso_check)
  }

  return(reso)
}


#' Check for Time Overlap Between Datasets
#'
#' \code{ts_overlap_check} checks whether measurement periods of dendrometer
#' data and temperature data overlap.
#'
#' @param df input \code{data.frame} of dendrometer data.
#'
#' @param tem input \code{data.frame} of temperature or climate data.
#'
#' @examples
#'
ts_overlap_check <- function(df, tem) {
  df_start <- df$ts[1]
  df_end <- df$ts[nrow(df)]
  tem_start <- tem$ts[1]
  tem_end <- tem$ts[nrow(tem)]

  if (tem_start > df_end) {
    stop("there is no overlap between dendrometer and temperature data.")
  }
  if (tem_end < df_start) {
    stop("there is no overlap between dendrometer and temperature data.")
  }
}
