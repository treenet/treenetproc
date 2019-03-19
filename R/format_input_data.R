#' Check Format of Input Data
#'
#' \code{check_format()} checks whether input table is in long or wide format.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
#' @examples
#'
check_format <- function(df, input) {
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
#'   and correct column classes are assigned.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#'
format_input <- function(df, input, tz) {
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
#' @keywords internal
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
#'   data and temperature data overlap.
#'
#' @param df input \code{data.frame} of dendrometer data.
#' @param tem input \code{data.frame} of temperature or climate data.
#'
#' @keywords internal
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


#' Create Dummy Temperature Dataset
#'
#' \code{create_temp_dummy} creates a dummy temperature dataset if local
#'   temperature measurements are missing. Temperatures in the months December,
#'   January and February are 0°C (i.e. frost shrinkage is possible).
#'   Tempeartures in the other months are 10°C (i.e. no frost shrinkage).
#'
#' @param df input \code{data.frame} of dendrometer data.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
#' @examples
#'
create_temp_dummy <- function(df) {
  start_posix <- df$ts[1]
  end_posix <- df$ts[length(df$ts)]
  reso <- as.numeric(difftime(df$ts[2], df$ts[1], units = "mins"))
  dd <- seq(start_posix, end_posix, by = paste0(reso, " min"))

  df <- as.data.frame(dd) %>%
    dplyr::select("ts" = 1) %>%
    dplyr::mutate(series = "airtemperature") %>%
    dplyr::mutate(month = as.numeric(substr(ts, 6, 7))) %>%
    dplyr::mutate(value = ifelse(month %in% c(1, 2, 12), 0, 10)) %>%
    dplyr::select(series, ts, value)

  return(df)
}


#' Create Dummy Temperature Dataset for Treenet Data
#'
#' \code{create_temp_dummy} creates a dummy temperature dataset if local
#'   temperature measurements are missing. Temperatures in the months December,
#'   January and February are 0°C (i.e. frost shrinkage is possible).
#'   Tempeartures in the other months are 10°C (i.e. no frost shrinkage).
#'
#' @param df input \code{data.frame} of dendrometer data.
#'
#' @keywords internal
#'
#' @examples
#'
create_temp_dummy_treenet <- function(df) {
  df <- df %>%
    dplyr::distinct(ts) %>%
    dplyr::mutate(month = as.numeric(substr(ts, 6, 7))) %>%
    dplyr::mutate(value = ifelse(month %in% c(1, 2, 12), 0, 10)) %>%
    dplyr::mutate(series = "airtemperature") %>%
    dplyr::mutate(version = 0) %>%
    dplyr::select(series, ts, value, version)

  return(df)
}
