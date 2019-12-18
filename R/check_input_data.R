#' Check Format of Input Data
#'
#' \code{check_format()} checks whether input table is in long or wide format.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
check_format <- function(df, input) {
  if (input == "long") {
    if (!("series" %in% colnames(df))) {
      stop("you need to provide a 'series' column for data in 'long' format.")
    }
  }
  if (input == "wide") {
    if ("series" %in% colnames(df)) {
      stop("data in 'wide' format does not need a 'series' column.")
    }
  }

  nr_value_col <- length(which(sapply(df, class) == "numeric" &
                                 sapply(sapply(df, unique), length) > 1))
  if (nr_value_col == 0) {
    stop(paste("provide at least one numeric column with raw dendrometer or",
               "raw meteorological measurements."))
  }
  if (nr_value_col > 1 & input == "long") {
    stop("provided data is not in 'long' format.")
  }
}


#' Check TS Column
#'
#' \code{check_ts} checks whether the 'ts' column is in a standard date
#'   or datetime format.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
check_ts <- function(df, date_format, tz) {

  if (!("ts" %in% colnames(df))) {
    stop("column with time stamps (named 'ts') is missing.")
  }

  ts <- as.character(df$ts)
  ts <- as.POSIXct(ts, format = date_format, tz = tz)

  if (is.na(unique(ts)[1])) {
    stop(paste("Date format in 'ts' not recognized. Please provide",
               "'ts' in a valid format or specify a custom format in",
               "'date_format'."))
  }

  df$ts <- ts

  return(df)
}


#' Format Input Data
#'
#' \code{format_input} formats input data. Wide format is converted to long
#'   and correct column classes are assigned.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
format_input <- function(df, input, tz) {
  if (input == "wide") {
    nc <- ncol(df)
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      dplyr::mutate(id = 1:nrow(.)) %>%
      stats::reshape(., timevar = "series", idvar = "id", varying = c(2:nc),
                     direction = "long", v.names = "value",
                     times = col_names) %>%
      dplyr::select(-id)

    if (is.factor(df$value)) {
      df <- df %>%
        transform(value = as.numeric(levels(value)[value]))
    } else {
      df <- df %>%
        transform(value = as.numeric(value))
    }

    df <- df %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  if (input == "long") {
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      transform(value = as.numeric(value)) %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  return(df)
}


#' Check for Sensors with Missing Data
#'
#' \code{check_missing} removes series without any data or with the same
#'   value over the whole period.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
check_missing <- function(df) {
  series_missing <- df %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(unique_value = ifelse(length(unique(value)) == 1, 1, 0)) %>%
    dplyr::filter(unique_value == 1) %>%
    dplyr::select(series) %>%
    unlist(use.names = FALSE) %>%
    unique()

  if (length(series_missing) > 0) {
    df <- df %>%
      dplyr::filter(!(series %in% series_missing))

    message(paste0("the following series were excluded due to missing data",
                   "over the entire period: ",
                   paste0(series_missing, collapse = ", "), "."))
  }

  return(df)
}


#' Check Time Resolution of L0 Input Data
#'
#' \code{reso_check} extracts the median time resolution of \code{L0} data
#'   and compares it to the user-specified \code{reso}. If \code{2.1 * reso}
#'   is smaller than the median time resolution, a warning message is printed.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
reso_check_L0 <- function(df, reso) {

  # calculate median resolution of input data
  reso_med <- df %>%
    dplyr::mutate(reso = as.numeric(difftime(ts, dplyr::lag(ts, 1),
                                             units = "mins"))) %>%
    dplyr::summarise(reso_med = stats::median(reso, na.rm = TRUE)) %>%
    dplyr::select(reso_med) %>%
    unlist(use.names = FALSE)

  if (2.1 * reso < reso_med) {
    message(paste("The specified 'reso' is very small compared to the",
                  "median time resolution of the input data. The",
                  "time-alignment may therefore not work properly.",
                  "Please increase the value of 'reso'."))
  }
}


#' Check Time Resolution of L1 Input Data
#'
#' \code{reso_check} checks time resolution of \code{L1} input
#'   \code{data.frame}.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
reso_check_L1 <- function(df) {
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
#'   Temperatures in the other months are 10°C (i.e. no frost shrinkage
#'   assumed).
#'
#' @param df input \code{data.frame} of dendrometer data.
#'
#' @keywords internal
#'
create_temp_dummy <- function(df) {
  start_posix <- min(df$ts, na.rm = TRUE)
  end_posix <- max(df$ts, na.rm = TRUE)
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


#' Remove Leading and Trailing NA's
#'
#' \code{lead_trail_na} removes and saves leading and trailing \code{NA} to a
#'   \code{list}.
#'
#' @param df input \code{data.frame}
#'
#' @return list, first element is \code{df} and the second element are the
#'   leading and trailing \code{NA}.
#'
#' @keywords internal
#'
remove_lead_trail_na <- function(df) {

  lead <- FALSE
  if (is.na(df$value[1])) {
    nrow_na <- which(!is.na(df$value))[1] - 1
    leading_na <- df[c(1:nrow_na), ]
    na_rows <- leading_na
    df <- df[-c(1:nrow_na), ]
    lead <- TRUE
  }
  le <- nrow(df)
  trail <- FALSE
  if (is.na(df$value[le])) {
    nrow_na <- max(which(!is.na(df$value))) + 1
    trailing_na <- df[c(nrow_na:le), ]
    na_rows <- trailing_na
    if (lead) {
      na_rows <- dplyr::bind_rows(leading_na, trailing_na)
    }
    df <- df[-c(nrow_na:le), ]
    trail <- TRUE
  }

  if (sum(lead, trail) == 0) {
    na_rows <- NULL
  }
  list_na <- list(df, na_rows)

  return(list_na)
}


#' Append Leading and Trailing NA's
#'
#' \code{append_lead_trail_na} appends leading and trailing \code{NA} back
#'   to the \code{data.frame}.
#'
#' @param df input \code{data.frame}.
#' @param na \code{data.frame} containing rows with leading and/or trailing
#'   \code{NA}.
#'
#' @keywords internal
#'
append_lead_trail_na <- function(df, na) {

  if (length(na) != 0) {
    df <- dplyr::bind_rows(df, na) %>%
      dplyr::arrange(ts)
  }

  return(df)
}
