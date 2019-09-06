#' Check for Logical Input
#'
#' \code{check_logical} checks whether input variable is of type logical.
#'
#' @param var variable to be checked.
#' @param var_name character, name of variable to be checked.
#'
#' @keywords internal
#'
check_logical <- function(var, var_name) {
  if (!(var %in% c(TRUE, FALSE))) {
    stop(paste(var_name, "has to be either 'TRUE' or 'FALSE'."))
  }
}


#' Checks Input of Variable Method
#'
#' \code{check_method} checks the input of the variable method and converts
#'   it to a symbol.
#'
#' @inheritParams check_logical
#'
#' @keywords internal
#'
check_method <- function(var, var_name) {
  if (!(var %in% c("value", "diff_val"))) {
    stop(paste(var_name, "has to be either 'value' or 'diff_val'."))
  }

  var <- dplyr::sym(var)
  return(var)
}


#' Check Date Format
#'
#' \code{isdate} checks whether values in a vector are in a standard
#'   date or datetime format and converts them to \code{POSIXct}.
#'
#' @param datevec character vector in a standard date or datetime format.
#' @param date_formats character vector, date or datetime formats that are
#'   checked
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
isdate <- function(datevec, var_name, date_formats, tz) {

  datevec <- as.character(datevec)
  date_check <-
    tryCatch(
      !is.na(as.POSIXct(datevec, tz = tz,
                        tryFormats = date_formats)),
      error = function(err) {
        FALSE
        })

  if (length(unique(date_check)) > 1) {
    stop(paste("Date format of some dates in '", var_name, "' not recognized.",
               " Provide dates in a valid format, e.g. %Y-%m-%d %H:%M:%S"))
  }
  if (!(unique(date_check))) {
    stop(paste0("Date format of dates in '", var_name, "' not recognized.",
               " Provide dates in a valid format, e.g. %Y-%m-%d %H:%M:%S"))
  }
  if (unique(date_check)) {
    datevec <- as.POSIXct(datevec, tz = tz,
                          tryFormats = date_formats)
  }

  return(datevec)
}


#' Check Date Vector Input
#'
#' \code{check_datevec} checks whether all dates specified are in a standard
#'   date or datetime format. Dates are converted to \code{POSIXct}.
#'
#' @inheritParams isdate
#' @inheritParams corr_dendro_L3
#'
#' @keywords internal
#'
check_datevec <- function(datevec, tz) {

  date_formats <- c("%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S",
                    "%Y.%m.%d %H:%M:%S",
                    "%d-%m-%Y %H:%M:%S", "%d/%m/%Y %H:%M:%S",
                    "%d.%m.%Y %H:%M:%S",
                    "%m-%d-%Y %H:%M:%S", "%m/%d/%Y %H:%M:%S",
                    "%m.%d.%Y %H:%M:%S",
                    "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y.%m.%d %H:%M",
                    "%d-%m-%Y %H:%M", "%d/%m/%Y %H:%M", "%d.%m.%Y %H:%M",
                    "%m-%d-%Y %H:%M", "%m/%d/%Y %H:%M", "%m.%d.%Y %H:%M",
                    "%Y-%m-%d %H", "%Y/%m/%d %H", "%Y.%m.%d %H",
                    "%d-%m-%Y %H", "%d/%m/%Y %H", "%d.%m.%Y %H",
                    "%m-%d-%Y %H", "%m/%d/%Y %H", "%m.%d.%Y %H",
                    "%Y-%m-%d", "%Y/%m/%d", "%Y.%m.%d",
                    "%d-%m-%Y", "%d/%m/%Y", "%d.%m.%Y",
                    "%m-%d-%Y", "%m/%d/%Y", "%m.%d.%Y")

  dates <- isdate(datevec = datevec, date_formats = date_formats, tz = tz)

  return(dates)
}


#' Check Date Input for Period
#'
#' \code{check_date_period} checks whether the provided dates overlap with
#'   the period of the data.
#'
#' @param df input \code{data.frame}.
#' @inheritParams isdate
#' @inheritParams corr_dendro_L3
#'
#' @keywords internal
#'
check_date_period <- function(datevec, datevec_name, df) {

  start <- df$ts[1]
  end <- df$ts[nrow(df)]

  for (i in 1:length(datevec)) {
    if (datevec[i] < start | datevec[i] > end) {
      stop(paste0(datevec[i], " in '", datevec_name, "' is not part of the ",
                  "measurement period."))
    }
  }
}


#' Check data_L1 Input
#'
#' \code{check_data_L1} checks the input data given to the variable
#'   \code{data_L1}.
#'
#' @param data_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}.
#'
#' @keywords internal
#'
check_data_L1 <- function(data_L1) {
  if (sum(colnames(data_L1) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned dendrometer data generated with 'proc_L1'.")
  }
}


#' Check data_L2 Input
#'
#' \code{check_data_L2} checks the input data given to the variable
#'   \code{data_L1}.
#'
#' @param data_L2 processed dendrometer data as produced by
#'   \code{\link{proc_dendro_L2}}.
#'
#' @keywords internal
#'
check_data_L2 <- function(data_L2) {
  if (sum(colnames(data_L2) %in% c("series", "ts", "value", "version", "max",
                                   "twd")) != 6) {
    stop("provide processed dendrometer data generated with 'proc_dendro_L2'.")
  }
}


#' Checks Input of Series
#'
#' \code{check_series} checks the input of the variabel series.
#'
#' @param df input \code{data.frame}.
#' @inheritParams corr_dendro_L3
#'
#' @keywords internal
#'
check_series <- function(df, series) {
  if (length(series) == 0 & length(unique(df$series)) > 1) {
    stop("name of the series needs to be provided in 'series'.")
  }
  if (length(series) > 1) {
    stop("only one series can be provided at a time in 'series'.")
  }
  if (length(series) == 1) {
    if (!(series %in% unique(df$series))) {
      stop("provided name in 'series' does not exist.")
    }
  }
}


#' Check the Date Sequence in Delete
#'
#' \code{check_delete} checks whether the first value in a pair of dates
#'   occurs before the second value.
#'
#' @inheritParams corr_dendro_L3
#'
#' @keywords internal
#'
check_delete <- function(delete) {
  for (d in seq(1, length(delete), by = 2)) {
    if (delete[d] > delete[d + 1]) {
      stop(paste0("Error in date pair: ", delete[d], ", ", delete[d + 1],
                  ". The first date of a date pair in 'delete' needs ",
                  "to be smaller than the second date."))
    }
  }
}

