#' Check for Logical Input
#'
#' \code{check_logical} checks whether input variable is of type logical.
#'
#' @param var variable to be checked.
#' @param var_name character, name of variable to be checked.
#'
#' @keywords internal
#'
#' @examples
#'
check_logical <- function(var, var_name) {
  if (!(var %in% c(TRUE, FALSE))) {
    stop(paste(var_name, "has to be either 'TRUE' or 'FALSE'."))
  }
}


#' Check Multiple Dates Input
#'
#' \code{check_datevec} checks whether all dates specified are in the correct
#'   format.
#'
#' @param var character vector, dates to be checked.
#' @param var_name character, name of variable to be checked.
#' @inheritParams corr_dendro_L3
#'
#' @keywords internal
#'
#' @examples
#'
check_datevec <- function(var, var_name, tz) {
  var_posix <- .POSIXct(rep(NA, length(var)), tz = tz)
  for (i in 1:length(var)) {
    stop <- 0
    if (nchar(var[i]) == 10 &
        is.na(as.POSIXct(var[i], format = "%Y-%m-%d"))) {
      stop <- 1
    }
    if (nchar(var[i]) == 19 &
        is.na(as.POSIXct(var[i], format = "%Y-%m-%d %H:%M:%S"))) {
      stop <- 1
    }
    if (!(nchar(var[i]) %in% c(10, 19))) {
      stop <- 1
    }

    if (stop == 1) {
      stop(paste(var[i], "of", var_name, "is not in the required date format.",
                 "Format needs to be either 'YYYY-MM-DD' or",
                 "'YYYY-MM-DD HH:MM:SS'."))
    } else {
      if (nchar(var[i]) == 10) {
        var_posix[i] <- as.POSIXct(var[i], format = "%Y-%m-%d", tz = tz)
      }
      if (nchar(var[i]) == 19) {
        var_posix[i] <-
          as.POSIXct(var[i], format = "%Y-%m-%d %H:%M:%S", tz = tz)
      }
    }
  }
  return(var_posix)
}


#' Check data_L1 Input
#'
#' \code{check_data_L1} checks the input data given to the variable
#'   \code{\link{data_L1}}.
#'
#' @param data_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}.
#'
#' @keywords internal
#'
#' @examples
#'
check_data_L1 <- function(data_L1) {
  if (sum(colnames(data_L1) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned dendrometer data generated with 'proc_L1'.")
  }
}


#' Check data_L2 Input
#'
#' \code{check_data_L2} checks the input data given to the variable
#'   \code{\link{data_L1}}.
#'
#' @param data_L2 processed dendrometer data as produced by
#'   \code{\link{proc_dendro_L2}}.
#'
#' @keywords internal
#'
#' @examples
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
#' @examples
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
#' @examples
#'
check_delete <- function(delete) {
  for (d in seq(1, length(delete), by = 2)) {
    if (delete[d] > delete[d + 1]) {
      stop(paste0("The first date of a date pair in 'delete' needs to be ",
                  "smaller than the second date. Error in date pair: ",
                  delete[d], ", ", delete[d + 1], "."))
    }
  }
}
