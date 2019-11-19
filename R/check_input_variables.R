#' Check all input variables automatically
#'
#' \code{check_input_variables} checks all input variables of exported
#'   functions.
#'
#' @param list list of all input arguments to be checked.
#'
#' @keywords internal
#'
check_input_variables <- function(list) {

  var_names <- names(list)

  # Check character variables -------------------------------------------
  # data_format
  if ("data_format" %in% var_names) {
    if (length(list$data_format) > 1) {
      stop("You cannot download multiple 'data_formats' at once.\n",
           "'data_format' needs to be 'L0', 'L1' or 'L2'.")
    }
    if (length(list$data_format) > 0) {
      if (!(list$data_format %in% c("L0", "L1", "L2"))) {
        stop("Variable 'data_format' needs to be 'L0', 'L1' or 'L2'.")
      }
    }
  }

  # site, sensor_name and sensor_class
  if ("site" %in% var_names & "sensor_name" %in% var_names &
      "sensor_class" %in% var_names) {
    if (length(list$site) == 0 & length(list$sensor_name) == 0 &
        length(list$sensor_class) == 0) {
      stop(paste0("Specify at least one of the following: site, ",
                  " sensor_name, sensor_class."))
    }
  }

  # temp_name
  if ("temp_name"%in% var_names) {
    if (length(list$temp_name) > 1) {
      stop("Provide single temperature dataset in 'temp_name'.")
    }
  }

  # server
  if ("server" %in% var_names) {
    if (!(list$server %in% c("treenet", "decentlab"))) {
      stop("Variable 'server' needs to be 'treenet' or 'decentlab'.")
    }
    if (length(list$data_format) != 0) {
      if (list$server == "decentlab" & list$data_format != "L0") {
        stop(paste("Only 'L0' data can be downloaded from the decentlab",
                   "server. Change variable 'data_format' to 'L0'."))
      }
    }
  }

  # plot_period
  if ("plot_period" %in% var_names) {
    if (!(list$plot_period %in% c("full", "yearly", "monthly"))) {
      stop(paste0("Variable 'plot_period' needs to be ",
                  "'full', 'yearly' or 'monthly'."))
    }
  }

  # plot_show
  if ("plot_show" %in% var_names) {
    # diff_corr is used for function 'corr_dendro_L3'
    if (!(list$plot_show %in% c("all", "diff", "diff_corr"))) {
      stop(paste0("Variable 'plot_show' needs to be ",
                  "'all' or 'diff'."))
    }
  }

  # proc_to
  if ("proc_to" %in% var_names) {
    if (!(list$proc_to %in% c("L1", "L2"))) {
      stop("Variable 'proc_to' needs to be 'L1' or 'L2'.")
    }
  }

  # year
  if ("year" %in% var_names) {
    if (!(list$year %in% c("asis", "full"))){
      stop("Variable 'year' needs to be 'asis' or 'full'.")
    }
  }

  # input
  if ("input" %in% var_names) {
    if (!(list$input %in% c("long", "wide"))) {
      stop("Variable 'input' needs to be 'long' or 'wide'.")
    }
  }


  # Check date variables -------------------------------------------
  # from
  if ("from" %in% var_names) {
    if (length(list$from) != 0) {
      if (is.na(as.Date(list$from, format = "%Y-%m-%d"))) {
        stop("Provide variable 'from' in date format. ",
             "Format needs to be 'YYYY-MM-DD'.")
      }
    }
  }

  # to
  if ("to" %in% var_names) {
    if (length(list$to) != 0) {
      if (is.na(as.Date(list$to, format = "%Y-%m-%d"))) {
        stop("Provide variable 'to' in date format. ",
             "Format needs to be 'YYYY-MM-DD'.")
      }
    }
  }

  # Check logical variables -------------------------------------------
  # export
  if ("export" %in% var_names) {
    check_logical(var = list$export, var_name = "export")
  }

  # bind_df
  if ("bind_df" %in% var_names) {
    check_logical(var = list$bind_df, var_name = "bind_df")
  }

  # export and bind_df
  if ("export" %in% var_names & "bind_df" %in% var_names) {
    if (list$export && list$bind_df) {
      stop(paste0("Variables 'export' and 'bind_df' cannot both be TRUE ",
                  "at the same time."))
    }
  }

  # plot
  if ("plot" %in% var_names) {
    check_logical(var = list$plot, var_name = "plot")
  }

  # plot_export
  if ("plot_export" %in% var_names) {
    check_logical(var = list$plot_export, var_name = "plot_export")
  }

  # plot_mds
  if ("plot_mds" %in% var_names) {
    check_logical(var = list$plot_mds, var_name = "plot_mds")
  }

  # print_vars
  if ("print_vars" %in% var_names) {
    check_logical(var = list$print_vars, var_name = "print_vars")
  }

  # temp_ref
  if ("temp_ref" %in% var_names) {
    check_logical(var = list$temp_ref, var_name = "temp_ref")
  }

  # use_intl
  if ("use_intl" %in% var_names) {
    check_logical(var = list$use_intl, var_name = "use_intl")
  }


  # Check numeric variables -------------------------------------------
  # tol_jump
  if ("tol_jump" %in% var_names) {
    check_numeric(var = list$tol_jump, var_name = "tol_jump")
  }

  # tol_out
  if ("tol_out" %in% var_names) {
    check_numeric(var = list$tol_out, var_name = "tol_out")
  }

  # frost_thr
  if ("frost_thr" %in% var_names) {
    check_numeric(var = list$frost_thr, var_name = "frost_thr")
  }

  # lowtemp
  if ("lowtemp" %in% var_names) {
    check_numeric(var = list$lowtemp, var_name = "lowtemp")
  }

  # iter_clean
  if ("iter_clean" %in% var_names) {
    check_numeric(var = list$iter_clean, var_name = "iter_clean")
  }

  # interpol
  if ("interpol" %in% var_names) {
    if (length(list$interpol > 0)) {
      check_numeric(var = list$interpol, var_name = "interpol")
    }
  }

  # frag_len
  if ("frag_len" %in% var_names) {
    if (length(list$frag_len > 0)) {
      check_numeric(var = list$frag_len, var_name = "frag_len")
    }
  }

  # n_days
  if ("n_days" %in% var_names) {
    if (length(list$n_days > 0)) {
      check_numeric(var = list$n_days, var_name = "n_days")
    }
  }

}


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
  if (!(is.logical(var))) {
    stop(paste0("Variable '", var_name,
                "' has to be either 'TRUE' or 'FALSE'."))
  }
}


#' Check for Numeric Input
#'
#' \code{check_numeric} checks whether input variable is of type numeric.
#'
#' @inheritParams check_logical
#'
#' @keywords internal
#'
check_numeric <- function(var, var_name) {
  if (!(is.numeric(var))) {
    stop(paste0("Variable '", var_name, "' hast to be numeric."))
  }
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

  # round to day
  start <- as.POSIXct(trunc(df$ts[1], "days"))
  end <- as.POSIXct(trunc(df$ts[nrow(df)], "days") + 86400)

  for (i in 1:length(datevec)) {
    if (datevec[i] < start | datevec[i] > end) {
      stop(paste0("The date '", datevec[i], "' in '", datevec_name,
                  "' is not part of the measurement period."))
    }
  }
}


#' Check if Package is Installed
#'
#' \code{check_package} checks whether a suggested package that is needed
#'   for a specific function is already installed.
#'
#' @param pck_name character, name of the required package.
#'
#' @keywords internal
#'
check_package <- function(pck_name) {

      if (!requireNamespace(pck_name, quietly = TRUE)) {
      stop(paste0("Package '", pck_name, "' is needed for this function to ",
                  "work. Please install it."), call. = FALSE)
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
  if (sum(colnames(data_L2) %in% c("series", "ts", "value", "max",
                                   "twd", "version")) != 6) {
    stop("provide processed dendrometer data generated with 'proc_dendro_L2'.")
  }
}


#' Checks Input of Series
#'
#' \code{check_series} checks the input of the variable series.
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
