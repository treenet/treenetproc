#' Time-Alignement of Measurements to Regular Time Intervals
#'
#' \code{tsalign} aligns measurements to regular time intervals by
#'   interpolating between irregular time intervals. If precipitation data
#'   is supplied the values are summed up.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @return \code{data.frame} with measurements aligned to regular time
#' intervals, i.e. to \code{reso}.
#'
#' @keywords internal
#'
tsalign <- function(df, reso, year, tz) {

  series <- unique(df$series)

  out_generatets <- generatets(df, reso, year, tz)
  df <- out_generatets[[1]]
  ts_seq <- out_generatets[[2]]

  if (length(grep("prec", series, ignore.case = T)) > 0) {
    prec_sum_raw <- sum(df$value, na.rm = T)
    df <- fillintergaps_prec(df = df, reso = reso)
    prec_sum_proc <- sum(df$value, na.rm = T)
    if (!(identical(prec_sum_raw, prec_sum_proc))) {
      stop(paste0("there was an error with the time-alignement in the ",
                  "precipitation data. Error in ", series, "."))
    }
  } else {
    df <- fillintergaps(df = df, reso = reso, flag = FALSE,
                        interpol = NULL)
  }

  df <- df %>%
    dplyr::left_join(ts_seq, ., by = "ts") %>%
    dplyr::arrange(ts) %>%
    dplyr::distinct()

  return(df)
}


#' Generate Regular Time Stamps
#'
#' \code{generatets} generates regular time stamps over whole period at
#'   specified resolution (\code{reso}).
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
generatets <- function(df, reso, year, tz) {

  if (year == "asis") {
    start_posix <- roundtimetoreso(df = df, reso = reso, pos = "start",
                                   tz = tz)
    end_posix <- roundtimetoreso(df = df, reso = reso, pos = "end", tz = tz)
  }
  if (year == "full") {
    start <- paste0(substr(df$ts[1], 1, 4), "-01-01 00:00:00")
    start_posix <- as.POSIXct(start, format = "%Y-%m-%d %H:%M:%S",
                              tz = tz)
    end_time <- as.POSIXct("00:00:00", format = "%H:%M:%S") -
      as.difftime(reso, units = "mins")
    end <- paste0(substr(df$ts[nrow(df)], 1, 4), "-12-31 ",
                  substr(end_time, 12, 19))
    end_posix <- as.POSIXct(end, format = "%Y-%m-%d %H:%M:%S",
                            tz = tz)
  }

  dd <- seq(start_posix, end_posix, by = paste0(reso, " min"))
  dd <- as.data.frame(dd) %>%
    dplyr::select("ts" = 1)

  df <- dplyr::full_join(dd, df, by = "ts") %>%
    dplyr::arrange(ts) %>%
    dplyr::distinct()

  list_df_dd <- list(df, dd)

  return(list_df_dd)
}


#' Rounds Irregular Time Stamp to Regular
#'
#' \code{roundtimetoreso} rounds first and last irregular timestep to
#'   next or previous regular timestep.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
roundtimetoreso <- function(df, reso, pos, tz) {

  if (!(pos %in% c("start", "end"))) {
    stop("provide 'pos' with either 'start' or 'end'.")
  }

  if (pos == "start") {
    date_time <- df$ts[1]
  }
  if (pos == "end") {
    date_time <- df$ts[nrow(df)]
  }
  H_date_time <- as.numeric(format(date_time, "%H"))
  M_date_time <- as.numeric(format(date_time, "%M"))
  S_date_time <- as.numeric(format(date_time, "%S"))
  D_date_time <- as.POSIXct(substr(date_time, 1, 10), format = "%Y-%m-%d",
                        tz = tz)
  secs_date_time <- 3600 * H_date_time + 60 * M_date_time + S_date_time

  if (pos == "start") {
    date_time_posix <-
      as.POSIXct(ceiling(secs_date_time / (60 * reso)) * 60 * reso,
                 origin = D_date_time, tz = tz)
  }
  if (pos == "end") {
    date_time_posix <-
      as.POSIXct(floor(secs_date_time / (60 * reso)) * 60 * reso,
                 origin = D_date_time, tz = tz)
  }

  return(date_time_posix)
}


#' Interpolate Between Irregular and Regular Timesteps
#'
#' \code{fillintergaps} interpolates gaps between irregular and regular
#'   timesteps, i.e. only over very small gaps (more an alignement than an
#'   interpolation).
#'
#' @param df input \code{data.frame}.
#' @param type specify type of interpolation between regular timesteps.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
fillintergaps <- function(df, reso, interpol = NULL, type = "linear",
                          flag = FALSE) {

  if (type != "linear" | length(type) == 0) {
    print("no gapfilling...")
  }
  if (length(interpol) == 0) {
    interpol <- passobj("reso") * 2.1
  }

  if (interpol > 0) {
    nc <- ncol(df)
    if (type == "linear") {
      df <- df %>%
        dplyr::arrange(ts) %>%
        dplyr::mutate(isgap = is.na(value)) %>%
        dplyr::mutate(gaps = cumsum(isgap)) %>%
        dplyr::mutate(y = c(0, diff(gaps, lag = 1))) %>%
        dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
        dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
        dplyr::mutate(gapnr = cumsum(z)) %>%
        dplyr::mutate(diff_ts = as.numeric(difftime(ts, dplyr::lag(ts, 1),
                                                    units = "mins"))) %>%
        dplyr::mutate(diff_ts = c(0, diff_ts[2:dplyr::n()])) %>%
        dplyr::group_by(gapnr) %>%
        dplyr::mutate(gaple_mins = sum(diff_ts)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(value = ifelse(isgap & gaple_mins < interpol,
                                     stats::approx(ts, value, ts)$y, value))
    }

    if (flag) {
      df <- df %>%
        dplyr::mutate(flagfill = ifelse(isgap & gaple_mins < interpol,
                                        TRUE, FALSE)) %>%
        dplyr::select(1:nc, flagfill)
    } else {
      df <- df %>%
        dplyr::select(1:nc)
    }
  }

  return(df)
}


#' Interpolate Precipitation Data Between Irregular and Regular Timesteps
#'
#' \code{fillintergaps_prec} interpolates gaps between irregular and regular
#'   timesteps for precipitation data. In contrast to \code{fillintergaps}
#'   the values are summed up and not lineraly interpolated.
#'
#' @param df input \code{data.frame}.
#' @inheritParams proc_L1
#'
#' @keywords internal
#'
fillintergaps_prec <- function(df, reso) {
  nc <- ncol(df)
  df[2, -which(names(df) == "ts")] <- df[1, -which(names(df) == "ts")]
  df <- df[-1, ]

  df <- df %>%
    dplyr::arrange(ts) %>%
    dplyr::mutate(group_reso = cut(ts, breaks = paste(reso, "min"),
                                   labels = FALSE)) %>%
    dplyr::group_by(group_reso) %>%
    dplyr::mutate(value = ifelse(all(is.na(value)), NA,
                                      sum(value, na.rm = TRUE))) %>%
    dplyr::ungroup() %>%
    dplyr::select(1:nc)

  return(df)
}
