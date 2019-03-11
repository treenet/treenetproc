#' Time-Alignement of Measurements to Regular Time Intervals
#'
#' \code{tsalign} aligns measurements to regular time intervals by
#' interpolating between irregular time intervals.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_dendro_L1
#'
#' @return \code{data.frame} with measurements aligned to regular time
#' intervals, i.e. to \code{reso}.
#'
#' @examples
#'
tsalign <- function(df, reso, year, tz) {
  # This function is Copyright
  df <- generatets(df, reso, all = TRUE, year, tz)
  df <- fillintergaps(df, reso, wnd, type = "linear")
  df <- generatets(df, reso, all = FALSE, year, tz)

  return(df)
}


#' Time-Alignement of Precipitation Data to Regular Time Intervals
#'
#' \code{tsalign_prec} aligns precipitation measurements to regular time
#' intervals by summing up values of irregular time intervals.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_L1
#'
#' @examples
#'
tsalign_prec <- function(df, reso, year, tz) {
  # This function is Copyright
  df <- generatets(df, reso, all = TRUE, year, tz)
  df <- fillintergaps_prec(df, reso)
  df <- generatets(df, reso, all = FALSE, year, tz)

  return(df)
}


#' Generate Regular Time Stamps
#'
#' \code{generatets} generates regular time stamps over whole period.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_L1
#'
#' @examples
#'
generatets <- function(df, reso, all, year, tz) {
  # This function is Copyright
  if (all != TRUE & all != FALSE) {
    stop("provide 'all' with either TRUE or FALSE")
  }

  if (year == "asis") {
    start_posix <- roundtimetoreso(df = df, pos = "start", tz = tz,
                                   reso = reso)
    end_posix <- roundtimetoreso(df = df, pos = "end", tz = tz,
                                 reso = reso)
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

  if (all == TRUE) {
    df <- dplyr::full_join(dd, df, by = "ts") %>%
      dplyr::arrange(ts) %>%
      dplyr::distinct()
    }
  if (all == FALSE) {
      df <- dplyr::left_join(dd, df, by = "ts") %>%
        dplyr::arrange(ts) %>%
        dplyr::distinct()
  }

  return(df)
}


#' Rounds Irregular Time Stamp to Regular
#'
#' \code{roundtimetoreso} rounds first and last irregular timestep to
#' next or previous regular timestep.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_L1
#'
#' @examples
#'
roundtimetoreso <- function(df, reso, pos, tz) {
  if (!(pos %in% c("start", "end"))) {
    stop("provide 'pos' with either 'start' or 'end'")
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
#' timesteps, i.e. only over very small gaps (more an alignement than an
#' interpolation).
#'
#' @param df input \code{data.frame}.
#'
#' @param wnd length of time window over which values are interpolated.
#'
#' @param type specify type of interpolation between regular timesteps.
#'
#' @inheritParams proc_L1
#'
#' @examples
#'
fillintergaps <- function(df, reso, wnd = reso * 2.1, type = "linear") {
  # This function is Copyright
  if (type != "linear" | length(type) == 0) {
    print("no gapfilling...")
  }

  # save leading and trailing NA's
  lead <- FALSE
  if (is.na(df$value[1])) {
    nrow_na <- which(!is.na(df$value))[1] - 1
    leading_na <- df[c(1:nrow_na), ]
    df <- df[-c(1:nrow_na), ]
    lead <- TRUE
  }
  le <- nrow(df)
  trail <- FALSE
  if (is.na(df$value[le])) {
    nrow_na <- max(which(!is.na(df$value))) + 1
    trailing_na <- df[c(nrow_na:le), ]
    df <- df[-c(nrow_na:le), ]
    trail <- TRUE
  }
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
      dplyr::mutate(value = ifelse(isgap & gaple_mins < wnd,
                                   approx(ts, value, ts)$y, value)) %>%
      dplyr::select(1:nc)
  }

  if (lead) {
    df <- dplyr::bind_rows(leading_na, df)
  }
  if (trail) {
    df <- dplyr::bind_rows(df, trailing_na)
  }

  return(df)
}


#' Interpolate Precipitation Data Between Irregular and Regular Timesteps
#'
#' \code{fillintergaps_prec} interpolates gaps between irregular and regular
#' timesteps for precipitation data. In contrast to \code{fillintergaps}
#' the values are summed up and not lineraly interpolated.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams proc_L1
#'
#' @examples
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
