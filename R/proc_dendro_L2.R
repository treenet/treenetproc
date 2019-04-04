#' Process L1 Dendrometer Data to L2
#'
#' \code{proc_dendro_L2()} processes time-aligned (\code{L1}) dendrometer data
#'   to processed (\code{L2}) dendrometer data. It removes jumps, corrects
#'   small gaps and calculates growth, tree water deficit and maximum daily
#'   shrinkage.
#'
#' @param dendro_data \code{data.frame} with time-aligned dendrometer
#'   data. Output of function \code{proc_L1}.
#' @param temp_data \code{data.frame} with time-aligned temperature data.
#'   Output of function \code{proc_L1} (see Details for further information).
#' @param val_range numeric vector specifying the minimum and maximum
#'   \code{c(min, max)} of credible dendrometer measurement values. Values
#'   lower than \code{min} or higher than \code{max} are deleted without
#'   notice.
#' @param iter_clean numeric, specifies the number of times the cleaning
#'   process should run.
#' @param lowtemp numeric, specifies temperature in °C below which shrinkage
#'   in stem diameter due to frost is expected. Default value is set to
#'   \code{5°C} due to hysteresis shortly before or after frost events.
#' @param plot_mds logical, specify whether maxima and minima used for the
#'   calculation of mds (maximum daily shrinkage) should be plotted.
#' @inheritParams proc_L1
#' @inheritParams createflagmad
#'
#' @details \code{temp_data} is used to define periods in which frost shrinkage
#'   is probable, i.e. when temperature is <5°C. Without temperature data,
#'   frost shrinkage may be classified as outliers.
#'
#'   \code{temp_data} can also contain other climate data. In this case, the
#'   \code{series} name of temperature data has to contain the string
#'   \code{temp}. In case no temperature dataset is specified, a sample
#'   temperature dataset will be used with a warning. The sample temperature
#'   dataset assumes probable frost shringkage in the months December, January
#'   and February.
#'
#' @return The function returns:
#'  a \code{data.frame} with processed dendrometer data containing the
#'  following columns
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{max}{highest measured value up to this timestamp.}
#'    \item{twd}{tree water deficit, i.e. the amount of stem shrinkage
#'      expressed as the difference between \code{max} and \code{value}.}
#'    \item{mds}{maximum daily shrinkage, calculated as the difference between
#'      a local maximum that occurs before a local minimum during one day. If
#'      there is no local maximum or minimum or if the minimum occurs prior to
#'      the maximum, then \code{mds = NA}. This may occur on days with rain or
#'      in winter.}
#'    \item{gro_yr}{growth since the beginning of the year. Also calculated if
#'      for years with missing data.}
#'    \item{flags}{number specifying whether and which changes occurred during
#'      the processing.}
#'    \item{version}{processing version.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' proc_dendro_L2(dendro_data = data_L1_dendro, temp_data = data_L1_temp,
#'                val_range = c(0, 20000))
#' }
#'
proc_dendro_L2 <- function(dendro_data, temp_data = NULL,
                           val_range = c(0, 20000), wnd = 6, n_mad = 8,
                           iter_clean = 2, lowtemp = 5, tz = "Etc/GMT-1",
                           plot_mds = FALSE) {

  # Check input variables -----------------------------------------------------
  if (!is.numeric(val_range)) {
    stop("provide numeric limits to 'val_range'.")
  }
  if (val_range[1] > val_range[2]) {
    stop("first value of 'val_range' has to be smaller than second value.")
  }

  # Check input data ----------------------------------------------------------
  df <- dendro_data

  if (sum(colnames(df) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned data generated with 'proc_L1'")
  }

  if (length(temp_data) != 0) {
    tem <- temp_data
    tem_series <- unique(tem$series)

    if (length(grep("temp", tem_series, ignore.case = T)) == 0) {
      message("check temperature data, series name does not contain 'temp'.")
    }
    if (length(grep("temp", tem_series, ignore.case = T)) > 1) {
      stop("provide single temperature dataset.")
    }
    if (sum(colnames(tem) %in% c("series", "ts", "value", "version")) != 4) {
      stop("provide time-aligned temperature data generated with 'proc_L1'")
    }
  }

  if (length(temp_data) == 0) {
    df_series <- unique(df$series)

    if (length(grep("temp", df_series, ignore.case = T)) > 1) {
      stop("provide single temperature dataset.")
    }
    if (length(grep("temp", df_series, ignore.case = T)) == 0) {
      tem <- create_temp_dummy(df = df)
      message("sample temperature dataset will be used.")
    }
    if (length(grep("temp", df_series, ignore.case = T)) == 1) {
      temp_series <- df_series[grep("temp", df_series, ignore.case = T)]
      tem <- df %>%
        dplyr::filter(series == temp_series)
      df <- df %>%
        dplyr::filter(series != temp_series)
    }
  }

  reso_df <- reso_check(df = df)
  reso_tem <- reso_check(df = tem)
  if (reso_df != reso_tem) {
    stop("provide both dendrometer and temperature data at the same time ",
         "resolution.")
  } else {
    passenv$reso <- reso_df
  }

  ts_overlap_check(df = df, tem = tem)


  # Process to L2 (jump and gap corrections) ----------------------------------
  series_vec <- unique(df$series)
  list_L2 <- vector("list", length = length(series_vec))
  df_L1 <- df
  for (s in 1:length(series_vec)) {
    df <- df_L1 %>%
      dplyr::filter(series == series_vec[s])

    # save and remove leading and trailing NA's
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

    df <- cleanoutofrange(df = df, val_range = val_range)
    df <- createfrostflag(df = df, tem = tem, lowtemp = 5)

    df <- calcdiff(df = df, reso = passobj("reso"))
    df <- createflagmad(df = df, reso = passobj("reso"), wnd = 6, n_mad = 8)
    df <- executeflagout(df = df, len = 2)

    clean_list <- vector("list", length = length(iter_clean))
    clean_list[[1]] <- df
    for (i in 1:iter_clean) {
      df <- clean_list[[i]]

      df <- calcdiff(df = df, reso = passobj("reso"))
      df <- createflagmad(df = df, reso = passobj("reso"), wnd = 6, n_mad = 8)
      df <- creategapflag(df = df, reso = passobj("reso"),
                          gaple = 24 * (60 / passobj("reso")))
      df <- createjumpflag(df = df, thr = 0.2)
      df <- executejump(df = df)

      clean_list[[i + 1]] <- df
    }
    df <- clean_list[[iter_clean + 1]]

    df <- fillintergaps(df = df, reso = passobj("reso"),
                        wnd = 4 * 60 / passobj("reso"),
                        type = "linear")
    df <- calcmax(df = df)
    df <- calctwdgro(df = df, tz = tz)
    df <- calcmds(df = df, reso = passobj("reso"), tz = tz,
                  plot_mds = plot_mds)
    df <- summariseflags(df = df)

    if (lead) {
      df <- dplyr::bind_rows(leading_na, df)
    }
    if (trail) {
      df <- dplyr::bind_rows(df, trailing_na)
    }

    df <- df %>%
      dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
      dplyr::mutate(mds = ifelse(is.na(value), NA, mds)) %>%
      dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
      dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
      dplyr::mutate(version = 2) %>%
      dplyr::select(series, ts, value, max, twd, mds, gro_yr, flags, version)

    list_L2[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L2)

  return(df)
}
