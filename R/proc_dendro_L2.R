#' Process L1 Dendrometer Data to L2
#'
#' \code{proc_dendro_L2} processes time-aligned (\code{L1}) dendrometer data
#'   to processed (\code{L2}) dendrometer data. It removes jumps, corrects
#'   small gaps and calculates growth, tree water deficit and maximum daily
#'   shrinkage.
#'
#' @param dendro_L1 \code{data.frame} with time-aligned dendrometer
#'   data. Output of function \code{\link{proc_L1}}.
#' @param temp_L1 \code{data.frame} with time-aligned temperature data.
#'   Output of function \code{\link{proc_L1}} (see Details for further
#'   information).
#' @param tol_jump numeric, defines the tolerance of the threshold above or
#'   below which a value is flagged for jump correction.
#' @param tol_out numeric, defines the tolerance of the threshold above or
#'   below which a value is classified as outlier (see Details for further
#'   information).
#' @param frost_thr numeric, increases the thresholds for outlier
#'   classifiation in periods of probable frost (i.e. temperature <
#'   \code{lowtemp}). The thresholds are multiplied by the value provided.
#' @param lowtemp numeric, specifies temperature in °C below which shrinkage
#'   in stem diameter due to frost is expected. Default value is set to
#'   \code{5°C} due to hysteresis shortly before or after frost events.
#' @param interpol numeric, length of gaps (in minutes) in which values are
#'   linearly interpolated. Set \code{interpol = 0} to disable gapfilling.
#'   If \code{interpol = NULL} the default value is set to
#'   \code{interpol = 2.1 * reso}.
#' @param frag_len numeric, specifies the length of data fragments occurring
#'   in-between missing data that are automatically deleted during data
#'   cleaning. This can be helpful to remove short fragments of erroneous data
#'   within a period of missing data, i.e. after jumps. If
#'   \code{frag_len = NULL} the devault value is set to \code{frag_len = 2.1}.
#' @param plot logical, specify whether a comparison of \code{L1} and \code{L2}
#'   data should be plotted.
#' @param plot_phase logical, specify whether maxima and minima used for the
#'   calculation of the phase statistics should be plotted.
#' @param iter_clean numeric, specifies the number of times the cleaning
#'   process is repeated. Can be used to check whether running the cleaning
#'   process many times has an effect on the results.
#' @inheritParams proc_L1
#' @inheritParams plot_proc_L2
#'
#' @details Time-aligned temperature data \code{temp_L1} is used to define
#'   periods in which frost shrinkage is probable, e.g. when the temperature
#'   is <5°C. Without temperature data, shrinkages due to frost may be
#'   classified as outliers.
#'
#'   Temperature data can also be attached to dendrometer data. In this case,
#'   the \code{series} name of temperature data has to contain the string
#'   \code{temp}. In case no temperature dataset is specified, a sample
#'   temperature dataset will be used with a warning. The sample temperature
#'   dataset assumes probable frost shringkage in the months December, January
#'   and February.
#'
#'   Outliers are classified based on the value difference (referred to as
#'   \code{diff}) between two timesteps. A \code{diff} is classified as an
#'   outlier if it deviates more than a threshold value from the first
#'   or third quartile of all \code{diff}. Thresholds are calculated as:\cr
#'   \code{threshold_low = quantile(diff, probs = 0.25) + tol *
#'   \link[stats]{mad}(diff)}\cr
#'   \code{threshold_high = quantile(diff, probs = 0.75) + tol *
#'   \link[stats]{mad}(diff)}
#'
#'   Thus, \code{tol} describes the number of times \code{\link[stats]{mad}}
#'   is added to the first or third quartile of \code{diff}, before a
#'   \code{diff} is classified as an outlier. \code{tol} is increased to
#'   \code{tol * frost_thr} in periods of probable frost (i.e. in periods
#'   where the air temperature is below \code{lowtemp}).
#'
#'   The maximum daily shrinkage \code{mds} is calculated similarly as in
#'   the function \code{\link[dendrometeR]{phase_def}} in the package
#'   \code{dendrometeR}. First, local maxima and minima are identified
#'   using a moving window. \code{mds} is only calculated if a local maximum
#'   occurs before a local minimum (i.e. if the stem shrinks during the day).
#'
#' @return The function returns a \code{data.frame} with processed dendrometer
#'  data containing the following columns:
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{max}{highest measured value up to this timestamp.}
#'    \item{twd}{tree water deficit, i.e. the amount of stem shrinkage
#'      expressed as the difference between \code{max} and \code{value}.
#'    \item{gro_yr}{growth since the beginning of the year. Also calculated
#'      for years with missing data.}
#'    \item{flags}{character vector specifying the changes that occurred
#'      during the processing. For more details see the following vignette:
#'      \href{../doc/Introduction-to-treenetproc.html}{\code{vignette("Introduction-to-treenetproc", package = "treenetproc")}}}
#'    \item{version}{package version that was used.}
#'
#' @export
#'
#' @examples
#' proc_dendro_L2(dendro_L1 = dendro_data_L1, plot_period = "monthly",
#'                plot_export = FALSE)
#'
proc_dendro_L2 <- function(dendro_L1, temp_L1 = NULL,
                           tol_out = 10, tol_jump = 50,
                           lowtemp = 5, frost_thr = 5,
                           interpol = NULL, frag_len = NULL,
                           plot = TRUE, plot_period = "full",
                           plot_show = "all", plot_export = TRUE,
                           plot_name = "proc_L2_plot",
                           plot_phase = FALSE, iter_clean = 1, tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Save input variables for plotting -----------------------------------------
  if (plot) {
    passenv$tol_jump_plot <- tol_jump
    passenv$tol_out_plot <- tol_out
    passenv$frost_thr_plot <- frost_thr
    passenv$lowtemp_plot <- lowtemp
    passenv$tz_plot <- tz
  }


  # Check input data ----------------------------------------------------------
  df <- dendro_L1
  check_data_L1(data_L1 = df)

  if (length(temp_L1) != 0) {
    tem <- temp_L1
    tem_series <- unique(tem$series)

    if (length(grep("temp", tem_series, ignore.case = T)) > 1) {
      stop("provide single temperature dataset.")
    }
    if (sum(colnames(tem) %in% c("series", "ts", "value", "version")) != 4) {
      stop("provide time-aligned temperature data generated with 'proc_L1'")
    }

    # add column with temperature reference
    df$temp_ref <- tem_series
  }

  passenv$sample_temp <- FALSE
  if (length(temp_L1) == 0) {
    df_series <- unique(df$series)

    # for data from server
    if ("temp_ref" %in% colnames(df)) {
      temp_series <- stats::na.omit(unique(df$temp_ref))
      tem <- df %>%
        dplyr::filter(series %in% temp_series)
      df <- df %>%
        dplyr::filter(!(series %in% temp_series))
      dendro_L1 <- df
    }
    # for user-specified data
    if (!("temp_ref" %in% colnames(df))) {
      if (length(grep("temp", df_series, ignore.case = T)) > 1) {
        stop("provide single temperature dataset.")
      }
      if (length(grep("temp", df_series, ignore.case = T)) == 0) {
        tem <- create_temp_dummy(df = df)
        message("sample temperature dataset is used.")
        passenv$sample_temp <- TRUE
        df <- df %>%
          dplyr::mutate(temp_ref = "airtemperature")
      }
      if (length(grep("temp", df_series, ignore.case = T)) == 1) {
        temp_series <- df_series[grep("temp", df_series, ignore.case = T)]
        tem <- df %>%
          dplyr::filter(series == temp_series)
        df <- df %>%
          dplyr::filter(series != temp_series) %>%
          dplyr::mutate(temp_ref = temp_series)
        dendro_L1 <- df
      }
    }
  }

  reso_df <- reso_check_L1(df = df)
  reso_tem <- reso_check_L1(df = tem)
  if (reso_df != reso_tem) {
    stop("provide both dendrometer and temperature data at the same time ",
         "resolution.")
  } else {
    passenv$reso <- reso_df
  }

  # check for overlap between df and tem
  ts_overlap_check(df = df, tem = tem)


  # Process to L2 (jump and gap corrections) ----------------------------------
  series_vec <- unique(df$series)
  list_L2 <- vector("list", length = length(series_vec))
  df_L1 <- df
  for (s in 1:length(series_vec)) {
    message(paste0("processing ", series_vec[s], "..."))
    df <- df_L1 %>%
      dplyr::filter(series == series_vec[s])

    if (all(is.na(df$value))) {
      message(paste0("There is no data available for ", series_vec[s],
                     ". This series is skipped."))
      next
    }

    # remove leading and trailing NA's
    na_list <- remove_lead_trail_na(df = df)
    df <- na_list[[1]]
    lead_trail_na <- na_list[[2]]

    df <- createfrostflag(df = df, tem = tem, lowtemp = lowtemp,
                          sample_temp = passobj("sample_temp"))

    clean_list <- vector("list", length = iter_clean + 1)
    clean_list[[1]] <- df
    for (i in 1:iter_clean) {
      df <- clean_list[[i]]

      # remove outliers
      df <- calcdiff(df = df, reso = passobj("reso"))
      df <- createflagmad(df = df, reso = passobj("reso"), wnd = NULL,
                          tol = tol_out, save_thr = TRUE,
                          correction = "outlier", frost_thr = frost_thr)
      df <- executeflagout(df = df, len = 1, frag_len = frag_len,
                           plot_density = FALSE, plot_export = plot_export,
                           frost_thr = frost_thr)

      # remove jumps (jump correction)
      df <- calcdiff(df = df, reso = passobj("reso"))
      df <- createflagmad(df = df, reso = passobj("reso"), wnd = NULL,
                          tol = tol_jump, save_thr = TRUE,
                          correction = "jump", frost_thr = frost_thr)
      df <- createjumpflag(df = df)
      df <- executejump(df = df)

      clean_list[[i + 1]] <- df
    }
    df <- clean_list[[iter_clean + 1]]

    df <- fillintergaps(df = df, reso = passobj("reso"),
                        interpol = interpol, type = "linear", flag = TRUE)
    df <- calcmax(df = df)
    df <- calctwdgro(df = df, tz = tz)
    df <- summariseflags(df = df)

    # append leading and trailing NA's
    df <- append_lead_trail_na(df = df, na = lead_trail_na)

    df <- df %>%
      dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
      dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
      dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
      dplyr::mutate(frost = ifelse(is.na(value), NA, frost)) %>%
      dplyr::select(series, ts, value, max, twd, gro_yr, frost, flags) %>%
      dplyr::mutate(
        version = utils::packageDescription("treenetproc",
                                            fields = "Version", drop = TRUE))

    list_L2[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L2)

  if (plot) {
    print("plot data...")
    plot_proc_L2(dendro_L1 = dendro_L1, dendro_L2 = df,
                 plot_period = plot_period, plot_show = plot_show,
                 plot_export = plot_export, plot_name = plot_name, tz = tz,
                 print_vars = TRUE)
  }

  return(df)
}
