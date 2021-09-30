#' Calculate Parameters of Shrinkage and Expansion Phases
#'
#' \code{phase_stats} calculates the timing, duration, amplitude and the rate
#'   of change of shrinkage and expansion phases. In addition, days on which
#'   radial change is likely driven by transpiration or temperature are
#'   identified.
#'
#' @param dendro_L2 input \code{data.frame} containing cleaned \code{L2}
#'   dendrometer data (obtained from functions \code{\link{proc_dendro_L2}}
#'   or \code{\link{corr_dendro_L2}}).
#' @param phase_wnd numeric, specify the window length (in hours) used to
#'   identify local maxima and minima. A shorter window length leads to the
#'   identification of more local maxima and minima.
#' @param plot_phase logical, specify whether identified phases should be
#'   plotted.
#' @param agg_daily logical, specify whether phase statistics are aggregated
#'   by day (\code{agg_daily = TRUE}) or appended to the \code{L2} data
#'   (\code{agg_daily = FALSE}).
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @return The following additional variables are returned by
#'   \code{phase_stats}:
#'     \item{day}{day on which the respective phase ends (format
#'       \code{\%Y-\%m-\%d}.}
#'     \item{doy}{day of year on which the respective phase ends.}
#'     \item{shrink_start/exp_start}{timestamp of the start of the shrinkage
#'       or expansion phase.}
#'     \item{shrink_end/exp_end}{timestamp of the end of the shrinkage or
#'       expansion phase.}
#'     \item{shrink_dur/exp_dur}{duration (minutes) of the shrinkage or
#'       expansion phase.}
#'     \item{shrink_amp/exp_amp}{amplitude (\code{µm}) of the shrinkage or
#'       expansion phase.}
#'     \item{shrink_slope/exp_slope}{slope (\code{µm * reso^-1})
#'       of the shrinkage or expansion phase.}
#'     \item{phase_class}{days are classified as \code{1} if a shrinkage occurs
#'       during the day, and as \code{-1} if there is an expansion during the
#'       day. On all other days phase_class is set to NA.}
#'
#'   In case data is not aggregated to daily values
#'   (\code{agg_daily = FALSE}), all columns are appended to the input data.
#'   All parameters related to shrinkage or expansion phases are pasted at
#'   the timestamp corresponding to the end of the respective phases
#'   (\code{shrink_end} or \code{exp_end}). All other rows are set to
#'   \code{NA}.
#'
#' @details The identification of local maxima and minima in the function
#'   \code{phase_stats} is inspired by the functionality of
#'   \code{\link[dendrometeR]{phase_def}} in the package \code{dendrometeR}.
#'   Overlapping sets of time windows are used to identify local maxima and
#'   minima, i.e. maxima or minima that appear in both overlapping time windows
#'   and are not only bound to the start or the end of the respective time
#'   windows.
#'
#'   The often reported parameter maximum daily shrinkage (`mds`) can be
#'   extracted from the output parameters. `mds` is equal to `shrink_amp` in
#'   case `shrink_dur < 1440` (i.e. the shrinkage is shorter than 24 hours).
#'   Example code on how to extract `mds` can be found in the vignette
#'   \href{../doc/Introduction-to-treenetproc.html}{\code{vignette("Introduction-to-treenetproc", package = "treenetproc")}}.
#'
#' @export
#'
#' @examples
#' # Subset dataset for example
#' library(dplyr)
#' data_L2 <- dendro_data_L2 %>%
#'    filter(series == "site-1_dendro-2")
#'
#' phase_stats(dendro_L2 = data_L2, plot_phase = TRUE, plot_export = FALSE)
#'
phase_stats <- function(dendro_L2, phase_wnd = 8, plot_phase = FALSE,
                        plot_export = TRUE, agg_daily = TRUE, tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  df <- dendro_L2
  check_data_L2(data_L2 = df)
  reso <- reso_check_L1(df = df, tz = tz)
  if (reso > 360) {
    message("the time resolution of the dataset ('reso') is too coarse to
            calculate phase statistics")
    return(df)
  }


  # Calculate phase statistics ------------------------------------------------
  if ("phase_class" %in% colnames(df)) {
    df <- df %>%
      dplyr::select_if(!grepl("phase|shrink|exp", colnames(.)))
  }

  series_vec <- unique(df$series)
  list_phase <- vector("list", length = length(series_vec))
  df_phase <- df
  for (s in 1:length(series_vec)) {
    message(paste0("calculating phase statistics for ", series_vec[s], "..."))
    df <- df_phase %>%
      dplyr::filter(series == series_vec[s])

    maxmin1 <- findmaxmin(df = df, phase_wnd = phase_wnd, reso = reso, st = 1)
    maxmin2 <- findmaxmin(df = df, phase_wnd = phase_wnd, reso = reso, st = 2)

    max_wnd <- maxmin1[[1]] + maxmin2[[1]]
    min_wnd <- maxmin1[[2]] + maxmin2[[2]]

    max1 <- removeconsec(df = df, remove = max_wnd, notremove = min_wnd,
                         mode = "max") %>%
      dplyr::mutate(max1 = rem) %>%
      dplyr::select(-rem)
    min1 <- removeconsec(df = df, remove = min_wnd, notremove = max_wnd,
                         mode = "min") %>%
      dplyr::mutate(min1 = rem) %>%
      dplyr::select(-rem)

    maxmin <- dplyr::full_join(max1, min1, by = "ts") %>%
      dplyr::arrange(ts) %>%
      dplyr::mutate(extrema = ifelse(is.na(max1), "min", "max")) %>%
      dplyr::select(ts, extrema) %>%
      # set first and last row to a maximum
      dplyr::slice(which(extrema == "max")[1]:dplyr::n()) %>%
      dplyr::slice(1:dplyr::last(which(extrema == "max"))) %>%
      dplyr::mutate(shrink_group = c(rep(1:floor(dplyr::n() / 2), each = 2),
                                     NA)) %>%
      dplyr::mutate(exp_group = dplyr::lag(shrink_group))

    # delete extreme values that follow an NA or which are followed by an NA
    value_leadlag <- df %>%
      dplyr::mutate(value_lag = dplyr::lag(value)) %>%
      dplyr::mutate(value_lead = dplyr::lead(value)) %>%
      dplyr::select(ts, value, value_lag, value_lead)
    maxmin <- maxmin %>%
      dplyr::left_join(., value_leadlag, by = "ts") %>%
      dplyr::mutate(max_na = ifelse(is.na(value_lag), 1, 0)) %>%
      dplyr::mutate(max_na = max_na + ifelse(is.na(value_lead), 1, 0)) %>%
      dplyr::mutate(min_na = ifelse(is.na(value_lag), 1, 0)) %>%
      dplyr::mutate(min_na = min_na + ifelse(is.na(value_lead), 1, 0)) %>%
      dplyr::filter(min_na == 0 & max_na == 0) %>%
      dplyr::select(ts, extrema, shrink_group, exp_group)

    # calculate parameters for shrinkage and expansion
    shrink <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "shrink",
                                 phase_wnd = phase_wnd, tz = tz)
    exp <- calcshrinkexpparam(df = df, maxmin = maxmin, mode = "exp",
                              phase_wnd = phase_wnd, tz = tz)

    shrink_exp <- dplyr::full_join(shrink, exp, by = c("day", "doy")) %>%
      # classify days as transpir (1) or inverted (-1)
      dplyr::mutate(shrink_start_day =
                      as.POSIXct(substr(shrink_start, 1, 10), tz = tz)) %>%
      dplyr::mutate(exp_start_day =
                      as.POSIXct(substr(exp_start, 1, 10), tz = tz)) %>%
      dplyr::mutate(day_shrink = dplyr::if_else(shrink_start_day == day, 1, 0,
                                                missing = 0)) %>%
      dplyr::mutate(day_exp = dplyr::if_else(exp_start_day == day, -1, 0,
                                             missing = 0)) %>%
      dplyr::group_by(day) %>%
      dplyr::mutate(phase_class = sum(max(day_shrink), min(day_exp))) %>%
      dplyr::mutate(phase_class = ifelse(phase_class == 1 & shrink_dur > 720,
                                         0, phase_class)) %>%
      dplyr::mutate(phase_class = ifelse(phase_class == -1 & exp_dur > 720,
                                         0, phase_class)) %>%
      dplyr::mutate(phase_class = ifelse(phase_class == 0,
                                         NA, phase_class)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-shrink_start_day, -exp_start_day, -day_shrink,
                    -day_exp) %>%
      # set phases with 0 slopes to NA (almost straight lines),
      # classified in calcshrinkexpparam
      dplyr::mutate(shrink_slope = ifelse(shrink_slope == 0,
                                          NA, shrink_slope)) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs = list(~ replace(., is.na(shrink_slope), NA))) %>%
      dplyr::mutate(exp_slope = ifelse(exp_slope == 0, NA, exp_slope)) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ replace(., is.na(exp_slope), NA))) %>%
      # set phases with 0 duration to NA (too short durations)
      # classified in calcshrinkexpparam
      dplyr::mutate(shrink_dur = ifelse(shrink_dur == 0, NA, shrink_dur)) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs = list(~ replace(., is.na(shrink_dur), NA))) %>%
      dplyr::mutate(exp_dur = ifelse(exp_dur == 0, NA, exp_dur)) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ replace(., is.na(exp_dur), NA))) %>%
      # remove phase_class for removed phases
      dplyr::mutate(
        phase_class = ifelse(is.na(shrink_start) & phase_class == 1,
                             NA, phase_class)) %>%
      dplyr::mutate(
        phase_class = ifelse(is.na(exp_start) & phase_class == -1,
                             NA, phase_class)) %>%
      # overwrite duplicated shrinkage or expansion phases with NA
      dplyr::mutate(shrink_start =
                      replace(shrink_start, duplicated(shrink_start), NA)) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs = list(~ replace(., is.na(shrink_start), NA))) %>%
      dplyr::mutate(exp_start =
                      replace(exp_start, duplicated(exp_start), NA)) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ replace(., is.na(exp_start), NA))) %>%
      # rearrange rows to reduce rows with NAs
      dplyr::group_by(day) %>%
      dplyr::mutate_at(.vars = grep("shrink", colnames(.)),
                       .funs =
                         list(~ c(na.omit(.),
                                  rep(NA, sum(is.na(shrink_start)))))) %>%
      dplyr::mutate_at(.vars = grep("exp", colnames(.)),
                       .funs = list(~ c(na.omit(.),
                                        rep(NA, sum(is.na(exp_start)))))) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!(is.na(shrink_start) & is.na(exp_start))) %>%
      dplyr::arrange(day) %>%
      dplyr::mutate(series = series_vec[s]) %>%
      dplyr::select(series, day, doy, shrink_start, shrink_end, shrink_dur,
                    shrink_amp, shrink_slope, exp_start, exp_end, exp_dur,
                    exp_amp, exp_slope, phase_class)

    list_phase[[s]] <- shrink_exp

    if (plot_phase) {
      writeLines("plot phases...")
      plot_phase(df = df, phase = shrink_exp, plot_export = plot_export)
    }
  }

  phase <- dplyr::bind_rows(list_phase)

  if (!agg_daily) {
  # merge phases with df
  shrink <- phase %>%
    dplyr::select(series, shrink_start, shrink_end, shrink_dur, shrink_amp,
                  shrink_slope) %>%
    dplyr::mutate(ts = shrink_end) %>%
    dplyr::filter(!is.na(shrink_start))

  exp <- phase %>%
    dplyr::select(series, exp_start, exp_end, exp_dur, exp_amp, exp_slope) %>%
    dplyr::mutate(ts = exp_end) %>%
    dplyr::filter(!is.na(exp_start))

  phase_class <- phase %>%
    dplyr::select(series, day, phase_class) %>%
    dplyr::select(series, ts = day, phase_class) %>%
    dplyr::distinct()

  phase <- dplyr::left_join(df, shrink, by = c("series", "ts")) %>%
    dplyr::left_join(., exp, by = c("series", "ts")) %>%
    dplyr::left_join(., phase_class, by = c("series", "ts")) %>%
    dplyr::arrange(series, ts)
  }

  return(phase)
}
