#' Calculate Start and End of the Growing Season
#'
#' \code{grow_seas} returns the day of year at which growth starts
#'   or ends. Values are returned starting from the second year, since their
#'   calculation depends on the previous year.
#'
#' @param tol_seas numeric, defines the amount of yearly growth that needs to be
#'   surpassed for \code{gro_start} to be defined. \code{1 - tol_seas} is the
#'   amount of yearly growth at which \code{gro_end} is defined.
#' @param agg_yearly logical, specify whether growth start and end are appended
#'   to the \code{L2} data or or are exported as a yearly aggregated
#'   \code{data.frame}.
#' @inheritParams proc_dendro_L2
#' @inheritParams phase_stats
#'
#' @export
#'
#' @return
#'
#' @examples
#'
grow_seas <- function(dendro_L2, tol_seas = 0.05, agg_yearly = FALSE,
                      tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  check_data_L2(data_L2 = df)
  reso <- reso_check_L1(df = df)


  # Calculate growing season --------------------------------------------------
  if ("gro_start" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_start)
  }
  if ("gro_end" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_end)
  }

  gro_season <- df %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(gro_tot = sum(gro_yr, na.rm = T)) %>%
    dplyr::mutate(gro_start_tol = tol_seas * gro_tot) %>%
    dplyr::mutate(gro_end_tol = (1 - tol_seas) * gro_tot) %>%
    dplyr::mutate(gro_sum = cumsum(ifelse(is.na(gro_yr), 0, gro_yr))) %>%
    dplyr::mutate(
      gro_start_ind = dplyr::first(which(gro_sum >= gro_start_tol))) %>%
    dplyr::mutate(
      gro_start = as.numeric(strftime(ts[gro_start_ind], format = "%j"))) %>%
    dplyr::mutate(
      gro_end_ind = dplyr::last(which(gro_sum <= gro_end_tol))) %>%
    dplyr::mutate(
      gro_end = as.numeric(strftime(ts[gro_end_ind], format = "%j"))) %>%
    dplyr::summarise(ts = ts[1],
                     gro_start = gro_start[1],
                     gro_end = gro_end[1]) %>%
    dplyr::ungroup() %>%
    dplyr::filter(year != is.na(year)) %>%
    # remove first year (since results depend on values of previous year)
    dplyr::slice(-1) %>%
    dplyr::select(ts, gro_start, gro_end)

  df <- df %>%
    dplyr::left_join(., gro_season, by = "ts")

  if (agg_yearly) {
    df <- df %>%
      dplyr::select(series, ts, gro_start, gro_end) %>%
      dplyr::filter(!is.na(gro_start) | !(is.na(gro_end))) %>%
      dplyr::mutate(year = as.numeric(substr(ts, 1, 4))) %>%
      dplyr::mutate(date = )
      dplyr::arrange(series, year) %>%
      dplyr::select(series, year, gro_start, gro_end)
  }

  return(df)
}
