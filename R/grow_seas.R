#' Calculate Start and End of Yearly Growth
#'
#' \code{grow_seas} returns the day of the year at which growth starts
#'   or ends.
#'
#' @param df input \code{data.frame}.
#' @param tol numeric, defines the amount of yearly growth that needs to be
#'   surpassed for \code{gro_start} to be defined. \code{1 - tol} is the
#'   amount of yearly growth at which \code{gro_end} is defined.
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
grow_seas <- function(df, tol = 0.05, tz) {

  if ("gro_start" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_start)
  }
  if ("gro_end" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_end)
  }

  nc <- ncol(df)
  gro_season <- df %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::group_by(year) %>%
    dplyr::mutate(gro_tot = sum(gro_yr, na.rm = T)) %>%
    dplyr::mutate(gro_start_tol = tol * gro_tot) %>%
    dplyr::mutate(gro_end_tol = (1 - tol) * gro_tot) %>%
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

  return(df)
}
