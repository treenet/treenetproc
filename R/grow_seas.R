#' Calculate the Start and End of the Growing Season
#'
#' \code{grow_seas} returns the day of year at which growth starts
#'   or ends. Values are returned starting from the second year only, since
#'   the calculation depends on the previous year (see Details for further
#'   information on the calculation).
#'
#' @param tol_seas numeric, defines the amount of yearly growth
#'   that needs to be surpassed before \code{gro_start} is defined.
#'   Likewise, \code{1 - tol_seas} is the amount of yearly growth at which
#'   \code{gro_end} is defined.
#' @param agg_yearly logical, specify whether the output is aggregated by year
#'   (\code{agg_yearly = TRUE}) or appended to the \code{L2} data
#'   (\code{agg_yearly = FALSE}).
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#' @inheritParams phase_stats
#'
#' @export
#'
#' @details \code{gro_start} is defined as the day of year at which the
#'   maximum dendrometer value of the previous year is surpassed.
#'   \code{gro_end} is defined as the day of year at which the maximum
#'   dendrometer value is reached.
#'
#'   To reduce the influence of potential remaining outliers on
#'   \code{gro_start} and \code{gro_end}, an adjustable tolerance
#'   \code{tol_seas} value is used to define growth start and cessation.
#'   That is, by default \code{gro_start} represents the day of year at which
#'   5% of yearly growth is surpassed. Likewise, \code{gro_end} represents the
#'   day of year at which 95% of yearly growth is reached. See Knüsel et al.
#'   (2020) for a graphical example.
#'
#' @return The following variables are returned by \code{grow_seas}:
#'     \item{series}{name of the dendrometer series}
#'     \item{year}{year}
#'     \item{gro_start}{day of year at which growth starts}
#'     \item{gro_end}{day of year at which growth ends}
#'
#'   In case data is not aggregated to yearly values
#'   (\code{agg_yearly = FALSE}), all columns are appended to the output data.
#'   The values of \code{gro_start} and \code{gro_end} are only pasted at the
#'   first timestamp of the year, all other rows are set to \code{NA}.
#'
#' @references Knüsel S., Haeni M., Wilhelm M., Peters R.L., Zweifel R. 2020.
#'   treenetproc - An R package to clean, process and visualise dendrometer
#'   data. In preparation.
#'
#' @examples
#' grow_seas(dendro_L2 = dendro_data_L2)
#'
grow_seas <- function(dendro_L2, tol_seas = 0.05, agg_yearly = TRUE,
                      tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  df <- dendro_L2
  check_data_L2(data_L2 = df)
  reso <- reso_check_L1(df = df, tz = tz)


  # Calculate growing season --------------------------------------------------
  if ("gro_start" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_start)
  }
  if ("gro_end" %in% colnames(df)) {
    df <- df %>%
      dplyr::select(-gro_end)
  }

  series_vec <- unique(df$series)
  list_seas <- vector("list", length = length(series_vec))
  df_seas <- df
  for (s in 1:length(series_vec)) {
    df <- df_seas %>%
      dplyr::filter(series == series_vec[s]) %>%
      dplyr::mutate(year = as.numeric(substr(ts, 1, 4)))

    # find complete years
    complete_yrs <- df %>%
      dplyr::group_by(year) %>%
      dplyr::summarise(n_days = difftime(dplyr::last(ts), ts[1], tz = tz,
                                         units = "days")) %>%
      dplyr::ungroup() %>%
      dplyr::filter(n_days > 362) %>%
      dplyr::select(year) %>%
      unlist(use.names = FALSE)

    grow_seas <- df %>%
      dplyr::group_by(year) %>%
      dplyr::mutate(gro_tot = max(gro_yr, na.rm = T)) %>%
      dplyr::mutate(gro_start_tol = tol_seas * gro_tot) %>%
      dplyr::mutate(gro_end_tol = (1 - tol_seas) * gro_tot) %>%
      dplyr::mutate(
        gro_start = ts[dplyr::first(which(gro_yr > gro_start_tol))]) %>%
      dplyr::mutate(
        gro_start = as.numeric(strftime(gro_start, format = "%j"))) %>%
      dplyr::mutate(
        gro_end = ts[dplyr::last(which(gro_yr < gro_end_tol))]) %>%
      dplyr::mutate(
        gro_end = as.numeric(strftime(gro_end, format = "%j"))) %>%
      dplyr::summarise(ts = ts[1],
                       gro_start = gro_start[1],
                       gro_end = gro_end[1]) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year != is.na(year)) %>%
      # remove first year (since results depend on values of previous year)
      dplyr::slice(-1) %>%
      # remove incomplete years
      dplyr::filter(year %in% complete_yrs) %>%
      dplyr::mutate(series = series_vec[s]) %>%
      dplyr::select(series, ts, gro_start, gro_end)

    if (agg_yearly) {
      grow_seas <- grow_seas %>%
        dplyr::mutate(year = as.numeric(substr(ts, 1, 4))) %>%
        dplyr::arrange(series, year) %>%
        dplyr::select(series, year, gro_start, gro_end)
    }

    list_seas[[s]] <- grow_seas
  }

  df <- dplyr::bind_rows(list_seas)

  if (!agg_yearly) {
    if (length(df) == 0) {
      df <- df_seas %>%
        dplyr::mutate(gro_start = NA) %>%
        dplyr::mutate(gro_end = NA)

      return(df)
    }
    df <- dplyr::full_join(df_seas, df, by = c("series", "ts")) %>%
      dplyr::arrange(series, ts)
  }

  if (length(df) == 0) {
    stop("All series were too short to calculate growth start and end")
  }

  return(df)
}
