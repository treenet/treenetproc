#' Plot Processed Dendrometer Data
#'
#' \code{plot_dendro()} provides plots of time-aligned (\code{L1}) and
#'   processed (\code{L2}) dendrometer data to visually assess the processing.
#'   The first panel shows the \code{L1} data and the second panel the
#'   processed \code{L2} data. The third panel shows the weekly difference
#'   between \code{L1} and \code{L2} data. Large differences without an
#'   apparent jump in the data indicate problems in the processing.
#'
#' @param data_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}.
#' @param data_L2 processed dendrometer data as produced by
#'   \code{\link{proc_dendro_L2}}.
#' @param period specify whether plots should be displayed over the whole
#'   period (\code{period = "full"}) or for each year separately
#'   (\code{period = "yearly"}).
#' @param add logical, specify whether \code{L1} data should be plotted along
#'   with \code{L2} data in the second panel of the plot.
#' @inheritParams proc_L1
#'
#' @return Plots are saved to current working directory as
#'   \code{processing_L2_plot.pdf}.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' plot_dendro(data_L1 = data_L1_dendro, data_L2 = data_L2_dendro,
#'             period = "yearly")
#' }
plot_dendro <- function(data_L1, data_L2, period = "full", tz = "Etc/GMT-1",
                        add = TRUE) {

  # Check input variables -----------------------------------------------------
  if (!(period %in% c("full", "yearly"))) {
    stop("period needs to be either 'full' or 'yearly'.")
  }
  if (!(add %in% c(TRUE, FALSE))) {
    stop("add can only be 'TRUE' or 'FALSE'.")
  }


  # Check input data ----------------------------------------------------------
  if (sum(colnames(data_L1) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned dendrometer data generated with 'proc_L1'.")
  }
  if (sum(colnames(data_L2) %in% c("series", "ts", "value", "version", "max",
                                   "twd")) != 6) {
    stop("provide processed dendrometer data generated with 'proc_dendro_L2'.")
  }


  # Calculate weekly difference -----------------------------------------------
  sensors <- unique(data_L1$series)
  data_L1 <- data_L1 %>%
    dplyr::mutate(year = substr(ts, 1, 4))
  data_L2 <- data_L2 %>%
    dplyr::mutate(year = substr(ts, 1, 4))
  years <- unique(data_L1$year)

  pdf("processing_L2_plot.pdf", width = 8.3, height = 11.7)
  for (s in 1:length(sensors)) {
    sensor_label <- sensors[s]
    passenv$sensor_label <- sensor_label
    data_L1_sensor <- data_L1 %>%
      dplyr::filter(series == sensor_label)
    data_L2_sensor <- data_L2 %>%
      dplyr::filter(series == sensor_label)

    diff_sensor <- data_L1_sensor %>%
      dplyr::select(series, ts, value_L1 = value) %>%
      dplyr::full_join(., data_L2_sensor, by = c("series", "ts")) %>%
      dplyr::select(series, ts, value_L1, value_L2 = value) %>%
      dplyr::mutate(year = substr(ts, 1, 4)) %>%
      dplyr::mutate(month = substr(ts, 6, 7)) %>%
      dplyr::mutate(day = substr(ts, 9, 10)) %>%
      dplyr::mutate(week = strftime(ts, format = "%W")) %>%
      dplyr::group_by(year, week) %>%
      dplyr::mutate(value_L1_zero =
                      value_L1 - value_L1[which(!is.na(value_L1))[1]]) %>%
      dplyr::mutate(value_L2_zero =
                      value_L2 - value_L2[which(!is.na(value_L2))[1]]) %>%
      dplyr::summarise(diff =
                         abs(dplyr::last(
                           value_L1_zero[which(!is.na(value_L1_zero))]) -
                             dplyr::last(
                               value_L2_zero[which(!is.na(value_L2_zero))])),
                       month = month[1],
                       day = day[1]) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(ts = as.POSIXct(paste0(year, "-", month, "-", day),
                                    format = "%Y-%m-%d", tz = tz))


    # Plot L1, L2 and weekly diff ---------------------------------------------
    if (period == "yearly") {

      for (y in 1:length(years)) {
        year_label <- years[y]
        passenv$year_label <- year_label
        data_L1_year <- data_L1_sensor %>%
          dplyr::filter(year == year_label)
        data_L2_year <- data_L2_sensor %>%
          dplyr::filter(year == year_label)
        diff_year <- diff_sensor %>%
          dplyr::filter(year == year_label)

        if (sum(!is.na(data_L1_year$value)) != 0 &
            sum(!is.na(data_L2_year$value)) != 0) {
          plot_command(data_L1 = data_L1_year, data_L2 = data_L2_year,
                       diff = diff_year, add = add)
        } else {
          next
        }
      }
    }

    if (period == "full") {
      year_label <- paste0(years[1], "-", years[length(years)])
      passenv$year_label <- year_label

      if (sum(!is.na(data_L1_sensor$value)) != 0 &
          sum(!is.na(data_L2_sensor$value)) != 0) {
        plot_command(data_L1 = data_L1_sensor, data_L2 = data_L2_sensor,
                     diff = diff_sensor, add = add)
      } else {
        next
      }
    }
  }
  dev.off()
}
