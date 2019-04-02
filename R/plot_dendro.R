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
#'   period (\code{period = "full"}), for each year separately
#'   (\code{period = "yearly"}) or for each month (\code{period = "monthly"}).
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

  #data_L1 <- lens_L1 %>%
  #  dplyr::filter(series != "airtemperature")
  #data_L2 <- lens_L2
  #period <- "yearly"
  #tz <- "Etc/GMT-1"
  #add <- TRUE


  # Check input variables -----------------------------------------------------
  if (!(period %in% c("full", "yearly", "monthly"))) {
    stop("period needs to be either 'full', 'yearly' or 'monthly'.")
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
  #if (period %in% c("full", "yearly")) {
  #  group_vars <- c("year", "month", "week")
  #}
  #if (period == "monthly") {
    group_vars <- c("year", "month", "day")
  #}

  data_L1 <- data_L1 %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::mutate(day = strftime(ts, format = "%d", tz = tz)) %>%
    dplyr::mutate(week = strftime(ts, format = "%W", tz = tz))
  data_L2 <- data_L2 %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::mutate(day = strftime(ts, format = "%d", tz = tz)) %>%
    dplyr::mutate(week = strftime(ts, format = "%W", tz = tz))
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
      dplyr::select(series, ts, value_L1, value_L2 = value, year,
                    month, day, week) %>%
      dplyr::group_by_at(group_vars) %>%
      dplyr::mutate(
        value_L1_zero = value_L1 - value_L1[which(!is.na(value_L1))[1]]) %>%
      dplyr::mutate(
        value_L2_zero = value_L2 - value_L2[which(!is.na(value_L2))[1]]) %>%
      dplyr::summarise(
        diff = abs(dplyr::last(value_L1_zero[which(!is.na(value_L1_zero))]) -
                     dplyr::last(value_L2_zero[which(!is.na(value_L2_zero))]))) %>%
      dplyr::ungroup()

    #if (period %in% c("full", "yearly")) {
    #  diff_sensor <- diff_sensor %>%
    #    dplyr::mutate(ts = as.POSIXct(paste0(year, "-", month, "-01"),
    #                                  format = "%Y-%m-%d", tz = tz))
    #}
    #if (period == "monthly") {
      diff_sensor <- diff_sensor %>%
        dplyr::mutate(ts = as.POSIXct(paste0(year, "-", month, "-", day),
                                      format = "%Y-%m-%d", tz = tz))
    #}

    # Plot L1, L2 and weekly diff ---------------------------------------------
    if (period %in% c("yearly", "monthly")) {

      for (y in 1:length(years)) {
        year_label <- years[y]
        passenv$year_label <- year_label
        data_L1_year <- data_L1_sensor %>%
          dplyr::filter(year == year_label)
        data_L2_year <- data_L2_sensor %>%
          dplyr::filter(year == year_label)
        diff_L1_L2 <-
          data_L1_year$value[which(!is.na(data_L1_year$value))[1]] -
          data_L2_year$value[which(!is.na(data_L2_year$value))[1]]
        data_L2_year$value <- data_L2_year$value + diff_L1_L2
        diff_year <- diff_sensor %>%
          dplyr::filter(year == year_label)

        if (period == "yearly") {
          if (sum(!is.na(data_L1_year$value)) != 0 &
              sum(!is.na(data_L2_year$value)) != 0) {
            plot_proc(data_L1 = data_L1_year, data_L2 = data_L2_year,
                      diff = diff_year, add = add)
          } else {
            next
          }
        }

        if (period == "monthly") {
          months <- unique(data_L1_year$month)

          for (m in 1:length(months)) {
            month_label <- months[m]
            year_label <- paste0(years[y], "-", months[m])
            passenv$year_label <- year_label
            data_L1_month <- data_L1_year %>%
              dplyr::filter(month == month_label)
            data_L2_month <- data_L2_year %>%
              dplyr::filter(month == month_label)
            diff_L1_L2 <-
              data_L1_month$value[which(!is.na(data_L1_month$value))[1]] -
              data_L2_month$value[which(!is.na(data_L2_month$value))[1]]
            data_L2_month$value <- data_L2_month$value + diff_L1_L2
            diff_month <- diff_year %>%
              dplyr::filter(month == month_label)

            if (sum(!is.na(data_L1_month$value)) != 0 &
                sum(!is.na(data_L2_month$value)) != 0) {
              plot_proc(data_L1 = data_L1_month, data_L2 = data_L2_month,
                        diff = diff_month, add = add)
            } else {
              next
            }
          }
        }
      }
    }

    if (period == "full") {
      year_label <- paste0(years[1], "-", years[length(years)])
      passenv$year_label <- year_label

      if (sum(!is.na(data_L1_sensor$value)) != 0 &
          sum(!is.na(data_L2_sensor$value)) != 0) {
        plot_proc(data_L1 = data_L1_sensor, data_L2 = data_L2_sensor,
                  diff = diff_sensor, add = add)
      } else {
        next
      }
    }
  }
  dev.off()
}
