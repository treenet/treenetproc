#' Plot L1 Data
#'
#' \code{plot_L1} plots \code{L1} data divided into specified periods. Can be
#'   used for both dendrometer and climate data.
#'
#' @param data_L1_orig optional, used in \code{\link{corr_dendro_L1}}. Can be
#'   used to plot a previous version of \code{data_L1} in the background.
#' @inheritParams plot_proc_L2
#' @inheritParams proc_L1
#'
#' @return Plots are saved to current working directory as
#'   \code{L1_plot.pdf} or as specified in \code{plot_name}.
#'
#' @export
#'
#' @examples
#' plot_L1(data_L1 = dendro_data_L1, plot_period = "monthly",
#'         plot_export = FALSE)
#'
plot_L1 <- function(data_L1, data_L1_orig = NULL, plot_period = "full",
                    plot_export = TRUE, plot_name = "L1_plot",
                    tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data ----------------------------------------------------------
  check_data_L1(data_L1 = data_L1)


  # Plot L1 data --------------------------------------------------------------
  data_L1 <- data_L1 %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::mutate(day = strftime(ts, format = "%d", tz = tz))

  if (length(data_L1_orig) != 0) {
    data_L1_orig <- data_L1_orig %>%
      dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
      dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
      dplyr::mutate(day = strftime(ts, format = "%d", tz = tz))
  }
  sensors <- unique(data_L1$series)
  years <- unique(data_L1$year)

  if (plot_export) {
    grDevices::pdf(paste0(plot_name, ".pdf"), width = 11.7, height = 8.3)
  }
  for (s in 1:length(sensors)) {
    sensor_label <- sensors[s]
    passenv$sensor_label <- sensor_label

    data_L1_sensor <- data_L1 %>%
      dplyr::filter(series == sensor_label)
    if (length(data_L1_orig) != 0) {
      data_L1_orig_sensor <- data_L1_orig %>%
        dplyr::filter(series == sensor_label)
    } else {
      data_L1_orig_sensor <- NULL
    }

    if (plot_period %in% c("yearly", "monthly")) {

      for (y in 1:length(years)) {
        year_label <- years[y]
        passenv$year_label <- year_label

        data_L1_year <- data_L1_sensor %>%
          dplyr::filter(year == year_label)
        if (length(data_L1_orig) != 0) {
          data_L1_orig_year <- data_L1_orig_sensor %>%
            dplyr::filter(year == year_label)
        } else {
          data_L1_orig_year <- NULL
        }

        if (plot_period == "yearly") {
          if (sum(!is.na(data_L1_year$value)) != 0) {

            plotting_L1(data_L1 = data_L1_year,
                        data_L1_orig = data_L1_orig_year,
                        plot_period = plot_period, tz = tz)
            } else {
            next
            }
        }

        if (plot_period == "monthly") {
          months <- unique(data_L1_year$month)

          for (m in 1:length(months)) {
            month_label <- months[m]
            year_label <- paste0(years[y], "-", months[m])
            passenv$year_label <- year_label

            data_L1_month <- data_L1_year %>%
              dplyr::filter(month == month_label)
            if (length(data_L1_orig) != 0) {
              data_L1_orig_month <- data_L1_orig_year %>%
                dplyr::filter(month == month_label)
            } else {
              data_L1_orig_month <- NULL
            }

            if (sum(!is.na(data_L1_month$value)) != 0) {

              plotting_L1(data_L1 = data_L1_month,
                          data_L1_orig = data_L1_orig_month,
                          plot_period = plot_period, tz = tz)
              } else {
              next
              }
          }
        }
      }
    }

    if (plot_period == "full") {
      year_label <- paste0(years[1], "-", years[length(years)])
      passenv$year_label <- year_label

      if (sum(!is.na(data_L1_sensor$value)) != 0) {

        plotting_L1(data_L1 = data_L1_sensor,
                    data_L1_orig = data_L1_orig_sensor,
                    plot_period = plot_period, tz = tz)
        } else {
        next
        }
    }
  }
  if (plot_export) {
    grDevices::dev.off()
  }
}
