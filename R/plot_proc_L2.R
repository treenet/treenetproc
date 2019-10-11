#' Plot Processed Dendrometer Data
#'
#' \code{plot_proc_L2} provides plots of time-aligned (\code{L1}) and
#'   processed (\code{L2}) dendrometer data to visually assess the processing.
#'
#' @param data_L1 time-aligned dendrometer data as produced by
#'   \code{\link{proc_L1}}.
#' @param data_L2 processed dendrometer data as produced by
#'   \code{\link{proc_dendro_L2}}.
#' @param plot_period specify whether plots should be displayed over the whole
#'   plot_period (\code{plot_period = "full"}), for each year separately
#'   (\code{plot_period = "yearly"}) or for each month
#'   (\code{plot_period = "monthly"}).
#' @param plot_show specify whether all periods should be plotted
#'   (\code{plot_show = "all"}) or only those periods in which \code{L1} and
#'   \code{L2} dendrometer data differ after processing
#'   (\code{plot_show = "diff"}).
#' @param plot_name character, specify name of the PDF in which the plots are
#'   saved.
#' @param plot_export logical, specifies whether the plots are exported as a
#'   \code{PDF} file to the working directory or are plotted in the console.
#' @param print_vars logical, specifies whether used variables should be
#'   plotted in the first panel. Command is only used internally in the
#'   function \code{\link{proc_dendro_L2}}.
#' @inheritParams proc_L1
#'
#' @return Plots are saved in a PDF to current working directory as
#'   \code{proc_L2_plot.pdf} or as specified in \code{plot_name}.
#'
#'   The first panel shows the \code{L1} data, the second panel the
#'   processed \code{L2} data with the \code{L1} data in the background. The
#'   third panel shows the absolute value of the differences between \code{L1}
#'   and \code{L2} data. Large differences without an apparent jump in the
#'   data indicate problems in the processing.
#'
#' @export
#'
#' @examples
#' plot_proc_L2(data_L1 = dendro_data_L1, data_L2 = dendro_data_L2,
#'             plot_period = "yearly", plot_export = FALSE)
#'
plot_proc_L2 <- function(data_L1, data_L2, plot_period = "full",
                         plot_show = "all", plot_export = TRUE,
                         plot_name = "proc_L2_plot", tz = "UTC",
                         print_vars = FALSE) {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Check input data -----------------------------------------------------
  check_data_L1(data_L1 = data_L1)
  check_data_L2(data_L2 = data_L2)


  # Calculate differences -----------------------------------------------------
  # add diff_old, used to plot removed differences (see corr_dendro_L3)
  if (!("diff_old" %in% colnames(data_L1))) {
    data_L1$diff_old <- NA
    data_L1$diff_nr_old <- NA
  }
  # add month_plot used to plot months with force or delete
  # (see corr_dendro_L3)
  if (!("month_plot" %in% colnames(data_L1))) {
    data_L1$month_plot <- 0
  }
  data_L1 <- data_L1 %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(diff_L1 = c(NA, diff(value, lag = 1))) %>%
    dplyr::mutate(value_L1 = value) %>%
    dplyr::ungroup()
  data_L2 <- data_L2 %>%
    dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
    dplyr::mutate(month = strftime(ts, format = "%m", tz = tz)) %>%
    dplyr::group_by(series) %>%
    dplyr::mutate(diff_L2 = c(NA, diff(value, lag = 1))) %>%
    dplyr::mutate(value_L2 = value) %>%
    dplyr::ungroup()
  deleted <- data_L2 %>%
    dplyr::select(ts, value_L2) %>%
    dplyr::left_join(., data_L1, by = "ts") %>%
    dplyr::mutate(deleted = ifelse(!is.na(value_L1) & is.na(value_L2),
                                   100, NA)) %>%
    dplyr::select(ts, year, month, series, deleted)

  sensors <- unique(data_L1$series)
  years <- unique(data_L1$year)

  if (plot_export) {
    grDevices::pdf(paste0(plot_name, ".pdf"), width = 8.3, height = 11.7)
  }
  for (s in 1:length(sensors)) {
    sensor_label <- sensors[s]
    passenv$sensor_label <- sensor_label
    data_L1_sensor <- data_L1 %>%
      dplyr::filter(series == sensor_label)
    data_L2_sensor <- data_L2 %>%
      dplyr::filter(series == sensor_label)
    deleted_sensor <- deleted %>%
      dplyr::filter(series == sensor_label)

    diff_sensor <- data_L1_sensor %>%
      dplyr::select(series, ts, value_L1, diff_L1, diff_old, diff_nr_old,
                    month_plot) %>%
      dplyr::full_join(., data_L2_sensor, by = c("series", "ts")) %>%
      dplyr::select(series, ts, value_L1, value_L2, diff_L1, diff_L2,
                    diff_old, diff_nr_old, month_plot, year, month) %>%
      dplyr::mutate(diff = diff_L1 - diff_L2) %>%
      dplyr::mutate(diff = ifelse(abs(diff) <= 0.1, 0, diff)) %>%
      dplyr::mutate(diff = ifelse(is.na(diff), 0, diff)) %>%
      dplyr::mutate(diff_plot = abs(diff)) %>%
      dplyr::mutate(diff_nr = 0) %>%
      dplyr::mutate(diff_nr = ifelse(diff != 0, 1, 0)) %>%
      dplyr::mutate(diff_nr = cumsum(diff_nr)) %>%
      dplyr::mutate(diff_nr = ifelse(diff == 0, NA, diff_nr))


    # Plot L1, L2 and daily diff ----------------------------------------------
    if (plot_period %in% c("yearly", "monthly")) {

      for (y in 1:length(years)) {
        year_label <- years[y]
        passenv$year_label <- year_label
        data_L1_year <- data_L1_sensor %>%
          dplyr::filter(year == year_label)
        data_L2_year <- data_L2_sensor %>%
          dplyr::filter(year == year_label)
        deleted_year <- deleted_sensor %>%
          dplyr::filter(year == year_label)
        diff_L1_L2 <-
          data_L1_year$value[which(!is.na(data_L1_year$value))[1]] -
          data_L2_year$value[which(!is.na(data_L2_year$value))[1]]
        data_L2_year$value <- data_L2_year$value + diff_L1_L2
        diff_year <- diff_sensor %>%
          dplyr::filter(year == year_label)

        if (plot_period == "yearly") {
          if (sum(!is.na(data_L1_year$value)) != 0 &
              sum(!is.na(data_L2_year$value)) != 0) {
            if (plot_show == "diff" &
                max(abs(diff_year$diff), na.rm = TRUE) < 0.1) {
              next
            }
            plotting_proc_L2(data_L1 = data_L1_year, data_L2 = data_L2_year,
                             diff = diff_year, deleted = deleted_year,
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
            data_L2_month <- data_L2_year %>%
              dplyr::filter(month == month_label)
            deleted_month <- deleted_year %>%
              dplyr::filter(month == month_label)
            diff_L1_L2 <-
              data_L1_month$value[which(!is.na(data_L1_month$value))[1]] -
              data_L2_month$value[which(!is.na(data_L2_month$value))[1]]
            data_L2_month$value <- data_L2_month$value + diff_L1_L2
            diff_month <- diff_year %>%
              dplyr::filter(month == month_label)

            if (sum(!is.na(diff_month$diff_nr)) != 0 |
                sum(!is.na(diff_month$diff_nr_old)) != 0) {

              diff_month <- diff_month %>%
                dplyr::mutate(day = strftime(ts, format = "%d", tz = tz)) %>%
                dplyr::group_by(year, month, day) %>%
                dplyr::mutate(
                  diff_nr_first = dplyr::first(diff_nr[which(!is.na(diff_nr))]),
                  diff_nr_last = dplyr::last(diff_nr[which(!is.na(diff_nr))]),
                  diff_nr_old_first =
                    dplyr::first(diff_nr_old[which(!is.na(diff_nr_old))]),
                  diff_nr_old_last =
                    dplyr::last(diff_nr_old[which(!is.na(diff_nr_old))])) %>%
                dplyr::mutate(
                  diff_nr = ifelse(diff_nr_first != diff_nr_last,
                                   paste0(diff_nr_first, "-", diff_nr_last),
                                   as.character(diff_nr_first)),
                  diff_nr_old = ifelse(diff_nr_old_first != diff_nr_old_last,
                                       paste0(diff_nr_old_first, "-",
                                              diff_nr_old_last),
                                       as.character(diff_nr_old_first))) %>%
                dplyr::ungroup() %>%
                dplyr::filter(diff != 0 & !is.na(diff_nr) |
                                diff_old != 0 & !is.na(diff_nr_old)) %>%
                # select middle value for positioning label in plot
                dplyr::group_by(year, month, day) %>%
                dplyr::slice(ceiling(dplyr::n() / 2)) %>%
                dplyr::ungroup() %>%
                dplyr::select(ts, diff_nr, diff_plot, diff_nr_old, diff_old,
                              month_plot)
            }

            if (sum(!is.na(data_L1_month$value)) != 0 &
                sum(!is.na(data_L2_month$value)) != 0) {
              if (plot_show == "diff" &
                  sum(!is.na(diff_month$diff_nr)) == 0) {
                next
              }
              if (plot_show == "diff_corr") {
                if (sum(!is.na(diff_month$diff_nr_old)) == 0 &
                    !(1 %in% diff_month$month_plot)) {
                  next
                }
              }

              plotting_proc_L2(data_L1 = data_L1_month,
                               data_L2 = data_L2_month,
                               diff = diff_month,
                               deleted = deleted_month,
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

      if (sum(!is.na(data_L1_sensor$value)) != 0 &
          sum(!is.na(data_L2_sensor$value)) != 0) {
        plotting_proc_L2(data_L1 = data_L1_sensor, data_L2 = data_L2_sensor,
                         diff = diff_sensor, deleted = deleted_sensor,
                         plot_period = plot_period, tz = tz)
      } else {
        next
      }
    }

    # Plot yearly growth and print variables  ---------------------------------
    plot_gro_yr_print_vars(data_L1 = data_L1_sensor, data_L2 = data_L2_sensor,
                           tz = tz, print_vars = print_vars)
  }
  if (plot_export) {
    grDevices::dev.off()
  }
}
