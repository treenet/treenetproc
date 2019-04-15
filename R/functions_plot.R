#' Plot Command to Plot L2 Dendrometer Data
#'
#' \code{plotting_proc_L2} contains the code necessary to plot \code{L2}
#'   dendrometer data.
#'
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
#' @examples
#'
plotting_proc_L2 <- function(data_L1, data_L2, diff, period, add, tz) {

  # define axis labels
  axis_labs <- axis_labels_period(df = data_L2, period = period, tz = tz)

  # plot
  graphics::layout(matrix(c(1, 2, 3, 4), nrow = 4), heights = c(2, 1.6, 1, 2),
                   widths = 1)
  graphics::par(mar = c(0, 5, 4.1, 2.1))
  graphics::plot(data = data_L1, value ~ ts, type = "l", xaxt = "n", ylab = "",
                 las = 1, main = passobj("sensor_label"))
  graphics::title(ylab = "L1", mgp = c(3.5, 1, 0))
  graphics::par(mar = c(0, 5, 0, 2.1))
  graphics::plot(data = data_L2, value ~ ts, type = "n", xaxt = "n", ylab = "",
                 las = 1)
  if (add) {
    graphics::lines(data = data_L1, value ~ ts, col = "grey70")
  }
  graphics::lines(data = data_L2, value ~ ts, col = "#08519c")
  graphics::title(ylab = "L2", mgp = c(3.5, 1, 0))
  graphics::par(mar = c(0, 5, 0, 2.1))
  options(warn = -1)
  graphics::plot(data = diff, diff ~ ts, type = "n", xlab = "", log = "y",
                 yaxt = "n", xaxt = "n", ylab = "", ylim = c(0.1, 1200),
                 las = 1)
  graphics::abline(h = c(0.1, 1, 10, 100, 1000), col = "grey70")
  graphics::lines(data = diff, diff ~ ts, type = "h", lwd = 2, col = "#b30000")
  graphics::axis(2, at = c(0.1, 1, 10, 100, 1000),
                 labels = c(0, 1, 10, 100, 1000), las = 1)
  graphics::title(ylab = "log(diff[L1 - L2])", mgp = c(3.5, 1, 0))
  options(warn = 0)
  graphics::par(mar = c(4.1, 5, 0, 2.1))
  graphics::plot(data = data_L2, twd ~ ts, type = "l", xaxt = "n",
                 xlab = passobj("year_label"),  ylab = "", las = 1,
                 col = "#7a0177")
  graphics::axis(1, at = axis_labs[[1]], labels = axis_labs[[2]])
  graphics::title(ylab = "twd", mgp = c(3.5, 1, 0))

}


#' Define Axis Labels Based on Period
#'
#' \code{axis_labels_period} defines axis ticks and labels based on the
#'   period used for plotting.
#'
#' @param df input \code{data.frame}
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
#' @examples
#'
axis_labels_period <- function(df, period, tz) {

  if (period == "full") {
    ticks <- paste0(unique(df$year), "-01-01")
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 1, 4)
  }
  if (period == "yearly") {
    ticks <- paste0(unique(df$year), "-", unique(df$month), "-01")
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 6, 7)
  }
  if (period == "monthly") {
    ticks <- unique(substr(df$ts, 1, 10))
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 9, 10)
  }

  axis_labs <- list(ticks, labs)

  return(axis_labs)
}


#' Plot Maxima and Minima of MDS
#'
#' \code{plot_mds} plots maxima and minima selected to calculate mds (maximum
#'   daily shrinkage). Plots are saved as
#'
#' @param df input \code{data.frame}.
#' @param maxmin \code{data.frame} containing maxima and minima used for
#'   the calculation of mds.
#'
#' @return Plots are saved to current working directory as
#'   \code{mds_plot.pdf}.
#'
#' @keywords internal
#'
#' @examples
#'
plot_mds <- function(df, maxmin) {
  df <- df %>%
    dplyr::mutate(month = as.numeric(cut(ts, breaks = "month",
                                         labels = FALSE)))

  series <- unique(df$series)
  grDevices::pdf(paste0("mds_plot_", series, ".pdf"),
                 width = 8.3, height = 5.8)
  for (s in 1:length(series)) {
    df_series <- df %>%
      dplyr::filter(series == series[s])

    for (m in 1:max(df_series$month)) {
      df_plot <- df_series %>%
        dplyr::filter(month == m)

      graphics::plot(x = df_plot$ts, y = df_plot$value, type = "l",
                     xlab = substr(df_plot$ts[1], 1, 7), ylab = "value",
                     main = df_plot$series[1])
      graphics::points(x = maxmin$ts, y = maxmin$max1, pch = 1)
      graphics::points(x = maxmin$ts, y = maxmin$min1, pch = 2)
    }
  }
  grDevices::dev.off()
}


#' Plot Command to Plot L1 Data
#'
#' \code{plotting_L1} contains the code necessary to plot \code{L1}
#'   data.
#'
#' @param data_L1_orig uncorrected original \code{L1} data. If specified, it
#'   is plotted behind the corrected \code{L1} data.
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
#' @examples
#'
plotting_L1 <- function(data_L1, data_L1_orig, period, tz) {

  if (length(data_L1_orig) != 0) {
    add <- TRUE
  } else {
    add <- FALSE
  }

  # define axis labels
  axis_labs <- axis_labels_period(df = data_L1, period = period, tz = tz)

  # plot
  graphics::plot(data = data_L1, value ~ ts, type = "n",
                 xlab = passobj("year_label"),
                 ylab = "L1", xaxt = "n", las = 1,
                 main = passobj("sensor_label"))
  if (add) {
    graphics::lines(data = data_L1_orig, value ~ ts, col = "grey70")
  }
  graphics::lines(data = data_L1, value ~ ts, col = "#08519c")
  graphics::axis(1, at = axis_labs[[1]], labels = axis_labs[[2]])
}
