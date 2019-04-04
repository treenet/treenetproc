#' Plot Command to Plot Dendrometer Data
#'
#' \code{plot_proc()} contains the code necessary to plot dendormeter data.
#'
#' @param ... additional parameters.
#' @inheritParams plot_dendro
#'
#' @keywords internal
#'
#' @examples
#'
plot_proc <- function(data_L1, data_L2, diff, add) {

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
  graphics::plot(data = data_L2, twd ~ ts, type = "l",
                 xlab = passobj("year_label"),  ylab = "", las = 1,
                 col = "#7a0177")
  graphics::title(ylab = "twd", mgp = c(3.5, 1, 0))

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
