#' Plot Command to Plot Dendrometer Data
#'
#' \code{plot_command()} contains the code necessary to plot dendormeter data.
#'
#' @param ... further parameters.
#' @inheritParams plot_dendro
#'
#' @keywords internal
#'
#' @examples
#'
plot_command <- function(data_L1, data_L2, diff, ...) {

  layout(matrix(c(1, 2, 3, 4), nrow = 4), heights = c(2, 1.6, 1, 2),
         widths = 1)
  par(mar = c(0, 5, 4.1, 2.1))
  plot(data = data_L1, value ~ ts, type = "l", xaxt = "n", ylab = "",
       las = 1, main = sensor_label)
  title(ylab = "L1", mgp = c(3.5, 1, 0))
  par(mar = c(0, 5, 0, 2.1))
  plot(data = data_L2, value ~ ts, type = "n", xaxt = "n", ylab = "",
       las = 1)
  if (add) {
    lines(data = data_L1, value ~ ts, col = "grey70")
  }
  lines(data = data_L2, value ~ ts, col = "#08519c")
  title(ylab = "L2", mgp = c(3.5, 1, 0))
  par(mar = c(0, 5, 0, 2.1))
  options(warn = -1)
  plot(data = diff, diff ~ ts, type = "n", xlab = "", log = "y",
       yaxt = "n", xaxt = "n", ylab = "", ylim = c(0.1, 1200), las = 1)
  abline(h = c(0.1, 1, 10, 100, 1000), col = "grey70", lty = 2)
  lines(data = diff, diff ~ ts, type = "h", lwd = 2, col = "#b30000")
  axis(2, at = c(0.1, 1, 10, 100, 1000),
       labels = c(0, 1, 10, 100, 1000), las = 1)
  title(ylab = "log(diff[L1 - L2])", mgp = c(3.5, 1, 0))
  options(warn = 0)
  par(mar = c(4.1, 5, 0, 2.1))
  plot(data = data_L2, twd ~ ts, type = "l", xlab = year_label, ylab = "",
       las = 1, col = "#7a0177")
  title(ylab = "twd", mgp = c(3.5, 1, 0))

}
