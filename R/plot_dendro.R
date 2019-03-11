#' Plot Processed Dendrometer Data
#'
#' \code{plot_dendro} provides plots of time-aligned (L1) and processed (L2)
#' dendrometer data to visually assess the processing. The first panel shows
#' the L1 data and the second panel the processed L2 data. The third panel
#' shows the weekly difference between L1 and L2 data. Large differences
#' without an apparent jump in the data indicate problems in the processing.
#'
#' @param data_L1 time-aligned dendrometer data as produced by \code{proc_L1}.
#'
#' @param data_L2 processed dendrometer data as produced by
#' \code{proc_dendro_L2}.
#'
#' @param period specify whether plots should be displayed over the whole
#' period (\code{"full"}) or for each year separately (\code{"yearly}).
#'
#' @param add specify whether L1 data should be plotted along with L2 data
#' in the second panel.
#'
#' @param tz time-zone of provided data. Default is \code{"Etc/GMT-1"}.
#'
#' @return
#' @export
#'
#' @examples
#'
plot_dendro <- function(data_L1, data_L2,
                        period = "full", add = FALSE,
                        tz = "Etc/GMT-1", ...) {

  data_L1 <- data_L1_dendro %>%
    dplyr::filter(series == unique(series)[1])
  data_L2 <- data_L2_dendro %>%
    dplyr::filter(series == unique(series)[1])
  add <- TRUE
  period <- "full"
  tz <- "Etc/GMT-1"


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
  diff <- data_L1 %>%
    dplyr::select(series, ts, value_L1 = value) %>%
    dplyr::full_join(., data_L2, by = c("series", "ts")) %>%
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


  # Plot L1, L2 and weekly diff -----------------------------------------------
  layout(matrix(c(1, 2, 3), nrow = 3), heights = c(2, 1.6, 1), widths = 1)
  par(mar = c(0, 5, 4.1, 2.1))
  plot(data = data_L1, value ~ ts, type = "l", xaxt = "n", ylab = "",
       las = 1)
  title(ylab = "value_L1", mgp = c(3.5, 1, 0))
  par(mar = c(0, 5, 0, 2.1))
  plot(data = data_L2, value ~ ts, type = "n", xaxt = "n", ylab = "", las = 1)
  if (add) {
    lines(data = data_L1, value ~ ts, col = "grey85")
  }
  lines(data = data_L2, value ~ ts, col = "#08519c")
  title(ylab = "value_L2", mgp = c(3.5, 1, 0))
  par(mar = c(4.1, 5, 0, 2.1))
  #options(warn = -1)
  plot(data = diff, diff ~ ts, type = "h", xlab = "", log = "y",
       yaxt = "n", ylab = "", ylim = c(0.1, 1200), las = 1,
       col = "#b30000")
  axis(2, at = c(0.1, 1, 10, 100, 1000),
       labels = c(0, 1, 10, 100, 1000), las = 1)
  title(ylab = "diff", mgp = c(3.5, 1, 0))
  #options(warn = 0)

}
