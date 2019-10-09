#' Plot Command to Plot L2 Dendrometer Data
#'
#' \code{plotting_proc_L2} contains the code necessary to plot \code{L2}
#'   dendrometer data.
#'
#' @param plot_add logical, specify whether \code{L1} data should be plotted
#'   along with \code{L2} data in the second panel of the plot.
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plotting_proc_L2 <- function(data_L1, data_L2, diff, deleted,
                             plot_period, plot_add = TRUE,
                             plot_frost = TRUE, plot_interpol = TRUE, tz) {

  # define axis labels
  axis_labs <- axis_labels_period(df = data_L2, plot_period = plot_period,
                                  tz = tz)

  # plot ----------------------------------------------------------------------
  graphics::layout(mat = matrix(c(1, 2, 3, 4), nrow = 4),
                   heights = c(2, 1.6, 1, 2), widths = 1)

  # plot data_L1
  graphics::par(mar = c(0, 5, 4.1, 2.1))
  graphics::plot(data = data_L1, value ~ ts, type = "l", xaxt = "n", ylab = "",
                 las = 1, main = passobj("sensor_label"))
  graphics::title(ylab = "L1", mgp = c(3.5, 1, 0))

  # plot data_L2
  graphics::par(mar = c(0, 5, 0, 2.1))
  graphics::plot(data = data_L2, value ~ ts, type = "n", xaxt = "n", ylab = "",
                 las = 1)
  if (plot_frost) {
    if (plot_period %in% c("yearly", "monthly")) {
      plot_frost_period(data_L2 = data_L2)
    }
  }
  if (plot_add) {
    graphics::lines(data = data_L1, value ~ ts, col = "grey70")
  }
  graphics::lines(data = data_L2, value ~ ts, col = "#08519c")
  if (plot_interpol) {
    if (plot_period %in% c("yearly", "monthly")) {
      plot_interpol_points(data_L2 = data_L2)
    }
  }
  graphics::title(ylab = "L2", mgp = c(3.5, 1, 0))

  # plot diff
  graphics::par(mar = c(0, 5, 0, 2.1))
  options(warn = -1)
  graphics::plot(data = data_L2, value ~ ts, type = "n", xlab = "", log = "y",
                 yaxt = "n", xaxt = "n", ylab = "", ylim = c(0.1, 1200),
                 las = 1)
  graphics::abline(h = c(0.1, 1, 10, 100, 1000), col = "grey70")
  graphics::lines(data = deleted, deleted ~ ts, type = "h", lwd = 1,
                  col = "#fcdcd9")
  graphics::lines(data = diff, diff_old ~ ts, type = "h", lwd = 2,
                  col = "grey70")
  graphics::lines(data = diff, diff_plot ~ ts, type = "h", lwd = 2,
                  col = "#ef3b2c")
  if (plot_period == "monthly") {
    graphics::text(x = diff$ts,
                   y = rep(c(10, 3, 1, 0.3), length.out = nrow(diff)),
                   labels = diff$diff_nr_old, font = 2, col = "grey40")
    graphics::text(x = diff$ts,
                   y = rep(c(0.3, 1, 3, 10), length.out = nrow(diff)),
                   labels = diff$diff_nr, font = 2)
  }
  graphics::axis(2, at = c(0.1, 1, 10, 100, 1000),
                 labels = c(0, 1, 10, 100, 1000), las = 1)
  graphics::title(ylab = "difference [L1 - L2]", mgp = c(3.5, 1, 0))
  options(warn = 0)

  # plot twd
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
axis_labels_period <- function(df, plot_period, tz) {

  if (plot_period == "full") {
    ticks <- paste0(unique(df$year), "-01-01")
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 1, 4)
  }
  if (plot_period == "yearly") {
    ticks <- paste0(unique(df$year), "-", unique(df$month), "-01")
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 6, 7)
  }
  if (plot_period == "monthly") {
    ticks <- unique(substr(df$ts, 1, 10))
    ticks <- as.POSIXct(ticks, format = "%Y-%m-%d", tz = tz)
    labs <- substr(ticks, 9, 10)
  }

  axis_labs <- list(ticks, labs)

  return(axis_labs)
}


#' Plot Yearly Growth Curves and Print Variables
#'
#' \code{plot_gro_yr_print_vars} plots yearly growth curves for the whole
#'   period in a single plot and prints the used variables for plotting.
#'
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plot_gro_yr_print_vars <- function(data_L1, data_L2, tz, print_vars) {

  #data_L2 <- visp_L2_single %>%
  #  dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz))
  #tz <- "UTC"


  graphics::layout(mat = matrix(c(1, 2), nrow = 2), heights = c(2, 4.6),
                   widths = 1)

  # plot yearly growth curves
  graphics::par(mar = c(0, 5, 4.1, 2.1))

  data_L2_plot <- data_L2 %>%
    dplyr::mutate(doy = as.numeric(strftime(ts, format = "%j", tz = tz)) - 1)

  graphics::plot(data = data_L2_plot, gro_yr ~ doy, type = "n",
                 main = passobj("sensor_label"), ylab = "gro_yr",
                 xlab = "day of year", xlim = c(0, 365),
                 ylim = c(0, max(data_L2$gro_yr, na.rm = TRUE)), las = 1)

  years <- unique(data_L2_plot$year)
  colors <- rainbow(length(years))
  data_L2_year <- data_L2_plot %>%
    dplyr::group_by(year) %>%
    dplyr::group_split()
  for (y in 1:length(years)) {
    lines(data = data_L2_year[[y]], gro_yr ~ doy, col = colors[y])
  }
  legend(x = "topleft", legend = years, col = colors, bty = "n",
         lty = 1, seg.len = 0.8)


  # print used variables and threshold values
  if (print_vars) {
    graphics::par(mar = c(5.1, 5.1, 4.1, 2.1))

    graphics::plot(x = c(0, 1), y = c(0, 1), ann = FALSE, bty = "n",
                   type = "n", xaxt = "n", yaxt = "n")
    # print input variables
    graphics::text(x = 0, y = 1, adj = c(0, 1), font = 2, cex = 0.8,
                   labels = "input variables")
    graphics::text(x = 0, y = 0.97, adj = c(0, 1), cex = 0.8,
                   labels = paste0("tol_jump = ",
                                   passobj("tol_jump_plot"), "\n",
                                   "tol_out = ",
                                   passobj("tol_out_plot"), "\n",
                                   "frost_thr = ",
                                   passobj("frost_thr_plot"), "\n",
                                   "lowtemp = ",
                                   passobj("lowtemp_plot"), "\n",
                                   "interpol = ",
                                   passobj("interpol_plot"), "\n",
                                   "frag_len = ",
                                   passobj("frag_len_plot") *
                                     passobj("reso"), "\n",
                                   "tz = ", passobj("tz_plot")))
    # print applied thresholds and values
    graphics::text(x = 0.3, y = 1, adj = c(0, 1), font = 2, cex = 0.8,
                   labels = "applied thresholds and values")
    graphics::text(x = 0.3, y = 0.97, adj = c(0, 1), cex = 0.8,
                   labels = paste0("tol_jump = ",
                                   passobj("thr_jump_plot")[1], " / ",
                                   passobj("thr_jump_plot")[2], "\n",
                                   "tol_out = ",
                                   passobj("thr_out_plot")[1], " / ",
                                   passobj("thr_out_plot")[2], "\n",
                                   "tol_jump_frost = ",
                                   passobj("thr_jump_plot")[1] *
                                     passobj("frost_thr_plot"), " / ",
                                   passobj("thr_jump_plot")[2] *
                                     passobj("frost_thr_plot"), "\n",
                                   "tol_out_frost = ",
                                   passobj("thr_out_plot")[1] *
                                     passobj("frost_thr_plot"), " / ",
                                   passobj("thr_out_plot")[2] *
                                     passobj("frost_thr_plot"), "\n",
                                   "interpol = ",
                                   passobj("interpol_plot"), " min\n",
                                   "frag_len = ",
                                   passobj("frag_len_plot"), " min\n"))
    # print amount of missing, deleted and interpolated data
    list_missing <- calcmissing(data_L1 = data_L1, data_L2 = data_L2)
    graphics::text(x = 0.8, y = 1, adj = c(0, 1), font = 2, cex = 0.8,
                   labels = "changes in data")
    graphics::text(x = 0.8, y = 0.97, adj = c(0, 1), cex = 0.8,
                   labels = paste0("interpolated: ", list_missing[[1]], "%\n",
                                   "deleted: ", list_missing[[2]], "%\n",
                                   "missing: ", list_missing[[3]], "%"))
    # print package version
    version_pck <- utils::packageDescription("treenetproc",
                                             fields = "Version", drop = TRUE)
    graphics::text(x = 1, y = 0.6, adj = c(1, 1), cex = 0.8,
                   labels = paste0("treenetproc: ", version_pck))
  }

}


#' Plot Frost Period
#'
#' \code{plot_frost_period} draws a horizontal line in periods of possible
#'   frost, i.e. when the temperature < \code{lowtemp}.
#'   This function is exported for its use in vignettes only.
#'
#' @inheritParams plot_proc_L2
#'
#' @export
#' @keywords internal
#'
plot_frost_period <- function(data_L2) {

  if (sum(data_L2$frost, na.rm = TRUE) > 0) {
    x0 <- data_L2 %>%
      dplyr::mutate(frost_group = cumsum(frost)) %>%
      dplyr::filter(frost == TRUE) %>%
      dplyr::group_by(frost_group) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(ts)
    x0 <- x0$ts

    x1 <- data_L2 %>%
      dplyr::mutate(frost_group = cumsum(frost)) %>%
      dplyr::filter(frost == TRUE) %>%
      dplyr::group_by(frost_group) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(ts)
    x1 <- x1$ts

    y0 <- min(data_L2$value, na.rm = TRUE) + 0.02 *
      (max(data_L2$value, na.rm = TRUE) - min(data_L2$value, na.rm = TRUE))

    for (s in 1:length(x0)) {
      graphics::segments(x0 = x0[s], y0, x1 = x1[s], y1 = y0, col = "#1ac4c4")
    }
  }
}


#' Plot Interpolated Points
#'
#' \code{plot_interpol_points} adds points on top of line graph to show
#'   points that were interpolated.
#'
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plot_interpol_points <- function(data_L2) {

  interpol <- grep("fill", data_L2$flags)
  if (length(interpol) > 0) {
    graphics::points(x = data_L2$ts[interpol], y = data_L2$value[interpol],
                     col = "#08519c", pch = 1, cex = 1.2)
  }
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
plot_mds <- function(df, maxmin, plot_export) {
  df <- df %>%
    dplyr::mutate(month = as.numeric(cut(ts, breaks = "month",
                                         labels = FALSE)))

  series <- unique(df$series)
  if (plot_export) {
    grDevices::pdf(paste0("mds_plot_", series, ".pdf"),
                   width = 8.3, height = 5.8)
  }
  for (s in 1:length(series)) {
    df_series <- df %>%
      dplyr::filter(series == series[s])

    for (m in 1:max(df_series$month)) {
      df_plot <- df_series %>%
        dplyr::filter(month == m)

      if (sum(!is.na(df_plot$value)) != 0) {
        graphics::plot(x = df_plot$ts, y = df_plot$value, type = "l",
                       xlab = substr(df_plot$ts[1], 1, 7), ylab = "value",
                       main = df_plot$series[1])
        graphics::points(x = maxmin$ts, y = maxmin$max1, pch = 1)
        graphics::points(x = maxmin$ts, y = maxmin$min1, pch = 2)
      } else {
        next
      }
    }
  }
  if (plot_export) {
    grDevices::dev.off()
  }
}


#' Plot Command to Plot L1 Data
#'
#' \code{plotting_L1} plots \code{L1} data.
#'
#' @param data_L1_orig uncorrected original \code{L1} data. If specified, it
#'   is plotted behind the corrected \code{L1} data.
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plotting_L1 <- function(data_L1, data_L1_orig, plot_period, tz) {

  if (length(data_L1_orig) != 0) {
    plot_add <- TRUE
  } else {
    plot_add <- FALSE
  }

  # define axis labels
  axis_labs <- axis_labels_period(df = data_L1, plot_period = plot_period,
                                  tz = tz)

  # plot
  graphics::plot(data = data_L1, value ~ ts, type = "n",
                 xlab = passobj("year_label"),
                 ylab = "L1", xaxt = "n", las = 1,
                 main = passobj("sensor_label"))
  if (plot_add) {
    graphics::lines(data = data_L1_orig, value ~ ts, col = "grey70")
  }
  graphics::lines(data = data_L1, value ~ ts, col = "#08519c")
  graphics::axis(1, at = axis_labs[[1]], labels = axis_labs[[2]])
}


#' Plot density of differences
#'
#' \code{plot_density} plots the density of the value differences between
#'   two time stamps. In addition, the threshold values used to classify
#'   outliers are shown.
#'
#' @param df input \code{data.frame}.
#' @param ran numeric, range of rows in df considered for the density plot.
#'   Compatible with different window sizes for data processing.
#' @param low numeric, low threshold defining outliers. Inherited of the
#'   function \code{\link{calcflagmad}}.
#' @param high numeric, high threshold defining outliers. Inherited of the
#'   function \code{\link{calcflagmad}}.
#' @param limit_val numeric, defines the x-axis limits of the density plot.
#'   The x-axis limits are drawn at \code{limit_val * threshold}. Threshold
#'   values are inherited of the function \code{\link{calcflagmad}}.
#' @param plot_export logical, defines whether plots are exported or shown
#'   directly in the console.
#' @inheritParams proc_dendro_L2
#'
#' @return Plots are saved to current working directory as
#'   \code{density_plot.pdf}.
#'
#' @keywords internal
#'
plot_density <- function(df, low, high, limit_val = 20, frost_thr,
                         reso) {

  series <- unique(df$series)[1]
  df_plot <- df %>%
    dplyr::mutate(diff_val = c(NA, diff(value)) * (60 / reso))

  graphics::plot(stats::density(x = df_plot$diff_val, na.rm = TRUE),
                 xlim = c(limit_val * low, limit_val * high),
                 main = paste(series, "\n", substr(df_plot$ts[1], 1, 10),
                              "to", substr(dplyr::last(df_plot$ts), 1, 10)))

  graphics::rug(x = df_plot$diff_val[df$frost == FALSE], col = "black",
                quiet = TRUE)
  graphics::rug(x = df_plot$diff_val[df$frost == TRUE], col = "#73bfbf",
                side = 3, quiet = TRUE)
  graphics::abline(v = low, col = "#9c2828")
  graphics::abline(v = high, col = "#9c2828")
  graphics::abline(v = low * frost_thr, col = "#1ac4c4")
  graphics::abline(v = high * frost_thr, col = "#1ac4c4")
}
