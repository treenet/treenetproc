#' Plot Command to Plot L2 Dendrometer Data
#'
#' \code{plotting_proc_L2} contains the code necessary to plot \code{L2}
#'   dendrometer data.
#'
#' @param data_plot input \code{data.frame} containing both \code{L1} and
#'   \code{L2} data as well as changes in \code{L2} for plotting.
#' @param plot_add logical, specify whether \code{L1} data should be plotted
#'   along with \code{L2} data in the second panel of the plot.
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plotting_proc_L2 <- function(data_plot, plot_period, plot_add = TRUE,
                             plot_frost = TRUE, plot_interpol = TRUE, tz) {

  # define axis labels
  axis_labs <- axis_labels_period(df = data_plot, plot_period = plot_period,
                                  tz = tz)

  # plot ----------------------------------------------------------------------
  graphics::layout(mat = matrix(c(1, 2, 3, 4), nrow = 4),
                   heights = c(2, 1.6, 1, 2), widths = 1)

  # plot data_L1
  graphics::par(mar = c(0, 5, 4.1, 2.1))
  graphics::plot(data = data_plot, value_L1 ~ ts, type = "l", xaxt = "n",
                 ylab = "", las = 1, main = passobj("sensor_label"))
  graphics::title(ylab = "L1", mgp = c(3.5, 1, 0))

  # plot data_L2
  graphics::par(mar = c(0, 5, 0, 2.1))
  graphics::plot(data = data_plot, value_L2 ~ ts, type = "n", xaxt = "n",
                 ylab = "", las = 1)
  if (plot_frost) {
    if (plot_period %in% c("yearly", "monthly")) {
      plot_frost_period(df = data_plot)
    }
  }
  if (plot_add) {
    graphics::lines(data = data_plot, value_L1 ~ ts, col = "grey70")
  }
  graphics::lines(data = data_plot, value_L2 ~ ts, col = "#08519c")
  if (plot_interpol) {
    if (plot_period %in% c("yearly", "monthly")) {
      plot_interpol_points(df = data_plot)
    }
  }
  graphics::title(ylab = "L2", mgp = c(3.5, 1, 0))

  # plot diff
  graphics::par(mar = c(0, 5, 0, 2.1))
  options(warn = -1)
  graphics::plot(data = data_plot, value_L2 ~ ts, type = "n", xlab = "", log = "y",
                 yaxt = "n", xaxt = "n", ylab = "", ylim = c(0.1, 1200),
                 las = 1)
  graphics::abline(h = c(0.1, 1, 10, 100, 1000), col = "grey70")
  graphics::lines(data = data_plot, diff_old ~ ts, type = "h", lwd = 1.5,
                  col = "grey70")
  # different colors for deleted outliers and changes in values
  graphics::lines(data = data_plot, diff_plot ~ ts, type = "h", lwd = 2,
                  col = ifelse(grepl("out", data_plot$flags),
                               "#fcdcd9", "#ef3b2c"))
  if (plot_period == "monthly") {
    graphics::text(x = data_plot$ts,
                   y = rep(c(10, 3, 1, 0.3), length.out = nrow(data_plot)),
                   labels = data_plot$diff_nr_old, col = "grey40", font = 1)
    graphics::text(x = data_plot$ts,
                   y = rep(c(0.3, 1, 3, 10), length.out = nrow(data_plot)),
                   labels = data_plot$diff_nr, font = 2,
                   col = ifelse(grepl("out", data_plot$flags),
                                "#594f4f", "#802018"))
  }
  graphics::axis(2, at = c(0.1, 1, 10, 100, 1000),
                 labels = c(0, 1, 10, 100, 1000), las = 1)
  graphics::title(ylab = "difference [L1 - L2]", mgp = c(3.5, 1, 0))
  options(warn = 0)

  # plot twd
  graphics::par(mar = c(4.1, 5, 0, 2.1))
  graphics::plot(data = data_plot, twd ~ ts, type = "l", xaxt = "n",
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
#'   period in a single plot and prints the used input variables for plotting.
#'   In addition, median, maximum and minimum growth values for different
#'   periods are printed.
#'
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
plot_gro_yr_print_vars <- function(data_plot, print_vars, tz) {

  graphics::layout(mat = matrix(c(1, 2), nrow = 2), heights = c(2, 4),
                   widths = 1)

  # plot yearly growth curves
  graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

  data_plot <- data_plot %>%
    dplyr::mutate(doy = as.numeric(strftime(ts, format = "%j", tz = tz)) - 1)

  graphics::plot(data = data_plot, gro_yr ~ doy, type = "n",
                 main = passobj("sensor_label"), ylab = "gro_yr",
                 xlab = "day of year", xlim = c(0, 365),
                 ylim = c(0, max(data_plot$gro_yr, na.rm = TRUE)), las = 1)

  years <- unique(data_plot$year)
  colors <- c("grey", grDevices::rainbow(length(years)))
  data_year <- data_plot %>%
    dplyr::group_by(year) %>%
    dplyr::group_split()
  for (y in 1:length(years)) {
    graphics::lines(data = data_year[[y]], gro_yr ~ doy, col = colors[y])
  }
  graphics::legend(x = "topleft", legend = years, col = colors, bty = "n",
                   lty = 1, seg.len = 0.8)


  # print used variables and threshold values
  if (print_vars) {
    graphics::par(mar = c(5.1, 4.1, 4.1, 2.1))

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
                                   passobj("frag_len_plot"), "\n",
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
                                   passobj("frag_len_plot"), "\n"))
    # print amount of missing, deleted and interpolated data
    list_missing <- calcmissing(data_plot = data_plot)
    graphics::text(x = 0.8, y = 1, adj = c(0, 1), font = 2, cex = 0.8,
                   labels = "changes in data")
    graphics::text(x = 0.8, y = 0.97, adj = c(0, 1), cex = 0.8,
                   labels = paste0("interpolated: ", list_missing[[1]], "%\n",
                                   "deleted: ", list_missing[[2]], "%\n",
                                   "missing: ", list_missing[[3]], "%"))
    # print growth values for different periods
    gro_period <- calcgroperiods(df = data_plot, reso = passobj("reso"),
                                 tz = tz)
    if (length(gro_period) > 0) {
      graphics::text(x = 0, y = 0.7, adj = c(0, 1), font = 2, cex = 0.8,
                     labels = "growth statistics: median (min / max)")
      for (r in 1:nrow(gro_period)) {
        gro_period_single <- gro_period[r, ]
        graphics::text(x = 0, y = 0.7 - 0.03 * r, adj = c(0, 1), cex = 0.8,
                       labels = paste0(gro_period_single$period, ": ",
                                       gro_period_single$gro_med, " (",
                                       gro_period_single$gro_min, " / ",
                                       gro_period_single$gro_max, ")"))
      }
    }
    # print package version
    version_pck <- utils::packageDescription("treenetproc",
                                             fields = "Version", drop = TRUE)
    graphics::text(x = 1, y = 0.4, adj = c(1, 1), cex = 0.8,
                   labels = paste0("treenetproc: ", version_pck))
  }
}


#' Plot Frost Period
#'
#' \code{plot_frost_period} draws a horizontal line in periods of possible
#'   frost, i.e. when the temperature < \code{lowtemp}.
#'   This function is exported for its use in vignettes only.
#'
#' @param df input \code{data.frame}
#'
#' @export
#' @keywords internal
#'
plot_frost_period <- function(df) {

  if (sum(df$frost, na.rm = TRUE) > 0) {
    x0 <- df %>%
      dplyr::mutate(frost_group = cumsum(frost)) %>%
      dplyr::filter(frost == TRUE) %>%
      dplyr::group_by(frost_group) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup() %>%
      dplyr::select(ts)
    x0 <- x0$ts

    x1 <- df %>%
      dplyr::mutate(frost_group = cumsum(frost)) %>%
      dplyr::filter(frost == TRUE) %>%
      dplyr::group_by(frost_group) %>%
      dplyr::slice(dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::select(ts)
    x1 <- x1$ts

    y0 <- min(df$value_L2, na.rm = TRUE) + 0.02 *
      (max(df$value_L2, na.rm = TRUE) - min(df$value_L2,
                                                 na.rm = TRUE))

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
#' @param df input \code{data.frame}
#'
#' @keywords internal
#'
plot_interpol_points <- function(df) {

  interpol <- grep("fill", df$flags)
  if (length(interpol) > 0) {
    graphics::points(x = df$ts[interpol], y = df$value_L2[interpol],
                     col = "#08519c", pch = 1, cex = 1.2)
  }
}


#' Plot Cycles
#'
#' \code{plot_cycle} plots maxima and minima selected to calculate cycle
#'   statistics and prints some characteristics of the cycles.
#'
#' @param df input \code{data.frame}.
#' @param cycle \code{data.frame} containing cycle statistics on the shrinkage
#'   phase, refilling phase and full cycle (as produced by the function
#'   \code{calccycle}.
#'
#' @return Plots are saved to current working directory as
#'   \code{cycle_plot.pdf}.
#'
#' @keywords internal
#'
plot_cycle <- function(df, cycle, plot_export) {

  series <- unique(df$series)
  if (plot_export) {
    grDevices::pdf(paste0("cycle_plot_", series, ".pdf"),
                   width = 8.3, height = 5.8)
  }

  for (c in 1:nrow(cycle)) {
    cycle_plot <- cycle[c, ]

    if (is.na(cycle_plot$cycle_dur)) {
      next
    }

    plot_start <- cycle_plot$shrink_start - as.difftime(8, units = "hours")
    plot_end <- cycle_plot$ref_end + as.difftime(8, units = "hours")

    df_plot <- df %>%
      dplyr::filter(ts >= plot_start & ts <= plot_end)

    graphics::plot(x = df_plot$ts, y = df_plot$value, type = "l",
                   xaxt = "n", las = 1, ylab = "value",
                   xlab = paste("Time (Hours)\n",
                                as.Date(cycle_plot$shrink_start), "to",
                                as.Date(cycle_plot$ref_end)),
                   main = paste0(df_plot$series[1], "\n", "Cycle ",
                                 cycle$cycle[c]))
    graphics::axis.POSIXct(1, x = df_plot$ts, format = "%H")

    graphics::points(x = cycle_plot$shrink_start,
                     y = df_plot$value[df_plot$ts == cycle_plot$shrink_start])
    graphics::points(x = cycle_plot$ref_end,
                     y = df_plot$value[df_plot$ts == cycle_plot$ref_end])
    graphics::points(x = cycle_plot$shrink_end,
                     y = df_plot$value[df_plot$ts == cycle_plot$shrink_end],
                     pch = 2)

    graphics::legend(x = "bottomright",
                     legend = c(paste("Cycle duration =",
                                      cycle_plot$cycle_dur_class),
                                paste("Cycle class =", cycle_plot$cycle_class),
                                paste("Shrink amp =",
                                      round(cycle_plot$shrink_amp, 2)),
                                paste("Shrink slope =",
                                      round(cycle_plot$shrink_slope, 4)),
                                paste("Refill amp =",
                                      round(cycle_plot$ref_amp, 2)),
                                paste("Refill slope =",
                                      round(cycle_plot$ref_slope, 4))),
                     bty = "n")
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
                 xlab = "", ylab = "Density",
                 main = paste(series, "\n", substr(df_plot$ts[1], 1, 10),
                              "to", substr(dplyr::last(df_plot$ts), 1, 10)))

  graphics::rug(x = df_plot$diff_val[df$frost == FALSE], col = "#9c2828",
                quiet = TRUE)
  graphics::rug(x = df_plot$diff_val[df$frost == TRUE], col = "#73bfbf",
                side = 3, quiet = TRUE)
  graphics::abline(v = low, col = "#9c2828")
  graphics::abline(v = high, col = "#9c2828")
  graphics::abline(v = low * frost_thr, col = "#73bfbf")
  graphics::abline(v = high * frost_thr, col = "#73bfbf")
}
