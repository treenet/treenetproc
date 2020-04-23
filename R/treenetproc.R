#' treenetproc - Clean, process and visualize dendrometer data
#'
#' The \code{treenetproc} package contains three main functions:
#'   \code{proc_L1}, \code{proc_dendro_L2}, \code{corr_dendro_L2}
#'
#' @section \code{proc_L1()}:
#'   The function \code{proc_L1} aligns raw dendrometer and temperature
#'   measurements to regular time intervals.
#'
#' @section \code{proc_dendro_L2()}:
#'   The function \code{proc_dendro_L2} removes outliers and jumps or shifts
#'   in time-aligned dendrometer data.
#'
#' @section \code{corr_dendro_L2()}:
#'   The function \code{corr_dendro_L2} can be used to manually correct
#'   remaining errors after data cleaning with \code{proc_dendro_L2}.
#'
#' @section Vignettes:
#'   More information and some examples on how to use the functions can be
#'   found in the package vignette. To see the vignette use:\cr
#'   \code{browseVignettes("treenetproc")}\cr
#'   If \code{browseVignettes} does not work, the package may need to be
#'   reinstalled along with the vignette:\cr
#'   \code{library(devtools)}\cr
#'   \code{devtools::install_github("treenet/treenetproc",
#'   build_vignettes = TRUE))}
#'
#' @docType package
#' @name treenetproc
NULL

utils::globalVariables(c("series", "ts", "value", "version", "max", "twd",
                         "gro_yr", "frost", "flags", ".", ":=",
                         "Sensor_class", "Sensor_query", "Seriesname",
                         "Site", "Site_temp_ref", "amp", "cons", "cons_nr",
                         "day", "day_exp", "day_shrink", "diff_L1", "diff_L2",
                         "diff_gap", "diff_gro", "diff_jump", "diff_nona",
                         "diff_nr", "diff_nr_first", "diff_nr_last",
                         "diff_nr_old", "diff_nr_old_first",
                         "diff_nr_old_last", "diff_old", "diff_plot",
                         "diff_sum", "diff_ts", "diff_val", "doy", "dur",
                         "end", "exp_amp", "exp_dur", "exp_end", "exp_group",
                         "exp_slope", "exp_start", "exp_start_day", "extrema",
                         "fill_forw", "fill_rev", "flag", "flag_group",
                         "flag_len", "flag_nr", "flagdelete", "flagfill",
                         "flagfrag", "flagjump", "flagout", "flagouthigh",
                         "flagoutlow", "flagtemp", "frost_group", "gapflag",
                         "gaple", "gaple_mins", "gapnr", "gaps", "gro",
                         "gro_end", "gro_end_tol", "gro_start",
                         "gro_start_tol", "gro_tot", "group_reso", "id",
                         "iscons", "isgap", "jump_group", "jump_nr", "max_na",
                         "min_na", "month", "month_del", "month_forc",
                         "month_plot", "n_days", "na_prop", "norem", "rem",
                         "shrink_amp", "shrink_dur", "shrink_end",
                         "shrink_group", "shrink_slope", "shrink_start",
                         "shrink_start_day", "slope", "start", "time",
                         "unique_value", "uqk", "val_gap", "val_nona",
                         "value_L1", "value_L2", "value_lag", "value_lead",
                         "value_na", "y", "year", "z"))
