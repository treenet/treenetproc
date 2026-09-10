#' Clean Time-aligned Dendrometer Data
#'
#' \code{proc_dendro_L2} cleans time-aligned (\code{L1}) dendrometer data
#'   by removing outliers and correcting for erroneous jumps or shifts in
#'   the data.
#'
#' @param dendro_L1 \code{data.frame} with time-aligned dendrometer
#'   data. Output of function \code{\link{proc_L1}}.
#' @param temp_L1 \code{data.frame} with time-aligned temperature data.
#'   Output of function \code{\link{proc_L1}} (see Details for further
#'   information).
#' @param tol_jump numeric, defines the rigidity of the threshold above or
#'   below which a value is flagged for jump correction. Lower values
#'   increase the rigidity (see Details for further information).
#' @param tol_out numeric, defines the rigidity of the threshold above or
#'   below which a value is classified as an outlier. Lower values
#'   increase the rigidity (see Details for further information).
#' @param frost_thr numeric, increases the thresholds for outlier
#'   and jump detection in periods of probable frost (i.e. temperature <
#'   \code{lowtemp}). The thresholds are multiplied by the value provided.
#' @param lowtemp numeric, specifies the temperature in °C below which frost
#'   shrinkage or expansion is expected. Default value is set to
#'   \code{5°C} due to hysteresis shortly before or after frost events.
#' @param interpol numeric, length of gaps (in minutes) for which values are
#'   linearly interpolated after data cleaning. Set \code{interpol = 0} to
#'   disable gapfilling. If \code{interpol = NULL} the default value is set to
#'   \code{interpol = 2.1 * reso} (i.e. two timestamps).
#' @param frag_len numeric, specifies the length of data fragments occurring
#'   in-between missing data that are automatically deleted during data
#'   cleaning. This can be helpful to remove short fragments of erroneous data
#'   within a period of missing data, i.e. after jumps. If
#'   \code{frag_len = NULL} the default value is set to \code{frag_len = 2.1}.
#' @param plot logical, specify whether the changes that occurred during data
#'   cleaning should be plotted.
#' @param iter_clean numeric, specifies the number of times the cleaning
#'   process is repeated. Can be used to check whether running the cleaning
#'   process multiple times has an effect on the results. In most cases, a
#'   single iteration is sufficient.
#' @param prev_L2 \code{data.frame} of previously processed L2 dendrometer
#'   data (output of a prior call to \code{proc_dendro_L2}), used to
#'   correctly anchor \code{max} and \code{gro_yr} when processing an
#'   incremental update. The function determines which rows of
#'   \code{prev_L2} fall within the reprocessing window (controlled by
#'   \code{prev_L2_reprocess_days}) and re-cleans them together with the
#'   new \code{dendro_L1} data, then prepends the remaining unchanged
#'   \code{prev_L2} rows so the returned \code{data.frame} covers the
#'   full time series. Defaults to \code{NULL} (standard behaviour).
#' @param prev_L2_reprocess_days numeric, number of days from the end of
#'   \code{prev_L2} to include in re-cleaning together with the new
#'   \code{dendro_L1} data. Set to \code{0} to skip reprocessing entirely
#'   (the new data is simply appended after anchoring \code{max} and
#'   \code{gro_yr} from the last \code{prev_L2} row). The default
#'   (\code{NULL}) reprocesses the entire \code{prev_L2} together with
#'   the new data, which gives the most thorough cleaning but is slowest.
#'   Only used when \code{prev_L2} is supplied. Defaults to \code{NULL}.
#' @inheritParams proc_L1
#' @inheritParams plot_proc_L2
#'
#' @details Time-aligned temperature data \code{temp_L1} is used to define
#'   periods in which frost shrinkage is probable, e.g. when the temperature
#'   is below \code{lowtemp}. Without temperature data, frost shrinkages may be
#'   classified as outliers. For more details and an example see the following
#'   vignette:
#'   \href{../doc/Introduction-to-treenetproc.html}{\code{vignette("Introduction-to-treenetproc", package = "treenetproc")}}.
#'
#'   Temperature data can also be provided along with dendrometer data. In this
#'   case, the name of the temperature series has to contain the string
#'   \code{"temp"}. In case no temperature dataset is specified, a sample
#'   temperature dataset will be used with a warning. The sample temperature
#'   dataset assigns permanent frost to the three months December, January
#'   and February.
#'
#'   Outliers and jumps are identified when exceeding a lower or upper
#'   threshold. Thresholds are obtained on the basis of density distributions
#'   of differences between neighbouring data points. The rigidity of the
#'   thresholds can be controlled with the arguments \code{tol_jump} and
#'   \code{tol_out}. For more information on the calculation of the thresholds
#'   the user is referred to Knüsel et al. (2020, in prep).
#'
#' @return The function returns a \code{data.frame} with processed dendrometer
#'   data containing the following columns:
#'    \item{series}{name of the dendrometer series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value (\code{µm}).}
#'    \item{max}{highest measured value up to this timestamp (\code{µm}).}
#'    \item{twd}{tree water deficit (\code{µm}), i.e. the amount of stem
#'      shrinkage expressed as the difference between \code{max} and
#'      \code{value}.}
#'    \item{gro_yr}{growth since the beginning of the year (\code{µm}). Also
#'      calculated for years with missing data.}
#'    \item{frost}{indicates frost periods (i.e. periods in which the
#'      temperature is below \code{lowtemp}).}
#'    \item{flags}{character vector specifying the changes that occurred
#'      during the processing. For more details see the following vignette:
#'      \href{../doc/Introduction-to-treenetproc.html}{\code{vignette("Introduction-to-treenetproc", package = "treenetproc")}}}
#'    \item{version}{package version number.}
#'
#'   When \code{prev_L2} is supplied, the returned \code{data.frame} covers
#'   the full time series (unchanged \code{prev_L2} prefix + reprocessed/new
#'   rows), ready to replace the database table.
#'
#' @export
#'
#' @references Knüsel S., Haeni M., Wilhelm M., Peters R.L., Zweifel R. 2020.
#'   treenetproc: towards a standardized processing of stem radius data.
#'   In preparation.
#'
#' @examples
#' proc_dendro_L2(dendro_L1 = dendro_data_L1, plot_period = "monthly",
#'                plot_export = FALSE)
#'
proc_dendro_L2 <- function(dendro_L1, temp_L1 = list(), reso = 10,
                           tol_jump = 50, tol_out = 10, frost_thr = 5,
                           lowtemp = 5, interpol = 14400,
                           frag_len = 3, iter_clean = 1, plot = TRUE,
                           plot_period = "monthly", plot_show = "diff",
                           plot_export = TRUE, plot_name = "proc_L2_plot",
                           tz = "UTC",
                           prev_L2 = NULL,
                           prev_L2_reprocess_days = NULL) {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)


  # Validate prev_L2 / prev_L2_reprocess_days ---------------------------------
  if (!is.null(prev_L2)) {
    required_cols <- c("series_id", "ts", "value", "max", "twd", "gro_yr")
    if (!all(required_cols %in% colnames(prev_L2))) {
      stop("'prev_L2' must contain columns: ",
           paste(required_cols, collapse = ", "))
    }
    if (!inherits(prev_L2$ts, "POSIXct")) {
      prev_L2$ts <- as.POSIXct(prev_L2$ts, tz = tz)
    }
    if (!is.null(prev_L2_reprocess_days) &&
        (!is.numeric(prev_L2_reprocess_days) ||
         length(prev_L2_reprocess_days) != 1 ||
         prev_L2_reprocess_days < 0)) {
      stop("'prev_L2_reprocess_days' must be a single non-negative number.")
    }
  }


  # Save input variables for plotting -----------------------------------------
  if (plot) {
    passenv$tol_jump_plot <- tol_jump
    passenv$tol_out_plot <- tol_out
    passenv$frost_thr_plot <- frost_thr
    passenv$lowtemp_plot <- lowtemp
    passenv$tz_plot <- tz
  }


  # Check input data ----------------------------------------------------------
  df <- dendro_L1
  check_data_L1(data_L1 = df)

  if (length(temp_L1) != 0) {
    tem <- temp_L1
    tem_series <- unique(tem$series_id)

    if (length(grep("temp", tem_series, ignore.case = T)) > 1) {
      stop("provide single temperature dataset.")
    }
    if (sum(colnames(tem) %in% c("series_id", "ts", "value", "version")) != 4) {
      stop("provide time-aligned temperature data generated with 'proc_L1'")
    }

    # add column with temperature reference
    df$temp_ref <- tem_series
  }

  passenv$sample_temp <- FALSE
  if (length(temp_L1) == 0) {
    df_series <- unique(df$series_id)

    # for data from server
    if ("temp_ref" %in% colnames(df)) {
      temp_series <- stats::na.omit(unique(df$temp_ref))
      tem <- df %>%
        dplyr::filter(series_id %in% temp_series)
      df <- df %>%
        dplyr::filter(!(series_id %in% temp_series))
      dendro_L1 <- df
    }
    # for user-specified data
    if (!("temp_ref" %in% colnames(df))) {
      if (length(grep("temp", df_series, ignore.case = T)) > 1) {
        stop("provide single temperature dataset.")
      }
      if (length(grep("temp", df_series, ignore.case = T)) == 0) {
        tem <- create_temp_dummy(df = df)
        message("sample temperature dataset is used.")
        passenv$sample_temp <- TRUE
        df <- df %>%
          dplyr::mutate(temp_ref = "airtemperature")
      }
      if (length(grep("temp", df_series, ignore.case = T)) == 1) {
        temp_series <- df_series[grep("temp", df_series, ignore.case = T)]
        tem <- df %>%
          dplyr::filter(series_id == temp_series)
        df <- df %>%
          dplyr::filter(series_id != temp_series) %>%
          dplyr::mutate(temp_ref = temp_series)
        dendro_L1 <- df
      }
    }
  }

  reso_df <- reso_check_L1(df = df, tz = tz)
  reso_tem <- reso_check_L1(df = tem, tz = tz)
  if (reso_df != reso_tem) {
    stop("provide both dendrometer and temperature data at the same time ",
         "resolution.")
  } else {
    passenv$reso <- reso_df
  }

  # check for overlap between df and tem
  ts_overlap_check(df = df, tem = tem)


  # Process to L2 (jump and gap corrections) ----------------------------------
  series_vec <- unique(df$series_id)
  list_L2 <- vector("list", length = length(series_vec))
  list_thr <- vector("list", length = length(series_vec))
  df_L1 <- df
  for (s in 1:length(series_vec)) {
    writeLines(paste0("Processing ", series_vec[s], "..."))
    df <- df_L1 %>%
      dplyr::filter(series_id == series_vec[s])

    if (all(is.na(df$value))) {
      message(paste0("There is no data available for ", series_vec[s],
                     ". This series is skipped."))
      next
    }

    # Determine prev_L2 context for this series --------------------------------
    prev_unchanged <- NULL   # rows of prev_L2 NOT re-cleaned (prepended later)
    prev_anchor    <- NULL   # single anchor row just before the reprocess window
    reprocess_from_ts <- NULL

    if (!is.null(prev_L2)) {
      first_ts_new <- min(df$ts[!is.na(df$value)], na.rm = TRUE)

      prev_series_all <- prev_L2 %>%
        dplyr::filter(series_id == series_vec[s], !is.na(value)) %>%
        dplyr::arrange(ts)

      if (nrow(prev_series_all) > 0) {

        # Determine the start of the reprocessing window ----------------------
        if (is.null(prev_L2_reprocess_days)) {
          # Reprocess everything in prev_L2
          reprocess_from_ts <- min(prev_series_all$ts)
        } else if (prev_L2_reprocess_days == 0) {
          # Append only: no prev_L2 rows are reprocessed
          reprocess_from_ts <- first_ts_new
        } else {
          last_prev_ts <- max(prev_series_all$ts)
          reprocess_from_ts <- last_prev_ts -
            as.difftime(prev_L2_reprocess_days, units = "days")
        }

        # Rows before the window: kept unchanged
        prev_unchanged <- prev_L2 %>%
          dplyr::filter(series_id == series_vec[s],
                        ts < reprocess_from_ts) %>%
          dplyr::arrange(ts)

        # Single anchor: last unchanged row (provides max / gro_yr baseline)
        if (nrow(prev_unchanged) > 0) {
          prev_anchor <- prev_unchanged %>%
            dplyr::filter(!is.na(value)) %>%
            dplyr::slice_tail(n = 1)
        }

        # Re-process the anchor row together with the window. Without it the
        # window starts one sampling interval after the anchor and is then
        # re-levelled onto the anchor's value, which silently discards the real
        # stem change over that interval. The row is dropped again further down
        # because it is already carried in 'prev_unchanged'.
        include_anchor <- !is.null(prev_anchor) && nrow(prev_anchor) > 0 &&
          !(!is.null(prev_L2_reprocess_days) && prev_L2_reprocess_days == 0)
        align_from_ts <- if (include_anchor) prev_anchor$ts else reprocess_from_ts

        # Rows inside the window that need to be re-cleaned: convert back to L1
        # format (keep only columns present in df, drop derived L2 columns)
        prev_reprocess <- prev_L2 %>%
          dplyr::filter(series_id == series_vec[s],
                        ts >= align_from_ts,
                        ts < first_ts_new) %>%
          dplyr::arrange(ts)

        if (nrow(prev_reprocess) > 0) {
          shared_cols <- intersect(colnames(df), colnames(prev_reprocess))
          extra_cols  <- setdiff(colnames(df), colnames(prev_reprocess))
          prev_reprocess <- prev_reprocess %>%
            dplyr::select(dplyr::all_of(shared_cols))
          for (col in extra_cols) {
            prev_reprocess[[col]] <- NA
          }
          prev_reprocess <- prev_reprocess[, colnames(df)]
          df <- dplyr::bind_rows(prev_reprocess, df)
        }

        # If append-only (prev_L2_reprocess_days == 0), prepend a single stub
        # row (the last non-NA prev_L2 row) just for boundary jump detection,
        # and record it so we can remove it again after cleaning.
        if (!is.null(prev_L2_reprocess_days) && prev_L2_reprocess_days == 0) {
          anchor_row <- prev_series_all %>%
            dplyr::filter(ts < first_ts_new) %>%
            dplyr::slice_tail(n = 1)
          if (nrow(anchor_row) > 0) {
            stub <- anchor_row %>%
              dplyr::select(series_id, ts, value)
            shared_cols <- intersect(colnames(df), colnames(stub))
            extra_cols  <- setdiff(colnames(df), colnames(stub))
            stub <- stub %>%
              dplyr::select(dplyr::all_of(shared_cols))
            for (col in extra_cols) stub[[col]] <- NA
            stub <- stub[, colnames(df)]
            df <- dplyr::bind_rows(stub, df)
          }
        }
      }
    }

    # Timestamp marking the true start of new L1 data (used to strip stub row)
    first_ts_new <- min(
      df_L1 %>%
        dplyr::filter(series_id == series_vec[s]) %>%
        dplyr::pull(ts) %>%
        .[!is.na(.)],
      na.rm = TRUE
    )

    # remove leading and trailing NA's
    na_list <- remove_lead_trail_na(df = df)
    df <- na_list[[1]]
    lead_trail_na <- na_list[[2]]

    df <- createfrostflag(df = df, tem = tem, lowtemp = lowtemp,
                          sample_temp = passobj("sample_temp"))

    clean_list <- vector("list", length = iter_clean + 1)
    clean_list[[1]] <- df
    for (i in 1:iter_clean) {
      df <- clean_list[[i]]

      # remove outliers
      df <- calcdiff(df = df, reso = passobj("reso"))
      df <- createflagmad(df = df, reso = passobj("reso"), wnd = NULL,
                          tol = tol_out, save_thr = TRUE,
                          correction = "outlier", frost_thr = frost_thr)
      df <- executeflagout(df = df, len = 1, frag_len = frag_len,
                           plot_density = FALSE, plot_export = plot_export,
                           frost_thr = frost_thr)

      # remove jumps (jump correction)
      df <- calcdiff(df = df, reso = passobj("reso"))
      df <- createflagmad(df = df, reso = passobj("reso"), wnd = NULL,
                          tol = tol_jump, save_thr = TRUE,
                          correction = "jump", frost_thr = frost_thr)
      df <- createjumpflag(df = df)
      df <- executejump(df = df)

      clean_list[[i + 1]] <- df
    }
    df <- clean_list[[iter_clean + 1]]

    # If append-only: remove the prepended stub row ---------------------------
    if (!is.null(prev_L2) &&
        !is.null(prev_L2_reprocess_days) &&
        prev_L2_reprocess_days == 0) {
      df <- df %>%
        dplyr::filter(ts >= first_ts_new)
    }

    df <- fillintergaps(df = df, reso = passobj("reso"),
                        interpol = interpol, type = "linear", flag = TRUE)
    df <- calcmax(df = df)
    df <- calctwdgro(df = df, tz = tz)
    df <- summariseflags(df = df)

    # Re-anchor max and gro_yr using the last unchanged prev_L2 row -----------
    if (!is.null(prev_anchor) && nrow(prev_anchor) > 0) {
      prev_max  <- prev_anchor$max
      prev_gro  <- prev_anchor$gro_yr
      prev_val  <- prev_anchor$value

      # Re-level on the anchor timestamp itself when it was re-processed
      # (see 'include_anchor' above), otherwise on the first re-processed row.
      i_align <- which(df$ts == prev_anchor$ts & !is.na(df$value))[1]
      if (is.na(i_align)) {
        i_align <- which(!is.na(df$value))[1]
      }
      val_offset <- prev_val - df$value[i_align]

      df <- df %>%
        dplyr::mutate(
          value = ifelse(!is.na(value), value + val_offset, value),
          max   = ifelse(!is.na(max),   pmax(max + val_offset, prev_max), max),
          twd   = ifelse(!is.na(value), abs(value - max), twd)
        )

      # gro_yr must be the within-year increase of the CLAMPED max. Re-using the
      # gro_yr that calctwdgro() computed for the window and only shifting it by
      # prev_gro counts the recovery of the stem out of the water deficit that
      # was present at the anchor as new growth (up to twd at the anchor), and
      # leaves gro_yr inconsistent with max.
      anchor_year <- format(prev_anchor$ts, "%Y")
      df <- df %>%
        dplyr::mutate(year_ts = format(ts, "%Y")) %>%
        dplyr::group_by(year_ts) %>%
        dplyr::mutate(
          gro_yr = ifelse(is.na(value), NA_real_,
                          max - dplyr::first(stats::na.omit(max)))
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(
          gro_yr = ifelse(year_ts == anchor_year, gro_yr + prev_gro, gro_yr)
        ) %>%
        dplyr::select(-year_ts)
    }

    # append leading and trailing NA's
    df <- append_lead_trail_na(df = df, na = lead_trail_na)

    # Emit only the re-processing window. Everything before it is carried over
    # verbatim in 'prev_unchanged', so this drops the re-processed anchor row
    # and lets a caller supply L1 that reaches further back than the window
    # (extra cleaning context) without duplicating rows in the output.
    if (!is.null(reprocess_from_ts)) {
      df <- df %>% dplyr::filter(ts >= reprocess_from_ts)
    }

    df <- df %>%
      dplyr::mutate(gro_yr = ifelse(is.na(value), NA, gro_yr)) %>%
      dplyr::mutate(twd    = ifelse(is.na(value), NA, twd)) %>%
      dplyr::mutate(max    = ifelse(is.na(value), NA, max)) %>%
      dplyr::mutate(frost  = ifelse(is.na(value), NA, frost)) %>%
      dplyr::select(series_id, ts, value, max, twd, gro_yr, frost, flags) %>%
      dplyr::mutate(
        version = utils::packageDescription("treenetproc",
                                            fields = "Version", drop = TRUE))

    # Prepend the unchanged prev_L2 prefix ------------------------------------
    if (!is.null(prev_unchanged) && nrow(prev_unchanged) > 0) {
      prev_unchanged_out <- prev_unchanged %>%
        dplyr::select(dplyr::any_of(colnames(df)))
      # ensure version column exists
      if (!"version" %in% colnames(prev_unchanged_out)) {
        prev_unchanged_out <- prev_unchanged_out %>%
          dplyr::mutate(version = utils::packageDescription(
            "treenetproc", fields = "Version", drop = TRUE))
      }
      df <- dplyr::bind_rows(prev_unchanged_out, df)
    }

    list_L2[[s]] <- df

    # save threshold values for plot
    if (plot) {
      thr_plot <- saveplotthr(df = df, thr_out = passobj("thr_out_plot"),
                              thr_jump = passobj("thr_jump_plot"))
      list_thr[[s]] <- thr_plot
    }
  }

  df <- dplyr::bind_rows(list_L2)

  if (plot) {
    writeLines("plot data...")
    thr_plot <- dplyr::bind_rows(list_thr)
    plot_proc_L2(dendro_L1 = dendro_L1, dendro_L2 = df,
                 plot_period = plot_period, plot_show = plot_show,
                 plot_export = plot_export, plot_name = plot_name, tz = tz,
                 thr_plot = thr_plot)
  }

  return(df)
}
