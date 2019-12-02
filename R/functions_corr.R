#' Removes Wrong Corrections in Data
#'
#' \code{removecorr} removes wrong corrections in the dendrometer data.
#'
#' @inheritParams corr_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
removecorr <- function(data_L1, data_L2, remove, tz) {

  L1 <- data_L1 %>%
    dplyr::mutate(diff_L1 = c(NA, diff(value, lag = 1))) %>%
    dplyr::rename(value_L1 = value) %>%
    dplyr::select(series, ts, value_L1, diff_L1)

  df <- data_L2 %>%
    dplyr::mutate(diff_L2 = c(NA, diff(value, lag = 1))) %>%
    dplyr::rename(value_L2 = value) %>%
    dplyr::left_join(., L1, by = c("series", "ts")) %>%
    dplyr::mutate(diff = diff_L1 - diff_L2) %>%
    dplyr::mutate(diff = ifelse(abs(diff) <= 0.1, 0, diff)) %>%
    dplyr::mutate(diff = ifelse(is.na(diff), 0, diff)) %>%
    # add diff = 100 for removed outliers (flag = "out")
    dplyr::mutate(diff = ifelse(grepl("out", flags), 100, diff)) %>%
    dplyr::mutate(diff_plot = abs(diff)) %>%
    dplyr::mutate(diff_nr = 0) %>%
    dplyr::mutate(diff_nr = ifelse(grepl(".*out|.*jump", flags), 1, 0)) %>%
    dplyr::mutate(diff_nr = cumsum(diff_nr)) %>%
    dplyr::mutate(diff_nr = ifelse(grepl(".*out|.*jump", flags),
                                   diff_nr, NA))

  diff_L1 <- df$diff_L1
  val <- df$value_L2
  diff <- df$diff
  ts <- df$ts
  remove_row <- which(df$diff_nr %in% remove)
  ts_rem <- df$ts[remove_row]
  ts_rem <- as.POSIXct(paste(substr(as.character(ts_rem), 1, 10), "00:00:00"),
                       format = "%Y-%m-%d %H:%M:%S", tz = tz)
  flag_old <- df$flags
  flag <- as.vector(rep(FALSE, nrow(df)), mode = "logical")
  for (r in 1:length(remove_row)) {
    rem <- remove_row[r]
    val_diff <- diff[rem]

    # remove differences
    if (grepl("jump", flag_old[rem])) {
      val[rem:length(val)] <- val[rem:length(val)] + val_diff
    }
    # restore deleted values
    if (grepl("out", flag_old[rem])) {
      val[rem] <- val[rem - 1] + diff_L1[rem]
    }

    flag[rem] <- TRUE
  }

  # removed changes as input for plotting
  diff_old <- df %>%
    dplyr::filter(diff_nr %in% remove) %>%
    dplyr::rename(diff_nr_old = diff_nr) %>%
    dplyr::rename(diff_old = diff_plot) %>%
    dplyr::select(ts, diff_old, diff_nr_old)

  df <- data_L2 %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(flagremovecorr = flag)

  list_return <- list(df, diff_old)

  return(list_return)
}


#' Force Jump in Data
#'
#' \code{forejump} forces a jump (positive or negative) in the dendrometer
#'   data that was not corrected during the processing.
#'
#' @param n_days numeric, specifies the length of the period (in days) after
#'   the dates specified in \code{force} in which a missed jump is looked for.
#' @inheritParams plot_proc_L2
#' @inheritParams corr_dendro_L2
#'
#' @keywords internal
#'
forcejump <- function(data_L2, force, n_days = 5) {

  diff <- data_L2 %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff = c(NA, diff(value, lag = 1))) %>%
    dplyr::select(ts, diff) %>%
    dplyr::right_join(., data_L2, by = "ts") %>%
    dplyr::select(diff) %>%
    unlist(., use.names = FALSE)

  val <- data_L2$value
  ts <- data_L2$ts
  flag <- as.vector(rep(FALSE, nrow(data_L2)), mode = "logical")
  for (f in 1:length(force)) {
    f_start <- force[f]
    f_end <- f_start + as.difftime(n_days, units = "days")
    pos_start <- which(ts == f_start)
    pos_end <- which(ts == f_end)
    pos_diff <- which.max(abs(diff[pos_start:pos_end])) + pos_start - 1
    val_diff <- diff[pos_diff]

    val[pos_diff:length(val)] <- val[pos_diff:length(val)] - val_diff
    flag[pos_diff] <- TRUE
  }

  data_L2 <- data_L2 %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(flagforcejump = flag)

  return(data_L2)
}


#' Deletes Data in Specified Period
#'
#' \code{deleteperiod} deletes dendrometer data in specified period.
#'
#' @param df input \code{data.frame}.
#' @inheritParams corr_dendro_L2
#'
#' @keywords internal
#'
deleteperiod <- function(df, delete) {

  if ( (length(delete) %% 2) != 0) {
    stop("provide an even number of dates in 'delete'.")
  }

  val <- df$value
  ts <- df$ts
  flag <- as.vector(rep(FALSE, nrow(df)), mode = "logical")

  for (d in seq(1, length(delete), by = 2)) {
    d_start <- delete[d]
    d_end <- delete[d + 1]
    pos_start <- min(which(ts >= d_start))
    pos_end <- max(which(ts <= d_end))

    val[pos_start:pos_end] <- NA
    flag[pos_start:pos_end] <- TRUE
  }

  df <- df %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(flagdelete = flag)

  return(df)
}


#' Summarise Flags
#'
#' \code{summariseflagscorr} appends the flags of the corrections to the
#'   existing flags.
#'
#' @param df input \code{data.frame}.
#'
#' @keywords internal
#'
summariseflagscorr <- function(df, remove = NULL, force = NULL,
                               delete = NULL) {

  list_flags <- vector("list", length = 3)

  if (length(remove) != 0) {
    list_flags[[1]] <- ifelse(df$flagremovecorr, "rem", NA)
  } else {
    list_flags[[1]] <- NA
  }
  if (length(force) != 0) {
    list_flags[[2]] <- ifelse(df$flagforcejump, "fjump", NA)
  } else {
    list_flags[[2]] <- NA
  }
  if (length(delete) != 0) {
    list_flags[[3]] <- ifelse(df$flagdelete, "del", NA)
  } else {
    list_flags[[3]] <- NA
  }

  flags <- do.call("paste", c(list_flags, sep = ", "))
  list_all <- list(df$flags, flags)
  flags <- do.call("paste", c(list_all, sep = ", "))
  # remove flags of changes that were removed
  flags <- ifelse(grepl("(.*out|.*fill|.*jump)(.*rem)", flags, perl = TRUE),
                  gsub(".*out[[:digit:]]*|.*fill|.*jump[[:digit:]]*", "",
                       flags),
                  flags)
  # remove all other flags if value was deleted
  flags <- gsub(".*del", "del", flags)
  # remove NA's and single commas
  flags <- gsub(", NA|NA, |^, ", "", flags)
  flags <- ifelse(flags %in% c("NA", ""), NA, flags)

  df$flags <- flags

  return(df)
}
