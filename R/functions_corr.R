#' Reverses Wrong Corrections in Data
#'
#' \code{reversecorr} reverses wrong corrections in the dendrometer data.
#'
#' @inheritParams corr_dendro_L2
#' @inheritParams plot_proc_L2
#'
#' @keywords internal
#'
reversecorr <- function(data_L1, data_L2, reverse, tz) {

  L1 <- data_L1 %>%
    dplyr::mutate(diff_L1 = c(NA, diff(value, lag = 1))) %>%
    dplyr::rename(value_L1 = value) %>%
    dplyr::select(series, ts, value_L1, diff_L1)

  df <- data_L2 %>%
    dplyr::mutate(diff_L2 = c(NA, diff(value, lag = 1))) %>%
    dplyr::rename(value_L2 = value) %>%
    dplyr::left_join(., L1, by = c("series", "ts")) %>%
    dplyr::mutate(diff_plot = abs(diff_L1 - diff_L2)) %>%
    # add diff = 100 for reversed outliers (flag = "out")
    dplyr::mutate(diff_plot = ifelse(grepl("out", flags), 100, diff_plot)) %>%
    dplyr::mutate(diff_nr = 0) %>%
    dplyr::mutate(diff_nr = ifelse(grepl(".*out|.*jump", flags), 1, 0)) %>%
    dplyr::mutate(diff_nr = cumsum(diff_nr)) %>%
    dplyr::mutate(diff_nr = ifelse(grepl(".*out|.*jump", flags),
                                   diff_nr, NA)) %>%
    # sum up diff before jumps for jump removal
    dplyr::mutate(jump_group = !is.na(diff_nr)) %>%
    dplyr::mutate(jump_group = cumsum(jump_group)) %>%
    dplyr::mutate(y = c(0, diff(jump_group, lag = 1))) %>%
    dplyr::mutate(z = c(0, diff(y, lag = 1))) %>%
    dplyr::mutate(z = ifelse(z == -1, 1, z)) %>%
    dplyr::mutate(jump_nr = cumsum(z)) %>%
    dplyr::group_by(jump_nr) %>%
    dplyr::mutate(jump_group = ifelse(any(grepl(".*jump", flags)),
                                      jump_nr, NA)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(jump_group) %>%
    dplyr::mutate(diff_jump = sum(diff_L1, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(diff_L1 = ifelse(grepl(".*jump", flags),
                                   diff_jump, diff_L1)) %>%
    # add jump diff for plotting
    dplyr::mutate(diff_plot = ifelse(grepl(".*jump", flags),
                                     abs(diff_jump), diff_plot))

  diff_L1 <- df$diff_L1
  val <- df$value_L2
  ts <- df$ts
  reverse_row <- which(df$diff_nr %in% reverse)
  ts_rem <- df$ts[reverse_row]
  ts_rem <- as.POSIXct(paste(substr(as.character(ts_rem), 1, 10), "00:00:00"),
                       format = "%Y-%m-%d %H:%M:%S", tz = tz)
  flag_old <- df$flags
  flag <- as.vector(rep(FALSE, nrow(df)), mode = "logical")
  for (r in 1:length(reverse_row)) {
    rev <- reverse_row[r]

    # reverse differences
    if (grepl("jump", flag_old[rev])) {
      # if two or more jumps are consecutive, the difference value is accumulated
      # so when undoing / reversing one of the jumps is equivalent to reversing
      # altogether. This might have a better fix, but for now let's skip when
      # there's more than one jump together. Maybe it happens with outliers too?
      if (!(r > 1 && rev == reverse_row[r-1]+1)) { # Check the time instant is not the successor of the previous one. so they are consecutive.
        val[rev:length(val)] <- val[rev:length(val)] + diff_L1[rev]
      }
    }
    # restore deleted values
    if (grepl("out", flag_old[rev])) {
      val[rev] <- val[rev - 1] + diff_L1[rev]
    }

    flag[rev] <- TRUE
  }

  # reversed changes as input for plotting
  diff_old <- df %>%
    dplyr::filter(diff_nr %in% reverse) %>%
    dplyr::rename(diff_nr_old = diff_nr) %>%
    dplyr::rename(diff_old = diff_plot) %>%
    dplyr::select(ts, diff_old, diff_nr_old)

  df <- data_L2 %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(flagreversecorr = flag)

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
    dplyr::mutate(value = ifelse(grepl("fill", flags), NA, value)) %>%
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(diff = c(NA, diff(value, lag = 1))) %>%
    dplyr::select(ts, diff) %>%
    dplyr::right_join(., data_L2, by = "ts") %>%
    dplyr::select(diff) %>%
    unlist(., use.names = FALSE)

  val <- data_L2$value
  ts <- data_L2$ts
  flag <- as.vector(rep(FALSE, nrow(data_L2)), mode = "logical")
  flagfill <- as.vector(rep(FALSE, nrow(data_L2)), mode = "logical")
  for (f in 1:length(force)) {
    f_start <- force[f]
    f_end <- f_start + as.difftime(n_days, units = "days")
    pos_start <- which(ts == f_start)
    pos_end <- which(ts == f_end)
    pos_diff <- which.max(abs(diff[pos_start:pos_end])) + pos_start - 1
    val_diff <- diff[pos_diff]

    val[pos_diff:length(val)] <- val[pos_diff:length(val)] - val_diff
    flag[pos_diff] <- TRUE
    flagfill[pos_start:(pos_diff - 1)] <-
      ifelse(is.na(diff[pos_start:(pos_diff - 1)]), TRUE, FALSE)
  }

  data_L2 <- data_L2 %>%
    dplyr::mutate(value = val) %>%
    dplyr::mutate(flagforcejump = flag) %>%
    dplyr::mutate(flagtemp = flagfill) %>%
    dplyr::mutate(value = ifelse(grepl("fill", flags) &
                                   flagtemp, NA, value)) %>%
    dplyr::select(-flagtemp)

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

  if ((length(delete) %% 2) != 0) {
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
summariseflagscorr <- function(df, reverse = NULL, force = NULL,
                               delete = NULL) {

  list_flags <- vector("list", length = 3)

  if (length(reverse) != 0) {
    list_flags[[1]] <- ifelse(df$flagreversecorr, "rev", NA)
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
  # remove flags of changes that were reversed
  flags <- ifelse(grepl("(.*out|.*fill|.*jump)(.*rev)", flags, perl = TRUE),
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
