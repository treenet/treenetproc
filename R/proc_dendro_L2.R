#' Process L1 Dendrometer Data to L2
#'
#' \code{proc_dendro_L2()} lets you process time-aligned (\code{L1})
#' dendrometer data to processed (\code{L2}) dendrometer data without
#' jumps, some gap correction, and the calculation of growth.
#'
#' @param dendro_data \code{data.frame} with time-aligned dendrometer
#' data. Output of function \code{proc_L1}.
#'
#' @param temp_data \code{data.frame} with time-aligned temperature data.
#' Output of function \code{proc_L1}. Can also contain other climate data.
#' In this case the name of the temperature data in the \code{series} column
#' has to contain \code{temp}.
#'
#' @param val_range numeric vector specifying the minimum and maximum
#' \code{c(min, max)} of credible dendrometer measurement values. Values
#' lower than \code{min} or higher than \code{max} are deleted without notice.
#'
#' @param diffwin maximal hourly difference expected in winter.
#'
#' @param diffsum maximal hourly difference expected in summer.
#'
#' @inheritParams proc_L1
#'
#' @return \code{data.frame} with processed dendrometer data.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' proc_dendro_L2(dendro_data = data_L1_dendro, temp_data = data_L1_temp,
#'                val_range = c(0, 20000))
#' }
#'
proc_dendro_L2 <- function(dendro_data, temp_data, val_range = c(0, 20000),
                           diffwin = 2000, diffsum = 1000, tz = "Etc/GMT-1") {

  dendro_data <- data_L1_dendro
  temp_data <- data_L1_temp
  val_range <- c(0, 20000)
  diffwin <- 2000
  diffsum <- 1000
  s <- 1


  # Check input variables -----------------------------------------------------
  if (!is.numeric(val_range)) {
    stop("provide numeric limits to 'val_range'.")
  }
  if (val_range[1] > val_range[2]) {
    stop("first value of 'val_range' has to be smaller than second value.")
  }

  # Check input data ----------------------------------------------------------
  df <- dendro_data
  tem <- temp_data

  if (sum(colnames(df) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned dendrometer data generated with 'proc_L1'")
  }
  if (sum(colnames(tem) %in% c("series", "ts", "value", "version")) != 4) {
    stop("provide time-aligned temperature data generated with 'proc_L1'")
  }

  reso_df <- reso_check(df)
  reso_tem <- reso_check(tem)
  if (reso_df != reso_tem) {
    stop("provide both dendrometer and temperature data at the same time
         resolution.")
  } else {
    reso <- reso_df
  }

  if (length(grep("temp", unique(tem$series), ignore.case = T)) > 1) {
    stop("provide single temperature dataset.")
  }
  if (length(grep("temp", unique(tem$series), ignore.case = T)) == 1) {
    tem <- tem %>%
      dplyr::group_by(series) %>%
      dplyr::mutate(temp_series =
                      ifelse(length(grep("temp", unique(series),
                                         ignore.case = T)) == 0, 0, 1)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(temp_series == 1) %>%
      dplyr::select(-temp_series)
  }
  if (length(grep("temp", unique(tem$series), ignore.case = T)) == 0) {
    message("check temperature data, series name does not contain 'temp'.")
  }

  ts_overlap_check(df = df, tem = tem)


  # Process to L2 (jump and gap corrections) ----------------------------------
  series_vec <- unique(df$series)
  list_L2 <- list()
  df_L1 <- df
  for (s in 1:length(series_vec)) {
    df <- df_L1 %>%
      dplyr::filter(series == series_vec[s])

    # save and remove leading and trailing NA's
    lead <- FALSE
    if (is.na(df$value[1])) {
      nrow_na <- which(!is.na(df$value))[1] - 1
      leading_na <- df[c(1:nrow_na), ]
      df <- df[-c(1:nrow_na), ]
      lead <- TRUE
    }
    le <- nrow(df)
    trail <- FALSE
    if (is.na(df$value[le])) {
      nrow_na <- max(which(!is.na(df$value))) + 1
      trailing_na <- df[c(nrow_na:le), ]
      df <- df[-c(nrow_na:le), ]
      trail <- TRUE
    }

    df <- cleanoutofrange(df = df, val_range = val_range)
    df <- creategapflag(df = df, reso = reso, gaplength = 24 * (60 / reso))
    df <- calcdiff(df = df, reso = reso)
    df <- createfrostflag(df = df, tem = tem, lowtemp = 5)
    df <- removeoutliers(df = df, quan = 0.001, wnd = 3, reso = reso)
    df <- calcdiff(df = df, reso = reso)
    df <- createflagdiff(df = df, reso = reso, diffwin = diffwin,
                         diffsum = diffsum)
    df <- executeflagdiff(df, length = 1)
    df <- calcdiff(df, reso = reso)
    df <- createflagdiff(df = df, reso = reso, diffwin = diffwin,
                         diffsum = diffsum)
    df <- creategapflag(df = df, reso = reso, gaplength = 24 * (60 / reso))
    df <- createjumpoutflag(df = df, thr = 0.2)
    df <- executejumpout(df = df)
    df <- fillintergaps(df = df, reso = reso, wnd = 4 * 60 / reso,
                        type = "linear")
    df <- calcmax(df = df)
    df <- calctwd_mds_gro(df = df, tz = tz)
    df <- summariseflags(df = df)

    if (lead) {
      df <- dplyr::bind_rows(leading_na, df)
    }
    if (trail) {
      df <- dplyr::bind_rows(df, trailing_na)
    }

    df <- df %>%
      dplyr::mutate(gro_year = ifelse(is.na(value), NA, gro_year)) %>%
      dplyr::mutate(mds = ifelse(is.na(value), NA, mds)) %>%
      dplyr::mutate(twd = ifelse(is.na(value), NA, twd)) %>%
      dplyr::mutate(max = ifelse(is.na(value), NA, max)) %>%
      dplyr::mutate(version = 2) %>%
      dplyr::select(series, ts, value, max, twd, mds, gro_year, flags, version)

    list_L2[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L2)
  return(df)
}
