#' Process Raw Dendrometer or Meteorological Data to L1
#'
#' \code{proc_L1()} lets you process raw dendrometer and raw
#' meteorological data to time-aligned data.
#'
#' @param data input \code{data.frame} containing raw dendrometer or
#' meteorological data. The input data needs to contain a timestamp column
#' (named \code{ts}) formatted as: \code{%Y-%m-%d %H:%M:%S}. Data of multiple
#' sensors can either be in \emph{long} format \code{input = "long"} with a
#' column named \code{series} to differentiate the sensors or in \emph{wide}
#' format \code{input = "wide"} with sensors in separate columns. The name of
#' temperature and precipitation data have to contain \code{temp} or
#' \code{prec}, respectively.
#'
#' @param reso time resolution between two timestamps (in minutes). Resolution
#' should be chosen to be close to the actual measurement intervals.
#' \code{reso} needs to be a multiple of 5.
#'
#' @param tz specify the desired time zone. Default is \code{"Etc/GMT-1"}.
#'
#' @param input specify the way the input \code{data.frame} is structured. Can
#' be either in \code{"long"} format (i.e. sensors below each other with a
#' \code{series}) column specifying the sensors) or in \code{"wide"} format
#' (i.e. one sensor per column).
#'
#' @return
#' @export
#'
#' @examples
#'

proc_L1 <- function(data, reso = 10, year = "asis", tz = "Etc/GMT-1",
                    input = "long") {
  data <- data_L0_long_temp
  reso <- 10
  year <- "asis"
  tz <- "Etc/GMT-1"
  input <- "long"


  # Check input variables -----------------------------------------------------
  if (!(year %in% c("asis", "full"))) {
    stop("year needs to be either 'asis' or 'full'.")
  }
  if (!(input %in% c("long", "wide"))) {
    stop("input needs to be either 'long' or 'wide'.")
  }


  # Check input data ----------------------------------------------------------
  df <- data
  check_format(df)


  # Format input data ---------------------------------------------------------
  df <- format_input(df = df, input = input)
  series_vec <- unique(df$series)


  # Process to L1 (time alignement) -------------------------------------------
  list_L1 <- list()
  df_L0 <- df
  for (s in 1:length(series_vec)) {
    df <- df_L0 %>%
      dplyr::filter(series == series_vec[s])

    if (length(grep("prec", series_vec[s], ignore.case = T)) > 0) {
      prec_sum_raw <- sum(df$value, na.rm = T)
      df <- tsalign_prec(df = df, reso = reso, year = year, tz = tz)
      prec_sum_proc <- sum(df$value, na.rm = T)
      if (!(identical(prec_sum_raw, prec_sum_proc))) {
        stop("there was an error with the time-alignement in the precipitation
             data.")
      }
    } else {
      df <- tsalign(df = df, reso = reso, year = year, tz = tz)
    }

    df <- df %>%
      dplyr::mutate(series = series_vec[s]) %>%
      dplyr::mutate(version = 1)
    list_L1[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L1)
  return(df)
}
