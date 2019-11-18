#' Process Raw Dendrometer or Meteorological Data to L1
#'
#' \code{proc_L1} processes raw dendrometer and raw meteorological data to
#'   time-aligned data.
#'
#' @param data input \code{data.frame} containing raw dendrometer or
#'   meteorological data (see Details for formatting requirements).
#' @param reso desired time resolution of output (in minutes). See
#'   \code{Details} for more information on data aggregation.
#' @param year if \code{year = "full"} then the output \code{data.frame}
#'   contains data of complete years, i.e. \code{YYYY-01-01 to YYYY-12-31}; if
#'   \code{year = "asis"} the output \code{data.frame} covers the same period
#'   as the input data.
#' @param input specify the way the input \code{data.frame} is structured. Can
#'   be either in \code{"long"} format (i.e. sensors below each other with a
#'   \code{series}) or in \code{"wide"} format (i.e. one sensor per column).
#' @param date_format specify a custom date format if it is not
#'   \code{\%Y-\%m-\%d \%H:\%M:\%S}.
#' @param tz specify the desired time zone. Default is \code{"UTC"}.
#'
#' @details The input data needs to contain a timestamp column
#'   (named \code{ts}). If \code{ts} is not provided in the standard format
#'   (\code{\%Y-\%m-\%d \%H:\%M:\%S}) the format needs to be specified in
#'   \code{format_date}.
#'
#'   Data of multiple sensors can either be in \code{long}
#'   format (\code{input = "long"}) with a column named \code{series} to
#'   differentiate the sensors or in \code{wide} format (\code{input = "wide"})
#'   with sensors in separate columns. If temperature or precipitation data
#'   are provided in the same \code{data.frame} as dendrometer data, the names
#'   of the sensors must contain \code{temp} or \code{prec}, respectively.
#'
#'   To align data at regular time intervals defined in \code{reso}, the data
#'   is linearly interpolated between the two values measured closest to the
#'   regular time intervals. The linear interpolation is restricted to gaps
#'   that are smaller than \code{2.1 * reso}. If data is already provided at
#'   regular time steps, the corresponding values are selected. For more
#'   information see the following vignette: For more details see the
#'   following vignette:
#'   \href{../doc/Functionality-of-treenetproc.html}{\code{vignette("Functionality-of-treenetproc", package = "treenetproc")}}
#'
#' @return a \code{data.frame} with measurements aligned to regular time
#'   intervals (interval specified in \code{reso}) containing the following
#'   columns:
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{version}{package version that was used.}
#'
#' @export
#'
#' @examples
#' proc_L1(data = dendro_data_L0)
#' proc_L1(data = dendro_data_L0_wide, input = "wide")
#'
proc_L1 <- function(data, reso = 10, year = "asis", input = "long",
                    date_format = "%Y-%m-%d %H:%M:%S", tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)
  passenv$reso <- reso


  # Check input data ----------------------------------------------------------
  df <- data
  df <- check_ts(df = df, date_format = date_format, tz = tz)
  check_format(df = df, input = input)
  reso_check_L0(df = df, reso = reso)


  # Format input data ---------------------------------------------------------
  df <- format_input(df = df, input = input, tz = tz)
  df <- check_missing(df = df)
  series_vec <- unique(df$series)


  # Process to L1 (time-alignment) --------------------------------------------
  list_L1 <- vector("list", length = length(series_vec))
  df_L0 <- df
  for (s in 1:length(series_vec)) {
    df <- df_L0 %>%
      dplyr::filter(series == series_vec[s])

    df <- tsalign(df = df, reso = reso, year = year, tz = tz)

    df <- df %>%
      dplyr::mutate(series = series_vec[s]) %>%
      dplyr::mutate(
        version =
          utils::packageDescription("treenetproc",
                                    fields = "Version", drop = TRUE))
    list_L1[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L1) %>%
    dplyr::arrange(series, ts)

  return(df)
}
