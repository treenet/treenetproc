#' Time-align Raw Dendrometer or Temperature Data
#'
#' \code{proc_L1} aligns raw dendrometer and raw temperature data to
#'   regular time steps.
#'
#' @param data_L0 input \code{data.frame} containing raw dendrometer or
#'   temperature data (see Details for formatting requirements).
#' @param reso numeric, desired output time resolution (in minutes). See
#'   \code{Details} for more information on data aggregation.
#' @param input character, specify the structure of the input data
#'   (\code{data_L0}). Can either be in \code{"long"} format (i.e. sensors
#'   below each other with a \code{series} column separating the sensors)
#'   or in \code{"wide"} format (i.e. one sensor per column). See Details
#'   for more information on formatting requirements.
#' @param date_format character, specify a custom date format if it is not
#'   \code{"\%Y-\%m-\%d \%H:\%M:\%S"}.
#' @param year character, if \code{year = "asis"} the output
#'   \code{data.frame} covers the same period as the input data;
#'   if \code{year = "full"} then the output \code{data.frame} is expanded to
#'   complete years, i.e. \code{YYYY-01-01 to YYYY-12-31}.
#' @param tz specify the desired time zone. Default is \code{"UTC"}.
#'
#' @details Data of multiple sensors can either be in \code{long}
#'   format (\code{input = "long"}) with a column named \code{series} to
#'   differentiate the sensors or in \code{wide} format (\code{input = "wide"})
#'   with sensors in separate columns. If temperature data is provided
#'   in the same \code{data.frame} as dendrometer data, the name of the
#'   temperature sensor must contain the string \code{temp}. See the following
#'   vignette for examples of input data:
#'   \href{../doc/Introduction-to-treenetproc.html}{\code{vignette("Introduction-to-treenetproc", package = "treenetproc")}}.
#'
#'   The time-alignment uses a linear interpolation between the two closest
#'   measurement points in the raw dataset to obtain the values at the
#'   specified time resolution. The linear interpolation is restricted to gaps
#'   that are smaller than \code{2.1 * reso} (i.e. two timestamps). If data is
#'   already provided at regular time steps, the corresponding values are
#'   selected.
#'
#' @return a \code{data.frame} with measurements aligned to regular time
#'   intervals (interval specified in \code{reso}) containing the following
#'   columns:
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{version}{package version number.}
#'
#' @export
#'
#' @examples
#' # Input data in 'long' format
#' proc_L1(data_L0 = dendro_data_L0)
#'
#' # Input data in 'wide' format
#' proc_L1(data_L0 = dendro_data_L0_wide, input = "wide")
#'
proc_L1 <- function(data_L0, reso = 10, input = "long",
                    date_format = "%Y-%m-%d %H:%M:%S", year = "asis",
                    tz = "UTC") {

  # Check input variables -----------------------------------------------------
  list_inputs <- mget(ls())
  check_input_variables(list = list_inputs)
  passenv$reso <- reso


  # Check input data ----------------------------------------------------------
  df <- data_L0
  df <- check_ts(df = df, date_format = date_format, tz = tz)
  check_format(df = df, input = input)
  reso_check_L0(df = df, reso = reso, tz = tz)


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
