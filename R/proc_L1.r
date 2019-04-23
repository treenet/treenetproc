#' Process Raw Dendrometer or Meteorological Data to L1
#'
#' \code{proc_L1()} processes raw dendrometer and raw meteorological data to
#'   time-aligned data.
#'
#' @param data input \code{data.frame} containing raw dendrometer or
#'   meteorological data. The input data needs to contain a timestamp column
#'   (named \code{ts}). If \code{ts} is not provided in the standard format
#'   (\code{\%Y-\%m-\%d \%H:\%M:\%S}) the format needs to be specified in
#'   \code{format_date}. Data of multiple sensors can either be in \code{long}
#'   format \code{input = "long"} with a column named \code{series} to
#'   differentiate the sensors or in \code{wide} format \code{input = "wide"}
#'   with sensors in separate columns. The name of temperature and
#'   precipitation data have to at least contain \code{temp} or \code{prec},
#'   respectively.
#' @param reso desired time resolution of output (in minutes). Resolution
#'   should be chosen to be close to the actual measurement intervals.
#'   \code{reso} needs to be a multiple of 5.
#' @param year if \code{year = "full"} then the output \code{data.frame}
#'   contains data of complete years, i.e. \code{YYYY-01-01 to YYYY-12-31}; if
#'   \code{year = "asis"} the output \code{data.frame} covers the same period
#'   as the input data.
#' @param input specify the way the input \code{data.frame} is structured. Can
#'   be either in \code{"long"} format (i.e. sensors below each other with a
#'   \code{series}) or in \code{"wide"} format (i.e. one sensor per column).
#' @param date_format specify a custom date format if it is not
#'   \code{\%Y-\%m-\%d \%H:\%M:\%S}.
#' @param tz specify the desired time zone. Default is \code{"Etc/GMT-1"}.
#'
#' @details Data at \emph{irregular} time steps: Data is linearly interpolated
#'   between the irregular time steps closest to the regular time step.
#'
#'   Data at \emph{regular} time steps: Values corresponding to regular time
#'   stamps are selected.
#'
#' @return a \code{data.frame} with measurements aligned to regular time
#'   intervals (interval specified in \code{reso}) containing the following
#'   columns:
#'    \item{series}{name of the series.}
#'    \item{ts}{timestamp with format \code{\%Y-\%m-\%d \%H:\%M:\%S}.}
#'    \item{value}{dendrometer value.}
#'    \item{version}{processing version.}
#'
#' @export
#'
#' @examples
#' \dontrun{
#' proc_L1(data = data_L0_long)
#' }
proc_L1 <- function(data, reso = 10, year = "asis", input = "long",
                    date_format = "%Y-%m-%d %H:%M:%S", tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (!(year %in% c("asis", "full"))) {
    stop("year needs to be either 'asis' or 'full'.")
  }
  if (!(input %in% c("long", "wide"))) {
    stop("input needs to be either 'long' or 'wide'.")
  }


  # Check input data ----------------------------------------------------------
  df <- data
  check_format(df = df, input = input)
  df <- check_ts(df = df, date_format = date_format, tz = tz)


  # Format input data ---------------------------------------------------------
  df <- format_input(df = df, input = input, tz = tz)
  series_vec <- unique(df$series)


  # Process to L1 (time alignement) -------------------------------------------
  list_L1 <- vector("list", length = length(series_vec))
  df_L0 <- df
  for (s in 1:length(series_vec)) {
    df <- df_L0 %>%
      dplyr::filter(series == series_vec[s])

    df <- tsalign(df = df, reso = reso, year = year, tz = tz)

    df <- df %>%
      dplyr::mutate(series = series_vec[s]) %>%
      dplyr::mutate(version = 1) %>%
      dplyr::select(series, ts, value, version)
    list_L1[[s]] <- df
  }

  df <- dplyr::bind_rows(list_L1)

  return(df)
}
