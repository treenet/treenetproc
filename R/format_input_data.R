#' Format Input Data
#'
#' \code{format_input} formats input data. Wide format is convertet to long
#' and correct column classes are assigned.
#'
#' @param df input \code{data.frame}.
#'
#' @inheritParams process_dendro
#'
#' @return \code{data.frame}
#'
#' @examples
#'

format_input <- function(df, input) {
  if (input == "wide") {
    nc <- ncol(df)
    nr <- nrow(df)
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      transform(ts = as.POSIXct(as.character(ts),
                                format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      dplyr::mutate(id = 1:nr) %>%
      reshape(., timevar = "series", idvar = "id", varying = c(2:nc),
              direction = "long", v.names = "value", times = col_names) %>%
      dplyr::select(-id) %>%
      transform(value = as.numeric(value)) %>%
      dplyr::distinct() %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  if (input == "long") {
    col_names <- colnames(df)
    col_names <- col_names[!col_names == "ts"]
    df <- df %>%
      transform(ts = as.POSIXct(as.character(ts),
                                format = "%Y-%m-%d %H:%M:%S", tz = tz)) %>%
      dplyr::distinct() %>%
      dplyr::select(ts, col_names) %>%
      transform(value = as.numeric(value)) %>%
      dplyr::filter(!is.na(ts)) %>%
      dplyr::arrange(series, ts)
  }

  return(df)
}
