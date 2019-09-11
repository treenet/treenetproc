#' Function to select last values
#'
#' \code{last_values()} selects specified number of values from current moment
#'   back in time.
#'
#' @param df input \code{data.frame}.
#' @inheritParams select_data
#'
#' @keywords internal, treenet
#'
#' @examples
#' \dontrun{
#' last_values(df = treenetdown:::df_last_values, last = 144,
#'             reso = 10)
#' }
#'
last_values <- function(df, last, reso, tz) {
  current_posix <- as.POSIXct(strptime(Sys.time(),
                                       format = "%Y-%m-%d %H:%M:%S",
                                       tz = tz))
  H <- as.numeric(format(current_posix, "%H"))
  M <- as.numeric(format(current_posix, "%M"))
  S <- as.numeric(format(current_posix, "%S"))
  D <- as.POSIXct(substr(as.character(current_posix), 1, 10),
                  format = "%Y-%m-%d", tz = tz)
  secs <- 3600 * H + 60 * M + S
  current_ts <- as.POSIXct(trunc(secs / (60 * reso)) * 60 * reso, origin = D,
                           tz = tz)
  last_ts <- current_ts - as.difftime(last * reso, units = "mins")

  df <- df %>%
    dplyr::filter(ts <= current_ts) %>%
    dplyr::filter(ts >= last_ts)

  return(df)
}


#' Generate Query for Data from Decentlab Server
#'
#' \code{get_decentlab} generates a query to download a specified
#'   series from the decentlab servers.
#'
#' @param meta_single character, name of the sensor to download.
#' @param path_cred character, path to the file with the credentials.
#' @inheritParams proc_L1
#'
#' @keywords internal, treenet
#'
get_decentlab <- function(meta_single, path_cred = NULL, tz) {

  cred <- config::get("decentlab_cred", file = path_cred)
  domain <- cred$domain
  apiKey <- cred$api_key
  selectVar <- 'value'
  fill <- ''
  timeFilter <- fill <- interval <- ''
  device <- location <- sensor <- channel <- "//"
  includeNetworkSensors <- FALSE
  convertTimestamp <- TRUE
  timezone <- tz
  doCast <- FALSE
  aggFunc <- ""

  baseUrl <- paste0("https://", domain,
                    "/api/datasources/proxy/1/query?db=main&epoch=ms&q=")

  if (aggFunc != "") {
    selectVar <- paste0(aggFunc, '("value") as value')
    fill <- 'fill(null)'
  }

  if (timeFilter != "") {
    timeFilter <- paste0(' AND ', timeFilter)
  }

  filter <-
    paste0(" location =~ ", location,
           " AND node =~ ", device,
           " AND sensor =~ ", sensor,
           " AND ((channel =~ ", channel, " OR channel !~ /.+/)",
           " AND uqk =~ /", meta_single, "/",
           if (includeNetworkSensors) ")" else " AND channel !~ /^link-/)")

  q <- paste(
    'SELECT ',
    selectVar,
    ' FROM "measurements" ',
    ' WHERE ', filter, timeFilter,
    ' GROUP BY channel,node,sensor,unit,uqk ',
    interval,
    fill,
    sep = ' '
  )

  res <- httr::GET(paste0(baseUrl, utils::URLencode(q)),
                   httr::add_headers(Authorization =
                                       paste0("Bearer ", apiKey)))
  json <- httr::content(res)

  if (res$status_code != 200) {
    stop(json$message, json$error)
  }
  series <- json$results[[1]]$series

  if (is.null(series)) {
    stop("No series returned")
  }

  lists <- lapply(series, function(s) {
    tbl <- do.call(rbind, lapply(s$values, rbind))
    i <- 1
    for (tag in s$tags) {
      tagcol <- rep(tag, dim(tbl)[1])
      tbl <- cbind(tbl, tagcol)
      colnames(tbl)[dim(tbl)[2]] <- names(s$tags)[i]
      i <- i + 1
    }
    tbl
  })

  mat <- do.call(rbind, lists)
  mat[sapply(mat, is.null)] <- NA
  df <- data.frame(time = unlist(mat[, 1]), value = unlist(mat[, 2]),
                   apply(mat[, 3:dim(mat)[2]], 2, unlist))
  if (convertTimestamp) {
    df$time <- as.POSIXct(df$time / 1000,
                          origin = "1970-01-01",
                          tz = timezone)
  }

  return(df)
}
