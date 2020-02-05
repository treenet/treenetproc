#' Load Credentials
#'
#' \code{load_credentials} loads the credentials from the file 'config.yml'.
#'
#' @inheritParams select_series
#'
#' @keywords internal, treenet
#'
load_credentials <- function(path_cred = NULL) {

  if (length(path_cred) == 0) {
    if (file.exists(file.path(system.file("extdata", "config.yml",
                                          package = "treenetproc")))) {
      path_cred <- file.path(system.file("extdata", "config.yml",
                                         package = "treenetproc"))
    } else {
      stop("File with database credentials not found. Please move the ",
           "file 'config.yml' to the folder of the package or specify ",
           "an alternative path in the function call with 'path_cred = ...'")
    }
  } else {
    path_cred <- path_cred
  }

  return(path_cred)
}


#' Select Series from Metadata File
#'
#' \code{select_series} selects series from the metadata file based on
#'   selection criteria specified by the user (i.e. site, sensor_class, etc.).
#'   In addition, reference temperature data is selected for all dendrometer
#'   series.
#'
#' @param site character, specify the site. String is not case sensitive.
#' @param sensor_class character, specify a single or multiple sensor
#'   classes. String does not have to contain the full name of the sensor
#'   class, i.e. \code{"dendro"} works for \code{"dendrometer"}. String is not
#'   case sensitive.
#' @param sensor_name character, specify the name of a single or
#'   multiple sensors (e.g. \code{"LWF-Demo-1.dendrometer.ch0"}).
#' @param path_cred optional argument to specify the full path to the
#'   file \code{config.yml} containing the database login data. File can
#'   also be copied to the main folder of the package for automatic
#'   recognition.
#'
#' @keywords internal, treenet
#'
select_series <- function(site, sensor_class, sensor_name, path_cred) {

  # Check availability of packages --------------------------------------------
  check_package(pck_name = "config")
  check_package(pck_name = "googlesheets4")
  check_package(pck_name = "googledrive")


  # Select series for download via metadata file ------------------------------
  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  # read metadata file
  auth <- config::get("googledrive_auth", file = path_cred)
  googledrive::drive_auth(email = auth$email)
  googlesheets4::sheets_auth(token = googledrive::drive_token())

  suppressMessages(
    meta <- googledrive::drive_get("Metadata") %>%
      googlesheets4::read_sheet("Metadata")
  )

  # select specified series from metadata file
  meta_filter <- meta$Seriesname
  meta_airtemp <- vector()
  # site
  if (!is.null(site)) {
    meta_select <- vector()
    for (t in 1:length(site)) {
      meta_sub <- meta %>%
        dplyr::filter(Seriesname %in% meta_filter) %>%
        dplyr::filter(grepl(paste0(site[t]), Site, ignore.case = TRUE))

      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$Seriesname)
        meta_airtemp <- unique(meta_sub$Site_temp_ref)
      } else {
        stop(paste0("Site '", site[t], "' does not exist."))
      }
    }
    meta_filter <- meta_select
  }
  # sensor_class
  if (!is.null(sensor_class)) {
    meta_select <- vector()
    for (t in 1:length(sensor_class)) {
      lookup <-
        c(Dendrometer = "dendrometer",
          Temp = "airtemperature",
          Precipitation = "precipitation",
          Rad = "radiation",
          RelH = "relativeairhumidity_relh",
          Soil_temp = "soiltemperatrue",
          Soil_WP = "soilwaterpotential_soilwp",
          Wind_speed = "windspeed",
          Dendrometer_X = "dendrometer",
          Sapflow = "sapflow",
          StemCO2 = "stemco2",
          Soil_Temp = "soiltemperature",
          Soil_VWC = "soilvolumetricwatercontent_soilvwc",
          Air_pressure = "airpressure",
          PAR = "radiation_par",
          StemCO2_soil = "soilstemco2",
          Soil_WC = "soilwatercontent_soilwc",
          Stem_temp = "stemtemperature",
          Ozone = "ozone",
          Precipitation_Invervall = "precipitationinterval")

      meta_sensor <- meta %>%
        dplyr::filter(Seriesname %in% meta_filter) %>%
        dplyr::mutate(Sensor_query = unname(lookup[Sensor_class])) %>%
        dplyr::filter(grepl(paste0(sensor_class[t]), Sensor_query,
                            ignore.case = TRUE))

      if (nrow(meta_sensor) != 0) {
        meta_select <- c(meta_select, meta_sensor$Seriesname)
      } else {
        stop(paste0("Sensor '", sensor_class[t], "' does not exist."))
      }
    }
    meta_filter <- meta_select
  }
  # sensor_name
  if (!is.null(sensor_name)) {
    meta_select <- vector()
    for (t in 1:length(sensor_name)) {
      meta_sub <- meta %>%
        dplyr::filter(Seriesname %in% meta_filter) %>%
        dplyr::filter(grepl(paste0(sensor_name[t]), Seriesname,
                            ignore.case = TRUE))

      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$Seriesname)
      } else {
        stop(paste0("Sensor name '", sensor_name[t], "' does not exist."))
      }
    }
    meta_filter <- meta_select
  }

  meta_list <- list(meta, meta_filter)

  # Select reference temperature data for download via metadata file ----------
  meta_series_temp <- select_temp_data(meta_list = meta_list)

  return(meta_series_temp)
}


#' Select Reference Temperature Data for Selected Series
#'
#' \code{select_temp_data} selects reference temperature data for dendrometer
#'   series based on the metadata file.
#'
#' @param meta_list list, containing the metadata file (first element) and
#'   all series specified for download from the server (second element).
#'
#' @keywords internal, treenet
#'
select_temp_data <- function(meta_list) {

  meta <- meta_list[[1]]
  meta_series <- meta_list[[2]]

  # select names of reference temperature data
  meta_temp <- meta %>%
    dplyr::filter(Seriesname %in% meta_series) %>%
    dplyr::select(Seriesname, Site_temp_ref) %>%
    dplyr::select(series = Seriesname, temp_ref = Site_temp_ref)

  return(meta_temp)
}


#' Download Series from Server
#'
#' \code{download_series} downloads specified series from TreeNet or
#'   Decentlab server
#'
#' @param meta_series \code{data.frame}, contains the names of the series to
#'   download including the reference temperature dataset for each series.
#' @param data_format character, select processing level of data. Can either
#'   be \code{"L0"} (i.e. raw data), \code{"L1"} (i.e time-aligned) or
#'   \code{"L2"} (i.e. processed).
#' @param data_version character, optional argument specifying the package
#'   version number. If provided, \code{L2} data generated by the package
#'   version specified is downloaded from the server (download of \code{L0} or
#'   \code{L1} data is not affected).
#' @param from character, optional argument to select data after a specific
#'   date (\code{"YYYY-MM-DD"}).
#' @param to character, optional argument to select data up to a specific
#'   date (\code{"YYYY-MM-DD"}).
#' @param last numeric, optional argument to specify the number of latest
#'   values. If \code{last} is specified \code{from} and \code{to} need to
#'   be empty.
#' @param bind_df logical, indicate whether data should be returned as a
#'   \code{data.frame} (\code{bind_df = TRUE}) or as a named list
#'   \code{bind_df = FALSE}.
#' @param server character, specify server from which data is downloaded.
#'   Can be either \code{treenet} or \code{decentlab}.
#' @param temp_ref logical, specify whether the reference temperature
#'   dataset(s) should be downloaded along with specified data.
#' @param use_intl logical, specify whether this function is used
#'   internally on the server. Changes \code{stop()} error messages to
#'   \code{message()} only.
#' @inheritParams select_series
#' @inheritParams proc_L1
#'
#' @keywords internal, treenet
#'
download_series <- function(meta_series, data_format, data_version = NULL,
                            from, to, last, bind_df, reso, path_cred, server,
                            temp_ref, tz, use_intl) {

  # Check availability of packages --------------------------------------------
  check_package(pck_name = "sqldf")
  check_package(pck_name = "RPostgres")
  check_package(pck_name = "config")
  check_package(pck_name = "DBI")
  check_package(pck_name = "httr")


  # Set default data_version for L2 data --------------------------------------
  # load credentials
  path_cred <- load_credentials(path_cred = path_cred)

  if (length(data_version) == 0 & data_format == "L2") {
    auth <- config::get("googledrive_auth", file = path_cred)
    googledrive::drive_auth(email = auth$email)
    googlesheets4::sheets_auth(token = googledrive::drive_token())

    suppressMessages(
      data_version <- googledrive::drive_get("Metadata") %>%
        googlesheets4::read_sheet("Ancillary") %>%
        unname() %>%
        unlist()
    )
  }


  # Load functions ------------------------------------------------------------
  Sys.setenv(TZ = tz)
  setUTC1 <- function() {
    temp <- as.character(sqldf::sqldf("show timezone", connection = con))
    if (temp != tz) {
      xx <- sqldf::sqldf(paste0("SET TIME ZONE '", tz, "'"), connection = con)
    }
    temp <- as.character(sqldf::sqldf("show timezone", connection = con))
    if (temp != tz) {
      stop("Error with timestamp in database.")
    }
  }


  # Download series -----------------------------------------------------------
  # specify format
  if (server == "treenet") {
    if (data_format == "L0") {
      db_folder <- "treenet0"
      db_version <- "version = 0;"
      version_nm <- "L0"
    }
    if (data_format == "L1") {
      db_folder <- "treenet1"
      db_version <- "version = 1;"
      version_nm <- "L1"
    }
    if (data_format == "L2") {
      db_folder <- "treenet2"
      db_version <- paste0("version = '", data_version, "';")
      version_nm <- "L2"
    }
  }
  if (server == "decentlab") {
    version_nm <- "L0"
  }

  # download series
  options(warn = -1)
  if (temp_ref) {
    n_temp_ref <- length(unique(meta_series$temp_ref))
    n_series <- nrow(meta_series) + n_temp_ref
    series <- c(meta_series$series, unique(meta_series$temp_ref))
  }
  if (!temp_ref) {
    n_series <- nrow(meta_series)
    series <- meta_series$series
  }
  server_data <- vector("list", length = n_series)
  for (i in 1:n_series) {
    if (server == "treenet") {
      drv <- RPostgres::Postgres()
      cred <- config::get("treenet_cred", file = path_cred)
      con <- DBI::dbConnect(drv,
                            dbname = cred$dbname,
                            host = cred$host,
                            port = cred$port,
                            user = cred$user,
                            password = cred$password)
      setUTC1()

      foo <- sqldf::sqldf(paste0("SELECT * from ", db_folder,
                                 " where series = '", series[i], "' AND ",
                                 db_version), connection = con)
      invisible(DBI::dbDisconnect(con))
    }
    if (server == "decentlab") {
      series_single <- series[i]
      foo <- get_decentlab(series_single = series_single,
                           path_cred = path_cred, tz = tz) %>%
        dplyr::select(series = uqk, ts = time, value) %>%
        transform(series = as.character(series))
    }

    df <- foo %>%
      dplyr::select(-insert_date) %>%
      transform(ts = as.POSIXct(ts, format = "%m-%d-%y %H:%M:%S",
                                tz = tz)) %>%
      dplyr::arrange(ts) %>%
      dplyr::distinct() %>%
      dplyr::filter(ts <= Sys.time()) %>%
      transform(value = as.numeric(value))

    if (length(from) != 0) {
      from <- as.POSIXct(from, format = "%Y-%m-%d", tz = tz)
      df <- df %>%
        dplyr::filter(ts >= from)
    }
    if (length(to) != 0) {
      to <- as.POSIXct(to, format = "%Y-%m-%d", tz = tz)
      df <- df %>%
        dplyr::filter(ts <= to)
    }

    if (length(last) != 0) {
      df <- last_values(df = df, last = last, reso = reso, tz = tz)
    }

    # skip series if there is not data available
    if (all(is.na(df$value))) {
      print(paste0("There is no data for '", series[i],
                   "' in specified period."))
      next
    }

    # save data to list
    server_data[[i]] <- df
    names(server_data)[i] <- series[i]
  }
  options(warn = 0)

  # remove empty list elements
  server_data <- Filter(f = length, x = server_data)

  # return error if no data is available
  if (length(server_data) == 0) {
    if (!use_intl) {
      stop(paste("There is no data available for the specified sensor(s):",
                 meta_series$series))
    }
    if (use_intl) {
      message(paste("There is no data available for the specified sensor(s):",
                    meta_series$series))
    }
  }

  if (!bind_df) {
    return(server_data)
  }
  if (bind_df) {
    df <- dplyr::bind_rows(server_data)
    return(df)
  }
}


#' Function to select last values
#'
#' \code{last_values()} selects specified number of values from current moment
#'   back in time.
#'
#' @param df input \code{data.frame}.
#' @inheritParams download_series
#' @inheritParams proc_L1
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
#'   series from the Decentlab servers.
#'
#' @param series_single character, name of the sensor to download.
#' @inheritParams select_series
#' @inheritParams proc_L1
#'
#' @keywords internal, treenet
#'
get_decentlab <- function(series_single, path_cred = NULL, tz) {

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
           " AND uqk =~ /", series_single, "/",
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
