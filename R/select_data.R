#' Select Dendrometer and Temperature Data from Treenet Server
#'
#' \code{select_data} accesses and downloads dendrometer and temperature
#'   data from treenet server. Function imported and adapted from package
#'   \code{treenetdown}.
#'
#' @param site character, specify the site. String is not case sensitive.
#' @param sensor_class charcter, specify a single or multiple sensor
#'   classes. String does not have to contain the full name of the sensor
#'   class, i.e. \code{"dendro"} works for \code{"dendrometer"}. String is not
#'   case sensitive.
#' @param sensor_name character, specify the name of a single or
#'   multiple sensors (e.g. \code{"LWF-Demo-1.dendrometer.ch0"}) of the
#'   same site.
#' @param data_format character, select processing level of data. Can either
#'   be \code{"L0"} (i.e. raw data), \code{"L1"} (i.e time-aligned) or
#'   \code{"L2"} (i.e. processed).
#' @param from character, optional argument to select data after a specific
#'   date (\code{"YYYY-MM-DD"}).
#' @param to character, optional argument to select data up to a specific
#'   date (\code{"YYYY-MM-DD"}).
#' @param path_cred optional argument to specify the full path to the
#'   file \code{config.yml} containing the database login data. File can
#'   also be copied to the main folder of the package for automatic
#'   recognition.
#' @param temp_name character, optional argument to specify the series name
#'   of a temperature dataset. Needed if there are no or multiple on-site air
#'   temperature measurements. If \code{temp_name} is not specified and no
#'   temperature dataset is found on the server, a sample dataset will be
#'   used with a warning.
#' @param select_temp logical, specifies whether a temperature dataset should
#'   be automatically selected if the respective site contains temperature
#'   measurements.
#' @param last numeric, optional argument to specify the number of latest
#'   values. If \code{last} is specified \code{from} and \code{to} need to
#'   be empty.
#' @param export logical, indicate whether each sensor will be saved as
#'   an \code{.RData}-file in the current working directory
#'   (\code{export = TRUE}) or will be returned to the console.
#' @param bind_df logical, indicate whether data should be returned as a
#'   \code{data.frame} (\code{bind_df = TRUE}) or as a named list
#'   \code{bind_df = FALSE}.
#' @param server character, specify server from which data is downloaded.
#'   Can be either \code{treenet} or \code{decentlab}.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @return The output will be one \code{.RData} file for each sensor in case
#' \code{export = TRUE}. Otherwise data will be saved in a list.
#'
#' @keywords internal, treenet
#'
#' @examples
#' \dontrun{
#' select_data(site = "birmensdo")
#' }
#'
select_data <- function(site = NULL, sensor_class = NULL, sensor_name = NULL,
                        data_format = NULL, from = NULL, to = NULL,
                        path_cred = NULL, temp_name = NULL, select_temp = TRUE,
                        last = NULL, export = FALSE, bind_df = FALSE,
                        server = "treenet", reso = 10, tz = "Etc/GMT-1") {

  # Check input variables -----------------------------------------------------
  if (!(data_format %in% c("L0", "L1", "L2"))) {
    stop("'data_format' needs to be 'L0', 'L1' or 'L2'.")
  }
  if (length(data_format) > 1) {
    stop("You cannot download multiple 'data_formats' at once.")
  }
  if (length(site) == 0 & length(sensor_name) == 0) {
    stop(paste0("Specify at least one of the following: site, ",
                " sensor_name."))
  }
  if (length(site) > 1) {
    stop("You can only specify one site at a time.")
  }
  if (length(site) != 0 & length(sensor_name) != 0) {
    if (!grepl(site, sensor_name, ignore.case = TRUE)) {
      stop("Site and sensor_name need to correspond.")
    }
  }
  if (length(from) != 0) {
    if (is.na(as.Date(from, format = "%Y-%m-%d"))) {
      stop("Provide 'from' in date format. Format needs to be 'YYYY-MM-DD'.")
    }
  }
  if (length(to) != 0) {
    if (is.na(as.Date(to, format = "%Y-%m-%d"))) {
      stop("Provide 'to' in date format. Format needs to be 'YYYY-MM-DD'.")
    }
  }
  if (length(temp_name) > 1) {
    stop("Provide single temperature dataset with 'temp_name'.")
  }
  check_logical(var = export, var_name = "export")
  check_logical(var = bind_df, var_name = "bind_df")
  if (!(server %in% c("treenet", "decentlab"))) {
    stop("server needs to be either 'treenet' or 'decentlab'.")
  }
  if (server == "decentlab" & data_format != "L0") {
    stop(paste("only 'L0' data can be downloaded from the decentlab server.",
               "Change 'data_format' to 'L0'."))
  }


  # Database credentials ------------------------------------------------------
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
  }


  # Load functions ------------------------------------------------------------
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


  # Specify series for download via metadata file -----------------------------
  Sys.setenv(TZ = tz)
  options(warn = -1)

  spread <- suppressMessages(googlesheets::gs_title("Metadata"))
  meta <- suppressMessages(as.data.frame(
    googlesheets::gs_read(ss = spread, ws = "Metadata", progress = FALSE)))

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
      meta_sensor <- meta %>%
        dplyr::filter(Seriesname %in% meta_filter) %>%
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
  # select temperature data
  if (select_temp) {
    if (length(temp_name) == 0) {
      if (length(meta_airtemp) == 0) {
        if (length(sensor_name) == 0) {
          meta_airtemp <- vector()
          meta_sub <- meta %>%
            dplyr::filter(Seriesname %in% meta_filter) %>%
            dplyr::filter(grepl("airtemperature", Sensor_query,
                                ignore.case = TRUE))
        }
        if (length(sensor_name) != 0) {
          meta_airtemp <- vector()
          meta_sub <- meta %>%
            dplyr::filter(Seriesname %in% meta_filter)
          temp_site <- unique(meta_sub$Site)
          if (length(temp_site) > 1) {
            stop("you can only select sensors from one site at a time.")
          }
          meta_sub <- meta %>%
            dplyr::filter(Site %in% temp_site) %>%
            dplyr::filter(grepl("airtemperature", Sensor_query,
                                ignore.case = TRUE))
        }
        if (nrow(meta_sub) == 1) {
          meta_airtemp <- meta_sub$Seriesname
        }
        if (nrow(meta_sub) > 1) {
          stop("Multiple air temperature datasets found. Please specify the ",
               "temperature sensor name explicitly in 'temp_name'.")
        }
      }
    } else {
      meta_sub <- meta %>%
        dplyr::filter(grepl(temp_name, Seriesname, ignore.case = TRUE))

      if (nrow(meta_sub) == 1) {
        meta_airtemp <- meta_sub$Seriesname
      }
      if (nrow(meta_sub) == 0) {
        stop(paste0("No air temperature data with 'temp_name = ",
                    temp_name, " found. Provide a valid 'temp_name'."))
      }
    }
    if (length(meta_airtemp) != 0) {
      if (bind_df && data_format == "L0") {
        meta_filter <- c(meta_filter, meta_airtemp)
      }
    }
  }

  # specify format
  if (server == "treenet") {
    if (data_format == "L0") {
      db_folder <- "treenet0"
      db_version <- "version = 0;"
      select_col <- c("series", "ts", "value")
      version_nm <- "L0"
    }
    if (data_format == "L1") {
      db_folder <- "treenet1"
      db_version <- "version = 1;"
      select_col <- c("series", "ts", "value")
      version_nm <- "L1"
    }
    if (data_format == "L2") {
      db_folder <- "treenet2"
      db_version <- "version = 2;"
      select_col <- NA
      version_nm <- "L2"
    }
  }
  if (server == "decentlab") {
    select_col <- NA
    version_nm <- "L0"
  }


  # Download series -----------------------------------------------------------
  server_data <- vector("list", length = length(meta_filter))
  for (i in 1:length(meta_filter)) {
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
                                 " where series = '", meta_filter[i], "' AND ",
                                 db_version), connection = con)
      invisible(DBI::dbDisconnect(con))
    }
    if (server == "decentlab") {
      meta_single <- meta_filter[i]
      foo <- get_decentlab(meta_single = meta_single,
                           path_cred = path_cred, tz = tz) %>%
        dplyr::select(series = uqk, ts = time, value) %>%
        transform(series = as.character(series))
    }

    df <- foo %>%
      dplyr::select(match(select_col, names(.))) %>%
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
      print(paste0("There is no data for '", meta_filter[i],
                   "' in specified period."))
      next
    }

    if (export) {
      save(df, file = paste0(meta_filter[i], "_", version_nm, ".RData"),
           compress = "xz")
    } else {
    server_data[[i]] <- df
    names(server_data)[i] <- meta_filter[i]
    }
  }
  options(warn = 0)

  # remove empty list elements
  server_data <- Filter(f = length, x = server_data)

  if (!export && !bind_df) {
    return(server_data)
  }

  if (bind_df && data_format == "L0") {
    df <- dplyr::bind_rows(server_data) %>%
      dplyr::select(series, ts, value)

    return(df)
  }
}
