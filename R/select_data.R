#' Select Dendrometer and Temperature Data from Treenet Server
#'
#' \code{select_data()} accesses and downloads dendrometer and temperature
#'   data from treenet server. Function imported and adapted from package
#'   \code{treenetdown}.
#'
#' @param site character string specifying the site. String is not
#'   case sensitive.
#' @param sensor_name character string specifying the name of a single or
#'   multiple sensors (e.g. \code{"LWF-Demo-1.dendrometer.ch0"}) of the
#'   same site.
#' @param from optional argument to select data after a specific date
#'   (\code{"YYYY-MM-DD"}).
#' @param to Optional argument to select data up to a specific date
#'   (\code{"YYYY-MM-DD"}).
#' @param path_cred optional argument to specify the full path to the
#'   file \code{config.yml} containing the database login data. File can
#'   also be copied to the main folder of the package for automatic
#'   recognition.
#' @param temp_name optional argument to specify the series name of a
#'   temperature dataset. Needed if there are no or multiple on-site air
#'   temperature measurements. If \code{temp_name} is not specified and no
#'   temperature dataset is found on the server, a sample dataset will be
#'   used with a warning.
#' @inheritParams proc_L1
#' @inheritParams proc_dendro_L2
#'
#' @keywords internal
#'
#' @examples
#' \dontrun{
#' select_data(site = "birmensdo")
#' }
#'
select_data <- function(site = NULL, sensor_name = NULL,
                        from = NULL, to = NULL, path_cred = NULL,
                        temp_name = NULL) {

  # Check input variables -----------------------------------------------------
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
    if (temp != "Etc/GMT-1") {
      xx <- sqldf::sqldf("SET TIME ZONE 'Etc/GMT-1'", connection = con)
    }
    temp <- as.character(sqldf::sqldf("show timezone", connection = con))
    if (temp != "Etc/GMT-1") {
      stop("Error with timestamp in database.")
    }
  }


  # Specify series for download via metadata file -----------------------------
  Sys.setenv(TZ = "Etc/GMT-1")
  options(warn = -1)

  spread <- suppressMessages(googlesheets::gs_title("Metadata"))
  meta <- suppressMessages(as.data.frame(
    googlesheets::gs_read(ss = spread, ws = "Metadata", progress = FALSE)))

  meta_filter <- meta$Seriesname
  # site
  if (!is.null(site)) {
    meta_select <- vector()
    for (t in 1:length(site)) {
      meta_sub <- meta %>%
        dplyr::filter(Seriesname %in% meta_filter) %>%
        dplyr::filter(grepl(paste0(site[t]), Site, ignore.case = TRUE))

      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$Seriesname)
      } else {
        stop(paste0("Site '", site[t], "' does not exist."))
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
  sample_temp <- FALSE
  if (length(temp_name) == 0) {
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
    if (nrow(meta_sub) == 0) {
      message("No air temperature data found. Sample temperature dataset ",
              "will be used.")
      sample_temp <- TRUE
    }
    if (nrow(meta_sub) > 1) {
      stop("Multiple air temperature datasets found. Please specify the ",
           "temperature sensor name explicitly in 'temp_name'.")
    }
    } else {
      meta_airtemp <- vector()
      meta_sub <- meta %>%
        dplyr::filter(grepl(temp_name, Sensor_query, ignore.case = TRUE))

      if (nrow(meta_sub) == 1) {
        meta_airtemp <- meta_sub$Seriesname
      }
      if (nrow(meta_sub) == 0) {
        stop(paste0("No air temperature data with 'temp_name = ",
                    temp_name, " found. Provide a valid 'temp_name'."))
      }
    }
  # select dendrometers
  meta_select <- vector()
  meta_sub <- meta %>%
    dplyr::filter(Seriesname %in% meta_filter) %>%
    dplyr::filter(grepl("dendrometer", Sensor_query,
                        ignore.case = TRUE))

  if (nrow(meta_sub) != 0) {
    meta_select <- c(meta_select, meta_sub$Seriesname)
    } else {
      stop("No dendrometer data found.")
    }
  meta_filter <- meta_select
  if (length(meta_airtemp) != 0) {
    meta_filter <- c(meta_filter, meta_airtemp)
  }

  # format (always L0)
  db_folder <- "treenet0"
  db_version <- "version = 0;"
  select_col <- c("series", "ts", "value")
  version_nr <- 0


  # Download series -----------------------------------------------------------
  treenet_data <- vector("list", length = length(meta_filter))
  for (i in 1:length(meta_filter)) {
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

    df <- foo %>%
      dplyr::select(match(select_col, names(.))) %>%
      transform(ts = as.POSIXct(ts, format = "%m-%d-%y %H:%M:%S",
                                tz = "Etc/GMT-1")) %>%
      dplyr::arrange(ts) %>%
      dplyr::distinct() %>%
      dplyr::filter(ts <= Sys.time()) %>%
      transform(value = as.numeric(value))

    treenet_data[[i]] <- df
  }

  options(warn = 0)
  df <- dplyr::bind_rows(treenet_data) %>%
    dplyr::mutate(version = version_nr) %>%
    dplyr::select(series, ts, value, version)

  if (sample_temp) {
    tem <- create_temp_dummy_treenet(df = df)
    df <- dplyr::bind_rows(df, tem)
  }

  return(df)
}
