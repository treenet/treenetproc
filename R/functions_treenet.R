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
#'   selection criteria specified by the user (i.e. measure_point, site_name, sensor_class, etc.).
#'   In addition, reference temperature data is selected for all dendrometer
#'   series.
#'
#' @param measure_point character, specify the measure point. String is not case sensitive.
#' @param site_name character, specify the site name. String is not case sensitive.
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
select_series <- function(measure_point, site_name, sensor_class, sensor_name, path_cred) {

  # Check availability of packages --------------------------------------------
  check_package(pck_name = "config")
  check_package(pck_name = "dplyr")
  require(magrittr, quietly = T, warn.conflicts = F)

  # Select series for download via metadata file ------------------------------
  # read metadata
  drv <- RPostgres::Postgres()
  cred <- config::get("treenet_cred", file = path_cred)
  con <- DBI::dbConnect(drv,
                        dbname = cred$dbname,
                        host = cred$host,
                        port = cred$port,
                        user = cred$user,
                        password = cred$password)
  meta <- DBI::dbGetQuery(paste0("SELECT * FROM view_metadata;"),
                          conn = con) %>%
    dplyr::mutate(id = dplyr::row_number())
  invisible(DBI::dbDisconnect(con))

  # select specified series from metadata file
  meta_filter <- meta$id
  warn_msg    <- vector()

  # site_name
  if (!is.null(site_name)) {
    site_name_sel    <- site_name
    meta_select <- vector()
    meta_msg    <- vector()
    for (t in 1:length(site_name_sel)) {
      meta_sub <- meta %>%
        dplyr::filter(id %in% meta_filter) %>%
        dplyr::filter(grepl(paste0(site_name_sel[t]), site_name, ignore.case = TRUE))

      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$id)
        # meta_airtemp <- unique(meta_sub$site_temp_ref)
      } else {
        meta_msg <- c(meta_msg, site_name_sel[t])
      }
    }
    if (length(meta_msg) > 0) warn_msg <- c(warn_msg, paste0("'", paste0(meta_msg, collapse = "', '"), "'"))
    meta_filter <- meta_select
  }
  # sensor_class
  if (!is.null(sensor_class)) {
    sensor_class_sel <- sensor_class
    meta_select      <- vector()
    meta_msg         <- vector()
    for (t in 1:length(sensor_class_sel)) {
      lookup <-
        c(Dendrometer             = "dendrometer",
          Dendrometer_X           = "dendrometer",
          Dendrometer_Root        = "dendrometer",
          Dendrometer_Branch      = "dendrometer",
          Dendrometer_Ref         = "dendrometer",
          Sapflow                 = "sapflow",
          Rad                     = "radiation",
          PAR                     = "radiation_par",
          RelH                    = "relativeairhumidity_relh",
          Wind_speed              = "windspeed",
          Wind_dir                = "winddirection",
          Ozone                   = "ozone",
          Air_pressure            = "airpressure",
          Temp                    = "airtemperature",
          Stem_temp               = "stemtemperature",
          StemCO2                 = "stemco2",
          StemCO2_soil            = "soilstemco2",
          Soil_temp               = "soiltemperatrue",
          Soil_WP                 = "soilwaterpotential_soilwp",
          Soil_WC                 = "soilwatercontent_soilwc",
          Soil_VWC                = "soilvolumetricwatercontent_soilvwc",
          Soil_DP                 = "soildielectricpermittivity_soildp",
          Soil_EC                 = "soilelectricalconductivity_soilec",
          Precipitation           = "precipitation",
          Precipitation_Invervall = "precipitationinterval")

      meta_sensor <- meta %>%
        dplyr::filter(id %in% meta_filter) %>%
        dplyr::mutate(Sensor_query = unname(lookup[sensor_class])) %>%
        dplyr::filter(grepl(paste0(sensor_class_sel[t]), Sensor_query,
                            ignore.case = TRUE))

      if (nrow(meta_sensor) != 0) {
        meta_select <- c(meta_select, meta_sub$id)
      } else {
        meta_msg <- c(meta_msg, sensor_class_sel[t])
      }
    }
    if (length(meta_msg) > 0) warn_msg <- c(warn_msg, paste0("'", paste0(meta_msg, collapse = "', '"), "'"))
    meta_filter <- meta_select
  }
  # sensor_name
  if (!is.null(sensor_name)) {
    sensor_name_sel <- sensor_name
    meta_msg        <- vector()
    meta_select     <- vector()
    for (t in 1:length(sensor_name_sel)) {
      meta_sub <- meta %>%
        dplyr::filter(id %in% meta_filter) %>%
        dplyr::filter(sensor_name %in% sensor_name_sel[t])
      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$id)
      } else {
        meta_msg <- c(meta_msg, sensor_name_sel[t])
      }
    }

    if (length(meta_msg) > 0) warn_msg <- c(warn_msg, paste0("'", paste0(meta_msg, collapse = "', '"), "'"))
    meta_filter <- meta_select
  }
  # measure_point
  if (!is.null(measure_point)) {
    measure_point_sel <- measure_point
    meta_select       <- vector()
    meta_msg          <- vector()
    for (t in 1:length(measure_point_sel)) {
      meta_sub <- meta %>%
        dplyr::filter(id %in% meta_filter) %>%
        dplyr::filter(grepl(paste0(measure_point_sel[t]), measure_point,
                            ignore.case = TRUE))
      if (nrow(meta_sub) != 0) {
        meta_select <- c(meta_select, meta_sub$id)
        # meta_airtemp <- unique(meta_sub$site_temp_ref)
      } else {
        meta_msg <- c(meta_msg, measure_point_sel[t])
      }
    }
    if (length(meta_msg) > 0) warn_msg <- c(warn_msg, paste0("'", paste0(meta_msg, collapse = "', '"), "'"))
    meta_filter <- meta_select
  }

  if (length(warn_msg) > 0) message("Selection criteria error: ", paste(warn_msg, collapse = " & ")," cannot be found.")
  meta_list <- list(meta, meta_filter)

  # Select reference temperature data for download via metadata file ----------
  meta_series_ref <- select_ref_data(meta_list = meta_list)

  return(meta_series_ref)
}


#' Select Reference Data for Selected Series
#'
#' \code{select_ref_data} selects reference temperature data as well as outlier, jump, and frost tolerances for dendrometer
#'   series based on the metadata file.
#'
#' @param meta_list list, containing the metadata file (first element) and
#'   all series specified for download from the server (second element).
#'
#' @keywords internal, treenet
#'
select_ref_data <- function(meta_list) {

  meta <- meta_list[[1]]
  meta_series <- meta_list[[2]]

  # select names of metadata
  meta_ref <- meta[meta_series, ] %>%
    dplyr::select(measure_point = measure_point,
                  series_id     = series_id,
                  series_start  = series_start,
                  series_stop   = series_stop,
                  site_id       = site_id,
                  sensor_name   = sensor_name,
                  site_temp_ref = site_temp_ref,
                  tol_out       = tree_proc_tol_out,
                  tol_jump      = tree_proc_tol_jump,
                  lowtemp       = tree_proc_frost_thr)

  return(meta_ref)
}


#' Download Series from Server
#'
#' \code{download_series} downloads specified series from TreeNet or
#'   Decentlab server
#'
#' @param meta_series \code{data.frame}, contains the names of the series to
#'   download including the reference temperature dataset for each series.
#' @param data_format character, select processing level of data. Can either
#'   be \code{"L0"} (i.e. raw data), \code{"L1"} (i.e time-aligned),
#'   \code{"L2"} (i.e. processed), \code{"LM"} (i.e. manually cleaned) or \code{"L2M"} (i.e. L2 and manually
#'   cleaned when available).
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
#' @param meteo logical, specify whether the site meteo dataset should be
#' downloaded along with specified data. Automatically enabled or disabled if
#' \code{"data_format"} ir \code{"L2M"} or \code{"L0"} data, respectively.
#' @param use_intl logical, specify whether this function is used
#'   internally on the server. Changes \code{stop()} error messages to
#'   \code{message()} only.
#' @inheritParams select_series
#' @inheritParams proc_L1
#'
#' @keywords internal, treenet
#'
download_series <- function(meta_series, data_format,
                            from, to, last, bind_df, reso, path_cred, server,
                            meteo, tz, use_intl) {

  # Check availability of packages --------------------------------------------
  check_package(pck_name = "sqldf")
  check_package(pck_name = "RPostgres")
  check_package(pck_name = "config")
  check_package(pck_name = "DBI")
  check_package(pck_name = "httr")
  check_package(pck_name = "lubridate")


  # Load credentials
  path_cred <- load_credentials(path_cred = path_cred)

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
    if (data_format == "L0")  { db_table  <- "data_all_l0"; meteo <- F }
    if (data_format == "L1")    db_table  <- "data_all_l1"
    if (data_format == "L2")    db_table  <- "data_dendro_l2"
    if (data_format == "LM")    db_table  <- "data_dendro_lm"
    if (data_format == "L2M") { db_table  <- "data_dendro_l2"; meteo <- T }
  }
  if (meteo) bind_df <- T
  if (server == "decentlab") {
    version_nm <- "L0"
  }

  # format time window
  if (length(from) == 0) {
    from <- "1970-01-01"
  }
  from <- as.POSIXct(as.character(from), format = "%Y-%m-%d", tz = tz)
  if (length(to) == 0) {
    to <- lubridate::today() %>% as.character()
  }
  to <- as.POSIXct(as.character(to), format = "%Y-%m-%d", tz = tz) +86399

  # download series
  options(warn = -1)

  # find unique meta_series take first start and last stop date per series_id
  # Assume all other metadata is identical in other rows
  meta_series <- meta_series %>% dplyr::group_by(series_id) %>%
    dplyr::mutate(start = min(as.POSIXct(series_start, format = "%d.%m.%Y", tz = tz),       na.rm=F),
                  stop =  max(as.POSIXct(series_stop,  format = "%d.%m.%Y", tz = tz)+86399, na.rm=F)) %>%
    dplyr::slice(1) %>% dplyr::ungroup()

  server_data <- vector("list", length = nrow(meta_series))
  for (i in 1:nrow(meta_series)) {
    if (server == "treenet") {

      # check time window request with available data
      start <- as.POSIXct(as.character(meta_series$start[i]), format = "%Y-%m-%d", tz = tz)
      if (is.na(start)) start <- as.POSIXct(as.character("1970-01-01"), format = "%Y-%m-%d", tz = tz)
      stop  <- as.POSIXct(as.character(meta_series$stop[i]),  format = "%Y-%m-%d", tz = tz)+86399
      if (is.na(stop)) stop <- lubridate::now(tz=tz)

      # too early
      if (to < start) {
        # writeLines(paste0("There is no data for '", meta_series$measure_point[i],
        #                   "' in specified period."))
        next
      }
      # too late
      if (from > stop) {
        # writeLines(paste0("There is no data for '", meta_series$measure_point[i],
        #                   "' in specified period."))
        next
      }

      start <- max(from, start)
      stop  <- min(to  , stop)

      # specify time window
      from.ts <- NULL
      to.ts   <- NULL
      db_time <- NULL
      if (length(start) != 0) {
        from.ts <- paste0(" ",db_table,".ts >= '", format(start, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "'::timestamp")
        db_time <- paste0(c(from.ts, to.ts), collapse = " AND")
      }
      if (length(stop) != 0) {
        to.ts   <- paste0(" ",db_table,".ts <= '", format(stop, "%Y-%m-%d %H:%M:%S", tz = "UTC"), "'::timestamp")
        db_time <- paste0(c(from.ts, to.ts), collapse = " AND")
      }
      if (length(last) != 0) {
        db_time <- paste0(" ORDER BY ts DESC LIMIT ", last)
      }


      drv <- RPostgres::Postgres()
      cred <- config::get("treenet_cred", file = path_cred)
      con <- DBI::dbConnect(drv,
                            dbname   = cred$dbname,
                            host     = cred$host,
                            port     = cred$port,
                            user     = cred$user,
                            password = cred$password)
      setUTC1()
      # # check newest data in treenet database
      # if (data_format %in% c("LM","L2M")) {
      #   new_dataset <- sqldf::sqldf(paste0("SELECT DISTINCT dataset FROM treenetm_summary WHERE series_id = ", meta_series$series_id[i], " ORDER BY dataset DESC;"),
      #                            connection = con)$dataset[1]
      #   if (is.na(new_dataset)) new_dataset <- "dummy.value"
      # }
      if (data_format == "L0")   db_version <- NULL
      if (data_format == "L1")   db_version <- NULL
      if (data_format == "L2")   db_version <- NULL
      if (data_format == "LM")   db_version <- NULL
      if (data_format == "L2M") {db_version <- NULL
      ts.max.LM <- sqldf::sqldf(paste0("SELECT until_lm FROM view_imported_dendro WHERE series_id = ", meta_series$series_id[i], " ;"),
                                connection = con)$until_lm
      ts.max.L2 <- sqldf::sqldf(paste0("SELECT until_l2 FROM view_imported_dendro WHERE series_id = ", meta_series$series_id[i], " ;"),
                                connection = con)$until_l2
      db_time.LM     <- NULL
      db_time.L2     <- NULL


      if (is.na(ts.max.L2)) message(paste0("There is no L2 data available for ", meta_series$measure_point[i], "."))
      if (is.na(ts.max.LM)) {
        message(paste0("There is no LM data available for ", meta_series$measure_point[i], ". Using L2 data."))
        if (meteo) {
          foo <- sqldf::sqldf(paste0("SELECT l.*, data_meteo_l2.*,
                            CASE WHEN data_meteo_l2.temp IS NULL THEN data_all_l1.value ELSE data_meteo_l2.temp END AS temperature
                            FROM (
                                SELECT series_id, ts, value
                                FROM ", db_table ,"
                                WHERE series_id = ", meta_series$series_id[i]," AND", db_time, "
                                ) l
                            LEFT JOIN data_meteo_l2 ON l.ts = data_meteo_l2.ts
                                           AND l.series_id = ", meta_series$series_id[i],"
                                           AND data_meteo_l2.site_id = ", meta_series$site_id[i], "
                            LEFT JOIN data_all_l1 ON l.ts = data_all_l1.ts
                                           AND data_all_l1.series_id = ", meta_series$site_temp_ref[i],
                                     dplyr::if_else(length(last) != 0, paste0(" WHERE ", db_time), ""), ";"),
                              connection = con)
        } else {
        foo <- sqldf::sqldf(paste0("SELECT * FROM  ", db_table ,"
                                   WHERE series_id = ", meta_series$series_id[i], " AND",
                                   db_time, ";"), connection = con)
        }
      } else {
        writeLines(paste0("Data from ", meta_series$measure_point[i], " is LM until ", format(ts.max.LM, "%Y-%m-%d %H:%M:%S"), " afterwhich it is L2."))

        if (length(from) == 0) from <- as.POSIXct("1970-01-01", format = "%Y-%m-%d", tz = tz)
        if (length(to)   == 0) to   <- as.POSIXct("2100-01-01", format = "%Y-%m-%d", tz = tz)

        db_time.LM <-paste0(" AND ts > '", as.POSIXct(min(from, ts.max.LM, na.rm = T), origin = "1970-01-01", tz = tz), "'::timestamp AND ts <= '", as.POSIXct(min(to,ts.max.LM, na.rm = T), origin = "1970-01-01", tz = tz), "'::timestamp")
        db_time.L2 <-paste0(" AND ts > '", as.POSIXct(max(from, ts.max.LM, na.rm = T), origin = "1970-01-01", tz = tz), "'::timestamp AND ts <= '", as.POSIXct(max(to,ts.max.LM, na.rm = T), origin = "1970-01-01", tz = tz), "'::timestamp")
        if (meteo) {
          foo <- sqldf::sqldf(paste0("SELECT l2m.*, data_meteo_l2.*,
                            CASE WHEN data_meteo_l2.temp IS NULL THEN data_all_l1.value ELSE data_meteo_l2.temp END AS temperature
                            FROM (
                                SELECT series_id, ts, value
                                FROM data_dendro_lm
                                WHERE series_id = ", meta_series$series_id[i]," ", db_time.LM, "
                                UNION
                                SELECT series_id, ts, value
                                FROM ", db_table ,"
                                WHERE series_id = ", meta_series$series_id[i]," ", db_time.L2, "
                                ) l2m
                            LEFT JOIN data_meteo_l2 ON l2m.ts = data_meteo_l2.ts
                                           AND l2m.series_id = ", meta_series$series_id[i],"
                                           AND data_meteo_l2.site_id = ", meta_series$site_id[i], "
                            LEFT JOIN data_all_l1 ON l2m.ts = data_all_l1.ts
                                           AND data_all_l1.series_id = ", meta_series$site_temp_ref[i],
                                     dplyr::if_else(length(last) != 0, paste0(" WHERE ", db_time), ""), ";"),
                              connection = con)

        } else {
          foo <- sqldf::sqldf(paste0("SELECT * FROM (
                                     SELECT series_id, ts, value
                                      FROM data_dendro_lm WHERE series_id = ", meta_series$series_id[i]," ", db_time.LM, "
                                     UNION
                                     SELECT series_id, ts, value
                                      FROM ", db_table ," WHERE series_id = ", meta_series$series_id[i]," ", db_time.L2,
                                     ") l2m ",
                                     dplyr::if_else(length(last) != 0, paste0(" WHERE ", db_time), ""), ";"),
                              connection = con)
        }
      }
      } else {
        if (meteo) {
          foo <- sqldf::sqldf(paste0("SELECT l.*, data_meteo_l2.*,
                            CASE WHEN data_meteo_l2.temp IS NULL THEN data_all_l1.value ELSE data_meteo_l2.temp END AS temperature
                            FROM (
                                SELECT series_id, ts, value
                                FROM ", db_table ,"
                                WHERE series_id = ", meta_series$series_id[i]," AND", db_time, "
                                ) l
                            LEFT JOIN data_meteo_l2 ON l.ts = data_meteo_l2.ts
                                           AND l.series_id = ", meta_series$series_id[i],"
                                           AND data_meteo_l2.site_id = ", meta_series$site_id[i], "
                            LEFT JOIN data_all_l1 ON l.ts = data_all_l1.ts
                                           AND data_all_l1.series_id = ", meta_series$site_temp_ref[i],
                                     dplyr::if_else(length(last) != 0, paste0(" WHERE ", db_time), ""), ";"),
                              connection = con)

        } else {
          foo <- sqldf::sqldf(paste0("SELECT * FROM ", db_table,
                                     " WHERE series_id = ", meta_series$series_id[i],
                                     paste0(c("", db_version,  db_time), collapse = " AND "), ";"), connection = con)
        }
      }
      invisible(DBI::dbDisconnect(con))
    }
    if (server == "decentlab") {
      series_single <- meta_series$sensor_name[i]
      foo <- get_decentlab(series_single = series_single,
                           path_cred = path_cred, tz = tz) %>%
        dplyr::select(series = uqk, ts = time, value) %>%
        transform(series = as.character(series))
    }

    df <- foo %>%
      dplyr::select_if(!(names(.) %in% "insert_date")) %>%
      transform(ts = lubridate::with_tz(ts, tzone = tz)) %>%
      dplyr::rename(series = series_id) %>% # !!!! Try to remove series rename ----
    dplyr::arrange(ts) %>%
      dplyr::distinct() %>%
      transform(value = as.numeric(value))

    # skip series if there is not data available
    if (all(is.na(df$value))) {
      writeLines(paste0("There is no data for '", meta_series$measure_point[i],
                        "' in specified period."))
      next
    }

    # save data to list
    server_data[[i]]      <- df
    names(server_data)[i] <- meta_series$measure_point[i]
  }
  options(warn = 0)

  # remove empty list elements
  server_data <- Filter(f = length, x = server_data)

  # return error if no data is available
  m_dendro <- names(server_data) %in% meta_series$measure_point
  if (length(server_data) == 0 | all(!m_dendro)) {
    if (!use_intl) {
      stop("There is no data available from the specified series.")
    }
    if (use_intl) {
      message("There is no data available from the specified series.")
      return(NULL)
    }
  }

  if (bind_df) {
    server_data <- dplyr::bind_rows(server_data)
  }

  if (meteo) {
    server_data <- server_data %>%
      dplyr::mutate(temp=temperature) %>%
      dplyr::select(-c("ts..4","site_id","temperature"))
  }

  # process with temperature data
  if (data_format == "L2M") {
    # if (any(m_temp) & any(m_dendro)) {
    temp.L1 <- try(proc_L1(server_data %>%
                             dplyr::select(ts, series, value = temp))
                   , silent=T)
    if ("try-error" %in% class(temp.L1)) {
      message("There are no temperature data available for this series")
      temp.L1 <- NULL
    }
    # } else {
    # temp.L1 <- NULL
    # }

    meta_series$tol_out [is.na(meta_series$tol_out)]  <- 10
    meta_series$tol_jump[is.na(meta_series$tol_jump)] <- 50
    meta_series$lowtemp [is.na(meta_series$lowtemp)]  <- 5

    data.L2 <- proc_dendro_L2(
      dendro_L1   = proc_L1(server_data %>% dplyr::select(ts, series, value)),
      temp_L1     = temp.L1,
      tol_out     = meta_series$tol_out [m_dendro],
      tol_jump    = meta_series$tol_jump[m_dendro],
      lowtemp     = meta_series$lowtemp [m_dendro],
      plot        = F,
      tz          = tz
    )

    # process growth start and end
    grostartend <- grow_seas(data.L2, tol_seas = 0.05, agg_yearly = T, tz = tz)

    # insert year
    data.L2 <- data.L2 %>%
      dplyr::mutate(year = strftime(ts, format = "%Y", tz = tz)) %>%
      dplyr::mutate(year = as.numeric(year))

    # join data
    server_data <- data.L2 %>%
      dplyr::full_join(server_data %>% dplyr::select(-c(value)), by=c("series","ts")) %>%
      dplyr::full_join(grostartend, by=c("series","year"))
  }
  return(server_data)
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
