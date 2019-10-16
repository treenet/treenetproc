## code to prepare `dendro_data_L0`, `dendro_data_L1` and `dendro_data_L2`
## datasets goes here

library(tidyverse)

# prepare L0 data -------------------------------------------------------------
### prepare jump data
jump_L0 <- download_treenet(sensor_name = "Beatenberg-0.dendrometer.ch1",
                            from = "2014-10-25", to = "2014-11-10",
                            data_format = "L0", server = "decentlab",
                            tz = "UTC") %>%
  mutate(value = value - 7000) %>%
  mutate(value = ifelse(value > 7000, value - 9000, value)) %>%
  mutate(series = "site-1_dendro-1")

# prepare outlier data
outlier_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch5",
                               from = "2012-07-12", to = "2012-07-29",
                               data_format = "L0", server = "decentlab",
                               tz = "UTC") %>%
  mutate(value = value + (last(jump_L0$value) - first(value)) + 5) %>%
  mutate(series = "site-1_dendro-1")

### prepare shrink data
shrink_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch1",
                              from = "2017-08-01", to = "2017-08-31",
                              data_format = "L0", server = "decentlab",
                              tz = "UTC") %>%
  mutate(series = "site-1_dendro-2")

### prepare delete data
delete_L0 <- download_treenet(sensor_name = "Lausanne-1.dendrometer.ch2",
                              from = "2018-03-20", to = "2018-03-23",
                              data_format = "L0", server = "decentlab",
                              tz = "UTC") %>%
  mutate(value = value * 0.1) %>%
  mutate(series = "site-1_dendro-3")

# prepare shrink 2 data
shrink_2_L0 <- download_treenet(sensor_name = "Pfynwald-02-11.dendrometer.ch0",
                                from = "2016-08-20", to = "2016-09-12",
                                data_format = "L0", server = "decentlab",
                                tz = "UTC") %>%
  mutate(value = value + (last(delete_L0$value) - first(value)) - 5) %>%
  mutate(series = "site-1_dendro-3")
shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-27" &
                    shrink_2_L0$ts <= "2016-08-28"] <- NA
shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-28"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-28"] - 490
shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-28 15:30:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-28 15:30:00"] + 215
shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-31 18:10:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2016-08-31 18:10:00"] + 33
shrink_2_L0$value[shrink_2_L0$ts >= "2016-09-02 18:30:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2016-09-02 18:30:00"] + 38
shrink_2_L0$value[shrink_2_L0$ts >= "2016-09-03 18:00:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2016-09-03 18:00:00"] + 42

### prepare frost data
frost_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch1",
                             from = "2013-03-10", to = "2013-03-30",
                             data_format = "L0", server = "decentlab",
                             tz = "UTC") %>%
  mutate(series = "site-1_dendro-4")


### merge L0 data
dendro_data_L0 <- bind_rows(list(jump_L0, outlier_L0, shrink_L0, delete_L0,
                                 shrink_2_L0, frost_L0))

# create continous timestamp column for site-1_dendro-1 and site-1_dendro-3
ts_dendro_1 <- download_treenet(sensor_name = "Beatenberg-0.dendrometer.ch1",
                                from = "2014-05-01", to = "2015-06-15",
                                data_format = "L0", server = "decentlab",
                                tz = "UTC") %>%
  select(ts) %>%
  slice(1:length(which(dendro_data_L0$series == "site-1_dendro-1")))
dendro_data_L0$ts[dendro_data_L0$series == "site-1_dendro-1"] <-
  ts_dendro_1[, 1]

ts_dendro_3 <-
  download_treenet(sensor_name = "Pfynwald-02-11.dendrometer.ch0",
                   from = as.POSIXct("2016-07-31 23:50:00",
                                     format = "%Y-%m-%d %H:%M:%S",
                                     tz = "UTC"),
                   to = "2016-08-31", data_format = "L0",
                   server = "decentlab") %>%
  select(ts) %>%
  slice(1:length(which(dendro_data_L0$series == "site-1_dendro-3")))
dendro_data_L0$ts[dendro_data_L0$series == "site-1_dendro-3"] <-
  ts_dendro_3[, 1]

# save L0 data
usethis::use_data(dendro_data_L0, overwrite = TRUE)


# prepare L0 data -------------------------------------------------------------
dendro_data_L1 <- proc_L1(data = dendro_data_L0, reso = 10, input = "long")

# save L1 data
usethis::use_data(dendro_data_L1, overwrite = TRUE)


# prepare L2 data -------------------------------------------------------------
dendro_data_L2 <- proc_dendro_L2(dendro_data = dendro_data_L1, plot = FALSE)

# save L2 data
usethis::use_data(dendro_data_L2, overwrite = TRUE)
