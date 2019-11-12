## code to prepare `dendro_data_L0`, `dendro_data_L1` and `dendro_data_L2`
## datasets goes here

library(tidyverse)


# prepare L0 data -------------------------------------------------------------
### prepare jump data
jump_L0 <- download_treenet(sensor_name = "Beatenberg-0.dendrometer.ch1",
                            from = "2014-10-25", to = "2014-11-10",
                            data_format = "L0", server = "decentlab",
                            tz = "UTC") %>%
  # set start of ts to 2013-05-01
  mutate(ts = ts - 46828800) %>%
  mutate(value = value - 7000) %>%
  mutate(value = ifelse(value > 7000, value - 7000, value)) %>%
  mutate(series = "site-1_dendro-1")

# prepare outlier data
outlier_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch5",
                               from = "2012-07-12", to = "2012-07-29",
                               data_format = "L0", server = "decentlab",
                               tz = "UTC") %>%
  # set start of ts to 2013-05-17
  mutate(ts = ts + 26697600) %>%
  mutate(value = value + (last(jump_L0$value) - first(value)) + 5) %>%
  mutate(series = "site-1_dendro-1")

# download multiyear dendrometer data
dendro_L0 <- download_treenet(sensor_name = "Pfynwald-02-11.dendrometer.ch0",
                            from = "2016-06-03", to = "2018-12-31",
                            data_format = "L0", server = "decentlab",
                            tz = "UTC") %>%
  # set to year 2013
  mutate(ts = ts - 3 * 31556952) %>%
  mutate(value = value + (last(outlier_L0$value) - first(value)) + 0.3) %>%
  mutate(series = "site-1_dendro-1")


### prepare shrink data
shrink_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch1",
                              from = "2017-08-01", to = "2017-08-31",
                              data_format = "L0", server = "decentlab",
                              tz = "UTC") %>%
  # set ts to year 2013
  mutate(ts = ts - 4 * 31556952) %>%
  mutate(series = "site-1_dendro-2")


### prepare delete data
delete_L0 <- download_treenet(sensor_name = "Lausanne-1.dendrometer.ch2",
                              from = "2018-03-20", to = "2018-03-23",
                              data_format = "L0", server = "decentlab",
                              tz = "UTC") %>%
  # set start of ts to 2013-08-01
  mutate(ts = ts - 146188800) %>%
  mutate(value = value * 0.1) %>%
  mutate(series = "site-1_dendro-3")

# prepare shrink 2 data
shrink_2_L0 <- download_treenet(sensor_name = "Pfynwald-02-11.dendrometer.ch0",
                                from = "2016-08-20", to = "2016-09-12",
                                data_format = "L0", server = "decentlab",
                                tz = "UTC") %>%
  # set start of ts to 2013-08-04
  mutate(ts = ts - 96076800) %>%
  mutate(value = value + (last(delete_L0$value) - first(value)) - 5) %>%
  mutate(series = "site-1_dendro-3")

shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-11" &
                    shrink_2_L0$ts <= "2013-08-12"] <- NA
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-12"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-12"] - 490
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:10:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:10:00"] + 10
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:20:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:20:00"] + 15
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:30:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:30:00"] + 25
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:40:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:40:00"] + 35
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:50:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 17:50:00"] + 20
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 18:00:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-15 18:00:00"] + 10
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-17 18:30:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-17 18:30:00"] + 10
shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-18 18:00:00"] <-
  shrink_2_L0$value[shrink_2_L0$ts >= "2013-08-18 18:00:00"] + 25

### prepare frost data
frost_L0 <- download_treenet(sensor_name = "Jussy-1.dendrometer.ch1",
                             from = "2013-03-10", to = "2013-03-30",
                             data_format = "L0", server = "decentlab",
                             tz = "UTC") %>%
  mutate(series = "site-1_dendro-4")

# increase steepness of frost shrinkage
frost_L0$value[frost_L0$ts >= "2013-03-16 02:00:00" &
                 frost_L0$ts <= "2013-03-16 06:00:00"] <-
  frost_L0$value[frost_L0$ts >= "2013-03-15 21:00:00" &
                   frost_L0$ts <= "2013-03-16 01:00:00"]

frost_L0$value[frost_L0$ts >= "2013-03-16 06:00:00" &
                 frost_L0$ts <= "2013-03-16 06:30:00"] <- NA

frost_L0 <- frost_L0 %>%
  mutate(value = ifelse(is.na(value), approx(ts, value, ts)$y,
                        value))

### merge L0 data
dendro_data_L0 <- bind_rows(list(jump_L0, outlier_L0, dendro_L0, shrink_L0,
                                 delete_L0, shrink_2_L0, frost_L0))

# save L0 data
usethis::use_data(dendro_data_L0, overwrite = TRUE)


# prepare L1 data -------------------------------------------------------------
dendro_data_L1 <- proc_L1(data = dendro_data_L0, reso = 10, input = "long")

# save L1 data
usethis::use_data(dendro_data_L1, overwrite = TRUE)


# prepare L2 data -------------------------------------------------------------
data("temp_data_L1")
dendro_data_L2 <- proc_dendro_L2(dendro_data = dendro_data_L1,
                                 temp_data = temp_data_L1, plot = FALSE)

# save L2 data
usethis::use_data(dendro_data_L2, overwrite = TRUE)
