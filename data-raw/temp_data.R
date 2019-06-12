## code to prepare `temp_data_L1` datasets goes here

library(tidyverse)

# prepare L0 data -------------------------------------------------------------
temp_data_L0 <- download_treenet(sensor_name = "Jussy-2.sht-temperature.ch0",
                                 from = "2019-02-10", to = "2019-03-10",
                                 data_format = "L0", server = "decentlab",
                                 tz = "UTC") %>%
  mutate(series = "site-1_tempearture")

data("dendro_data_L0")
data_L0 <- dendro_data_L0 %>%
  filter(series == "site-1_dendro-4") %>%
  select(ts)

# add timestamp of dendrometer data
temp_data_L0 <- temp_data_L0 %>%
  slice(1:nrow(data_L0)) %>%
  select(-version)

temp_data_L0$ts <- data_L0$ts

# shift temperature data to fit dendrometer data
temp_data_L0_2 <- temp_data_L0 %>%
  mutate(value = ifelse(ts <= "2013-03-12 05:00:00", value + 8, value)) %>%
  mutate(value = ifelse(ts >= "2013-03-12 05:00:00" &
                          ts <= "2013-03-14 05:00:00",
                        value * 0.8 + 6, value)) %>%
  mutate(value = ifelse(ts >= "2013-03-14 05:00:00" &
                          ts <= "2013-03-14 23:00:00",
                        value * 0.5 + 3.5, value)) %>%
  mutate(value = ifelse(ts > "2013-03-14 23:00:00" & ts < "2013-03-19",
                        value * 0.3, value)) %>%
  mutate(value = ifelse(ts >= "2013-03-19", value + 3, value)) %>%
  mutate(value = ifelse(ts >= "2013-03-19" & ts <= "2013-03-22",
                        value * 0.6, value))

# save L0 data
usethis::use_data(temp_data_L0, overwrite = TRUE)

# prepare L1 data -------------------------------------------------------------
temp_data_L1 <- proc_L1(data = temp_data_L0, reso = 10, input = "long")

# save L1 data
usethis::use_data(temp_data_L1, overwrite = TRUE)
