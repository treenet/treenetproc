## code to prepare `temp_data_L1` datasets goes here

library(tidyverse)


# prepare L0 data -------------------------------------------------------------

### frost data
frost <- download_treenet(sensor_name = "Jussy-2.sht-temperature.ch0",
                          from = "2019-02-10", to = "2019-03-10",
                          data_format = "L0", server = "decentlab",
                          tz = "UTC") %>%
  mutate(series = "site-1_temperature")

data("dendro_data_L0")
data_L0 <- dendro_data_L0 %>%
  filter(series == "site-1_dendro-4") %>%
  select(ts)

# add timestamp of dendrometer data
frost <- frost %>%
  slice(1:nrow(data_L0))

frost$ts <- data_L0$ts

# shift temperature data to fit dendrometer data
frost_L0 <- frost %>%
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

### extend temperature dataset
temp_L0 <- download_treenet(sensor_name = "Pfynwald-02-12.sht-temperature.ch3",
                                 from = "2016-03-30", to = "2018-12-31",
                                 data_format = "L0", server = "decentlab",
                                 tz = "UTC") %>%
  mutate(ts = ts - 3 * 31556952) %>%
  mutate(series = "site-1_temperature")

### merge temperature data
temp_data_L0 <- bind_rows(frost_L0, temp_L0) %>%
  arrange(ts)

# save L0 data
usethis::use_data(temp_data_L0, overwrite = TRUE)


# prepare L1 data -------------------------------------------------------------
temp_data_L1 <- proc_L1(data = temp_data_L0, reso = 10, input = "long")

# save L1 data
usethis::use_data(temp_data_L1, overwrite = TRUE)
