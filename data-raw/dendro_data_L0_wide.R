## code to prepare `dendro_L0_wide` dataset goes here

library(tidyverse)

# dendro_L0_wide --------------------------------------------------------------
dendro_L0_wide <- download_treenet(site = "beatenberg", from = "2013-05-01",
                                   to = "2013-05-02", server = "decentlab",
                                   data_format = "L0", tz = "UTC")

dendro_data_L0_wide <- dendro_L0_wide %>%
  filter(ts >= "2013-05-01 14:55:00") %>%
  filter(ts <= "2013-05-01 17:05:00") %>%
  filter(series != "Beatenberg-0.mps2-waterpotential.ch0") %>%
  select(-version) %>%
  spread(key = series, value = value) %>%
  rename("site-1_dendro-1" = "Beatenberg-0.dendrometer.ch1",
         "site-1_dendro-2" = "Beatenberg-0.dendrometer.ch2",
         "site-1_temperature" = "Beatenberg-0.mps2-temperature.ch0") %>%
  select(ts, "site-1_dendro-1", "site-1_dendro-2", "site-1_temperature")
dendro_data_L0_wide$'site-1_temperature'[1] <- 10.8

usethis::use_data(dendro_data_L0_wide, compress = "bzip2", overwrite = TRUE)
