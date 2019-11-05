## code to plot .jpg figures used in vignette "Functionality-of-treenetproc"

library(tidyverse)

# Figure outlier and jump correction thresholds -------------------------------

# download dendrometer data
dendro_L1 <- download_treenet(sensor_name = "Bachtel-2.dendrometer.ch0",
                              data_format = "L1", from = "2014-01-01",
                              to = "2018-12-31") %>%
  mutate(series = "dendro1_site1") %>%
  mutate(temp_ref = "temp_site1")

# download temperature data
temp_L1 <- download_treenet(sensor_name = "Bachtel-0.sht-temperature.ch1",
                            data_format = "L1", from = "2014-01-01",
                            to = "2018-12-31") %>%
  mutate(series = "temp_site1")


# plot density distribution of diff
passenv$sample_temp <- FALSE
passenv$reso <- 10
df <- createfrostflag(df = dendro_L1, tem = temp_L1, lowtemp = 5,
                      sample_temp = FALSE)
df <- calcdiff(df = df, reso = 10)
df <- createflagmad(df = df, reso = 10, wnd = NULL,
                    tol = 10, save_thr = TRUE,
                    correction = "outlier", frost_thr = 5)
# save density plots to vignette folder
setwd(paste0(getwd(), "/vignettes"))
df <- executeflagout(df = df, len = 1, frag_len = NULL,
                     plot_density = TRUE, plot_export = TRUE,
                     frost_thr = 5)

# !!! Plots need to be converted to JPG to be included in vignette


