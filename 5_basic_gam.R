library(mgcv)
library(mgcViz)
library(readr)

wind_dat_GE <- read_csv("Wind_data/Regional_wind_data/Grand.Est_wind_data.csv",
         locale = locale(tz = "UTC"))

GE_dat <- combine_data("Grand Est", wind_dat_GE)

GE_dat$time <- format(GE_dat$datetime, format = "%H:%M")
GE_dat$toy <- yday(GE_dat$datetime)
GE_dat$tod <- hour(GE_dat$datetime) + minute(GE_dat$datetime)/60
GE_dat$trend <- as.numeric(difftime(GE_dat$datetime, GE_dat$datetime[1], units = "days"))


GE_dat$windspeed10 <- sqrt(GE_dat$u10^2 + GE_dat$v10^2)
GE_dat$windspeed100 <- sqrt(GE_dat$u100^2 + GE_dat$v100^2)

GE_dat$direction10 <- atan2(GE_dat$u10, GE_dat$v10) + 180 / pi + 180
GE_dat$direction100 <- atan2(GE_dat$u100, GE_dat$v100) + 180 / pi + 180

names(GE_dat)

form1 <- production ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc") + capacity + 
  s(windspeed100) + s(direction100, bs = "cc")

gam_fit1 <- bam(
  form1, 
  data = GE_dat,
  discrete = TRUE
) |> 
  getViz()

print(plot(gam_fit1, allTerms = TRUE), pages = 1)

summary(gam_fit1)

preds <- predict(gam_fit1, type = "response")
