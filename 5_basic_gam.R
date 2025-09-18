library(mgcv)
library(mgcViz)
library(readr)
library(lubridate)
source("R/combine_data_funcs.R")

GE_dat <- load_region_ave("Grand Est")

GE_dat$prod_div_cap <- GE_dat$production / GE_dat$capacity

GE_dat$time <- format(GE_dat$datetime, format = "%H:%M")
GE_dat$toy <- yday(GE_dat$datetime)
GE_dat$tod <- hour(GE_dat$datetime) + minute(GE_dat$datetime)/60
GE_dat$trend <- as.numeric(difftime(GE_dat$datetime, GE_dat$datetime[1], units = "days"))


GE_dat$windspeed10 <- sqrt(GE_dat$u10^2 + GE_dat$v10^2)
GE_dat$windspeed100 <- sqrt(GE_dat$u100^2 + GE_dat$v100^2)

GE_dat$direction10 <- atan2(GE_dat$u10, GE_dat$v10) + 180 / pi + 180
GE_dat$direction100 <- atan2(GE_dat$u100, GE_dat$v100) + 180 / pi + 180

names(GE_dat)

form1 <- prod_div_cap ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc", k = 10) + capacity +
  s(windspeed100) + s(direction100, bs = "cc")

gam_fit1 <- bam(
  form1,
  data = GE_dat,
  discrete = TRUE,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(gam_fit1, allTerms = TRUE), pages = 1)

summary(gam_fit1)

preds <- predict(gam_fit1, type = "response")



####################################

GE_convex <- read_csv("temp/Grand.Est_convex.csv",
                      locale = locale(tz = "UTC"))

GE_convex <- combine_data("Grand Est", GE_convex)

GE_convex$prod_div_cap <- GE_convex$production / GE_convex$capacity


GE_convex$time <- format(GE_convex$datetime, format = "%H:%M")
GE_convex$toy <- yday(GE_convex$datetime)
GE_convex$tod <- hour(GE_convex$datetime) + minute(GE_convex$datetime)/60
GE_convex$trend <- as.numeric(difftime(GE_convex$datetime, GE_convex$datetime[1], units = "days"))


GE_convex$windspeed10 <- sqrt(GE_convex$u10^2 + GE_convex$v10^2)
GE_convex$windspeed100 <- sqrt(GE_convex$u100^2 + GE_convex$v100^2)

GE_convex$direction10 <- atan2(GE_convex$u10, GE_convex$v10) + 180 / pi + 180
GE_convex$direction100 <- atan2(GE_convex$u100, GE_convex$v100) + 180 / pi + 180


gam_fit2 <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()



######################################

GE_concave <- read_csv("temp/Grand.Est_concave.csv",
                      locale = locale(tz = "UTC"))

GE_concave <- combine_data("Grand Est", GE_concave)

GE_concave$prod_div_cap <- GE_concave$production / GE_concave$capacity

GE_concave$time <- format(GE_concave$datetime, format = "%H:%M")
GE_concave$toy <- yday(GE_concave$datetime)
GE_concave$tod <- hour(GE_concave$datetime) + minute(GE_concave$datetime)/60
GE_concave$trend <- as.numeric(difftime(GE_concave$datetime, GE_concave$datetime[1], units = "days"))


GE_concave$windspeed10 <- sqrt(GE_concave$u10^2 + GE_concave$v10^2)
GE_concave$windspeed100 <- sqrt(GE_concave$u100^2 + GE_concave$v100^2)

GE_concave$direction10 <- atan2(GE_concave$u10, GE_concave$v10) + 180 / pi + 180
GE_concave$direction100 <- atan2(GE_concave$u100, GE_concave$v100) + 180 / pi + 180


gam_fit3 <- bam(
  form1,
  data = GE_concave,
  discrete = TRUE,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(gam_fit2, allTerms = TRUE), pages = 1)

summary(gam_fit1)
summary(gam_fit2)
summary(gam_fit3)
