library(mgcv)
library(mgcViz)
library(readr)
library(lubridate)
library(dplyr)
source("R/combine_data_funcs.R")

GE_dat <- load_region_ave("Grand Est")

names(GE_dat)

form1 <- prod_div_cap ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc", k = 10) +
  s(capacity) +
  s(windspeed10) + s(direction10) + ti(windspeed10, direction10) +
  s(windspeed100) + s(direction100) + ti(windspeed100, direction100) +
  ti(windspeed10, windspeed100)

gam_fit1 <- bam(
  form1,
  data = GE_dat,
  discrete = TRUE,
  family = scat,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(gam_fit1, allTerms = TRUE), pages = 1)

summary(gam_fit1)

plotRGL(sm(gam_fit1, 9))

preds <- predict(gam_fit1, type = "response")



####################################

GE_convex <- read_csv("temp/Grand.Est_convex.csv",
                      locale = locale(tz = "UTC"))

GE_convex <- combine_data("Grand Est", GE_convex)


gam_fit2 <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  family = scat,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()



######################################

GE_concave <- read_csv("temp/Grand.Est_concave.csv",
                      locale = locale(tz = "UTC"))

GE_concave <- combine_data("Grand Est", GE_concave)



gam_fit3 <- bam(
  form1,
  data = GE_concave,
  discrete = TRUE,
  family = scat,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(gam_fit2, allTerms = TRUE), pages = 1)

summary(gam_fit1)
summary(gam_fit2)
summary(gam_fit3)
