library(mgcv)
library(mgcViz)
library(readr)
library(lubridate)
library(dplyr)
library(tidyr)
source("R/combine_data_funcs.R")
source("R/resids_funcs.R")



GE_convex <- read_csv("temp/convex_hull_reg_ave/Grand.Est_wind_data 1.csv",
                      locale = locale(tz = "UTC"))

GE_convex <- combine_data("Grand Est", GE_convex)


form1 <- prod_div_cap ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc", k = 10) +
  s(capacity) +
  s(windspeed10) + s(direction10) + ti(windspeed10, direction10) +
  s(windspeed100) + s(direction100) + ti(windspeed100, direction100) +
  ti(windspeed10, windspeed100)



gam_fit <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(gam_fit, allTerms = TRUE), pages = 1)

resids_mat <- get_resids_mat(GE_convex, gam_fit)

qq.gam(gam_fit, rep = 1)

##########################
# log-link

ll_fit <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  argGam = list(knots=list(tod=seq(0,24,length=9))),
                family=gaussian(link="log")
) |>
  getViz()

print(plot(ll_fit, allTerms = TRUE), pages = 1)

resids_mat <- get_resids_mat(GE_convex, ll_fit)

qq.gam(ll_fit, rep = 1)



##########################
# scat

scat_fit <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  family = scat,
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(scat_fit, allTerms = TRUE), pages = 1)

resids_mat <- get_resids_mat(GE_convex, scat_fit)

qq.gam(scat_fit, rep = 1)

##########################
# scat log-link

ll_scat_fit <- bam(
  form1,
  data = GE_convex,
  discrete = TRUE,
  family = scat(link = log),
  argGam = list(knots=list(tod=seq(0,24,length=9)))
) |>
  getViz()

print(plot(ll_scat_fit, allTerms = TRUE), pages = 1)

resids_mat <- get_resids_mat(GE_convex, ll_scat_fit)

qq.gam(ll_scat_fit, rep = 1)



AIC(gam_fit, ll_fit, scat_fit, ll_scat_fit)



#############################
# NCV

# nei <- list()
#
# nei$d <- order(GE_convex$datetime)
# nei$md <- order(GE_convex$datetime)

# a needs to be every neighbourhood in order
# ma needs to be the end points of each neighbourhood

# a <- c()
# ma <- c(0)
#
# # takes a while
# for (i in 1:nrow(GE_convex)) {
#
#   datetime_i <- GE_convex$datetime[i]
#   nei_i <- which(abs(difftime(GE_convex$datetime, datetime_i, units = "hours")) <= 24)
#
#   a <- c(a, nei_i)
#   ma <- c(ma, ma[i] + length(nei_i))
# }
#
# nei$a <- a
# nei$ma <- ma[-1]
#

# save(nei, file = "temp/nei_GE_convex.RData")
load("temp/nei_GE_convex.RData")

# nei$sample <- 10000


# bam seems to crash when I try on a small subset
form2 <- prod_div_cap ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc", k = 10) +
  s(capacity) + s(windspeed100) + s(direction100)


gc <- gam.control(ncv.threads=10)
ncv_fit <- gam(
  form2,
  data = GE_convex,
  argGam = list(knots=list(tod=seq(0,24,length=9))),
  method = "NCV",
  nei = nei,
  control = gc
) |>
  getViz()
x11()

print(plot(ncv_fit, allTerms = TRUE), pages = 1)

# save(ncv_fit, file = "temp/ncv_GE_convex_fit_sample_1000.RData")



compare_fit <- bam(
  form2,
  data = GE_convex,
  argGam = list(knots=list(tod=seq(0,24,length=9))),
  discrete = TRUE
) |>
  getViz()

print(plot(compare_fit, allTerms = TRUE), pages = 1)



qq.gam(ncv_fit, rep = 1)
qq.gam(compare_fit, rep = 1)
AIC(ncv_fit, compare_fit)
