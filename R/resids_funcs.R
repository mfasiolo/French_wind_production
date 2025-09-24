# function to get residuals in matrix with dates as rows and times of day as columns
get_resids_mat <- function(dat, fit) {

  # calculate residuals
  resids <- dat$prod_div_cap - fit$fitted.values

  dat |>
    select(datetime) |>
    mutate(resids = resids) |>
    mutate(
      date = as.Date(datetime),
      tod = format(datetime, "%H:%M")
    ) |>
    select(date, tod, resids) |>
    pivot_wider(
      names_from = tod,
      values_from = resids
    )
}

# function to fit basic gam and get residuals, in vector or matrix format
# format = "mat" or "vec"
basic_fit_resids <- function(dat, region, family, format) {

  # combine wind and production data
  dat <- combine_data(region, dat)

  # model formua
  form1 <- prod_div_cap ~ s(toy, bs = "cc", k = 5) + s(tod, bs = "cc", k = 10) +
    s(capacity) +
    s(windspeed10) + s(direction10) + ti(windspeed10, direction10) +
    s(windspeed100) + s(direction100) + ti(windspeed100, direction100) +
    ti(windspeed10, windspeed100)

  # fit gam
  gam_fit <- bam(
    form1,
    data = dat,
    discrete = TRUE,
    family = family,
    argGam = list(knots=list(tod=seq(0,24,length=9)))
  )

  # get residuals
  if (format == "vec") {
    resids_vec <- dat$prod_div_cap - gam_fit$fitted.values
    return(resids_vec)
  }

  resids_mat <- get_resids_mat(dat, gam_fit) |>
    as.data.frame()

  return(resids_mat)
}
