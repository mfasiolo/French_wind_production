get_resids_mat <- function(dat, fit) {

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
