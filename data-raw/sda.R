# Source: @MonikaFu https://github.com/2DegreesInvesting/r2dii.plot.static/issues/55
sda <- readr::read_csv(here::here("data-raw", "sda.csv"))
usethis::use_data(sda, overwrite = TRUE)
