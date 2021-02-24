# Source: @MonikaFu https://github.com/2DegreesInvesting/r2dii.ggplot/issues/55
sda_target <- readr::read_csv(here::here("data-raw", "sda_target.csv"))
usethis::use_data(sda_target, overwrite = TRUE)
