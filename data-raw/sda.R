# Source: @MonikaFu https://github.com/RMI-PACTA/r2dii.plot/issues/55
sda <- readr::read_csv(here::here("data-raw", "sda.csv"))
usethis::use_data(sda, overwrite = TRUE)
