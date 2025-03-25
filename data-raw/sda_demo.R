# Source: @MonikaFu https://github.com/RMI-PACTA/r2dii.plot/issues/55
sda_demo <- readr::read_csv(here::here("data-raw", "sda_demo.csv"))
usethis::use_data(sda_demo, overwrite = TRUE)
