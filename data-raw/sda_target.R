# Source: @MonikaFu https://github.com/2DegreesInvesting/r2dii.ggplot/issues/55
sda_data <- readr::read_csv(here::here("data-raw", "sda_target.csv"))
use_data(sda_data, overwrite = TRUE)
