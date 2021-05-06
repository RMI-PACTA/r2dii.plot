# Source: R/get_example_data.R (sha 396e4a0)
example_data <- vroom::vroom(here::here("data-raw", "example_data.csv"))
example_data$year <- as.integer(example_data$year)
usethis::use_data(example_data, overwrite = TRUE)
