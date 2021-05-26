# Source: @MonikaFu https://github.com/2DegreesInvesting/r2dii.plot.static/issues/151
r2dii_scenario_colours <- readr::read_csv(here::here("data-raw", "r2dii_scenario_colours.csv"))
usethis::use_data(r2dii_scenario_colours, overwrite = TRUE)
