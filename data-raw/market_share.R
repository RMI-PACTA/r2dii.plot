# Source: R/get_market_share.R (sha 396e4a0)
market_share <- vroom::vroom(here::here("data-raw", "market_share.csv"))
market_share$year <- as.integer(market_share$year)
usethis::use_data(market_share, overwrite = TRUE)
