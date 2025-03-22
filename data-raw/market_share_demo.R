# Source: R/get_market_share.R (sha 396e4a0)
market_share_demo <- vroom::vroom(here::here("data-raw", "market_share_demo.csv"))
market_share_demo$year <- as.integer(market_share_demo$year)
usethis::use_data(market_share_demo, overwrite = TRUE)
