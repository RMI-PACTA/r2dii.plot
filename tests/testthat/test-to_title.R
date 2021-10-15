test_that("spell_out_technology() converts techmix labels", {
  data <- tibble::tribble(
        ~technology,             ~expected,
         "electric",            "Electric",
           "hybrid",              "Hybrid",
              "ice",                 "ICE",
              "gas",                 "Gas",
              "oil",                 "Oil",
          "coalcap",       "Coal Capacity",
           "gascap",        "Gas Capacity",
         "hydrocap",      "Hydro Capacity",
           "oilcap",        "Oil Capacity",
    "renewablescap", "Renewables Capacity"
  )
  expect_equal(spell_out_technology(data$technology), data$expected)
})
