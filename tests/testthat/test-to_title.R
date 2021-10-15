test_that("spell_out_technology() converts techmix labels", {
  data <- tibble::tribble(
        ~technology,             ~expected,
              "gas",                 "Gas",
              "ice",                 "ICE",
          "coalcap",       "Coal Capacity",
          "xyz_hdv", "Heavy Duty Vehicles",

  )
  expect_equal(spell_out_technology(data$technology), data$expected)
})
