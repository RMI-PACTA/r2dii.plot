test_that("spell_out_technology() converts techmix labels", {
  data <- tribble(
             ~input,                   ~output,
              "gas",                     "Gas",
              "ice",                     "ICE",
          "coalcap",           "Coal Capacity",
          "xyz_hdv", "Xyz Heavy Duty Vehicles",
  )
  expect_equal(spell_out_technology(data$input), data$output)
})

test_that("recode_metric_techmix() converts labels", {
  data <- tribble(
           ~input,     ~output,
      "projected", "Portfolio",
        "target_",  "Scenario",
  "anything else", "Benchmark",
      # Edge cases
      "Projected", "Benchmark",
       ".target_", "Benchmark",
            "xyz", "Benchmark",
  )
  expect_equal(recode_metric_techmix(data$input), data$output)
})
