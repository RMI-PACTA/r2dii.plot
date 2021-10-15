test_that("format_metric() converts labels", {
  data <- tribble(
               ~input,             ~output,
          "projected",         "Projected",
  "corporate_economy", "Corporate Economy",
         "target_cps",               "CPS",
         "target_sds",               "SDS",
         "target_sps",               "SPS"
  )
  helper <- format_metric
  expect_equal(helper(data$input), data$output)
})

test_that("spell_out_technology() converts techmix labels", {
  data <- tribble(
             ~input,                   ~output,
              "gas",                     "Gas",
              "ice",                     "ICE",
          "coalcap",           "Coal Capacity",
          "xyz_hdv", "Xyz Heavy Duty Vehicles",
  )
  helper <- spell_out_technology
  expect_equal(helper(data$input), data$output)
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
  helper <- recode_metric_techmix
  expect_equal(helper(data$input), data$output)
})
