# outputs the expected snapshot

    Code
      out
    Output
      # A tibble: 30 x 10
         sector technology  year region scenario_source metric            production
         <chr>  <chr>      <int> <chr>  <chr>           <chr>                  <dbl>
       1 power  coalcap     2020 global demo_2020       projected             22284.
       2 power  coalcap     2020 global demo_2020       corporate_economy   3581945.
       3 power  coalcap     2020 global demo_2020       sds                   22284.
       4 power  coalcap     2025 global demo_2020       projected             20340.
       5 power  coalcap     2025 global demo_2020       corporate_economy   3586097.
       6 power  coalcap     2025 global demo_2020       sds                   19412.
       7 power  gascap      2020 global demo_2020       projected              4324.
       8 power  gascap      2020 global demo_2020       corporate_economy   1129817.
       9 power  gascap      2020 global demo_2020       sds                    4324.
      10 power  gascap      2025 global demo_2020       projected              4149.
      # ... with 20 more rows, and 3 more variables: technology_share <dbl>,
      #   metric_type <chr>, value <dbl>

# with more than one value of some columns errors gracefully

    `scenario_source` must have a single value. It has: 1, 2.

---

    `sector` must have a single value. It has: 1, 2.

---

    `region` must have a single value. It has: 1, 2.

# with bad metric errors gracefully

    Can't find values to recode as 'portfolio'.

