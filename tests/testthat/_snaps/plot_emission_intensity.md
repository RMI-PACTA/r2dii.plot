# if `data` is not a data frame errors gracefully

    is.data.frame(data) is not TRUE

# if `data` is not sda-like errors gracefully

    Must have missing names:
    emission_factor_metric, emission_factor_value

# if `data` has zero rows errors gracefully

    `zero_row` must have some rows but has none.

# with too many sectors errors gracefully

    `data` must have a single value of `sector` but has: a, b.
    Pick one value, e.g. 'a', with:
      subset(data, sector == 'a')

# with too many lines to plot errors gracefully

    Can't plot more than 7 lines in one plot.
    Found 12 lines: projected, corporate_economy, target_demo, adjusted_scenario_demo, 1, 2, 3, 4, 5, 6, 7, 8.
    Consider splitting the data over multiple plots.

# prep stays the same

    Code
      out
    Output
      # A tibble: 29 x 5
         sector year       emission_factor_metric emission_factor_value hex    
         <chr>  <date>     <chr>                                  <dbl> <chr>  
       1 cement 2020-01-01 Projected                              0.664 #1b324f
       2 cement 2021-01-01 Projected                              0.665 #1b324f
       3 cement 2022-01-01 Projected                              0.666 #1b324f
       4 cement 2023-01-01 Projected                              0.667 #1b324f
       5 cement 2024-01-01 Projected                              0.668 #1b324f
       6 cement 2025-01-01 Projected                              0.669 #1b324f
       7 cement 2020-01-01 Corporate Economy                      0.669 #00c082
       8 cement 2021-01-01 Corporate Economy                      0.670 #00c082
       9 cement 2022-01-01 Corporate Economy                      0.671 #00c082
      10 cement 2023-01-01 Corporate Economy                      0.672 #00c082
      # ... with 19 more rows

