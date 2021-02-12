# outputs the expected snapshot

    Code
      get_example_data()
    Output
      # A tibble: 1,170 x 8
         sector technology  year region scenario_source metric production
         <chr>  <chr>      <int> <chr>  <chr>           <chr>       <dbl>
       1 autom~ electric    2020 global demo_2020       proje~    145942.
       2 autom~ electric    2020 global demo_2020       corpo~   8134869.
       3 autom~ electric    2020 global demo_2020       targe~    145942.
       4 autom~ electric    2020 global demo_2020       targe~    145942.
       5 autom~ electric    2020 global demo_2020       targe~    145942.
       6 autom~ electric    2021 global demo_2020       proje~    148212.
       7 autom~ electric    2021 global demo_2020       corpo~   8183411.
       8 autom~ electric    2021 global demo_2020       targe~    148361.
       9 autom~ electric    2021 global demo_2020       targe~    160625.
      10 autom~ electric    2021 global demo_2020       targe~    149016.
      # ... with 1,160 more rows, and 1 more variable: technology_share <dbl>

