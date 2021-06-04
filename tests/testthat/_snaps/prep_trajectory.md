# with missing crucial columns errors gracefully

    Must have missing names:
    `sector`

# outputs the expected snapshot

    Code
      out
    Output
      # A tibble: 30 x 5
          year metric_type metric            technology value
         <int> <chr>       <chr>             <chr>      <dbl>
       1  2020 portfolio   projected         oilcap     1    
       2  2020 benchmark   corporate_economy oilcap     1    
       3  2020 scenario    cps               oilcap     1    
       4  2020 scenario    sds               oilcap     1    
       5  2020 scenario    sps               oilcap     1    
       6  2021 portfolio   projected         oilcap     0.994
       7  2021 benchmark   corporate_economy oilcap     0.992
       8  2021 scenario    cps               oilcap     0.932
       9  2021 scenario    sds               oilcap     0.922
      10  2021 scenario    sps               oilcap     0.931
      # ... with 20 more rows

# with multiple distinct values in some columns errors gracefully

    `sector` must have one value but has more: 1, 2.

---

    `technology` must have one value but has more: 1, 2.

---

    `region` must have one value but has more: 1, 2.

---

    `scenario_source` must have one value but has more: 1, 2.

# if `normalize` isn't length-1 errors gracefully

    `normalize` must be of length 1, not 2.

# if `normalize` isn't logical errors gracefully

    is.logical(normalize) is not TRUE

