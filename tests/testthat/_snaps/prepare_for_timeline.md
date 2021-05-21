# with bad `sector_filter` errors gracefully

    Invalid `sector_filter`: bad.
    Expected one of: automotive, aviation, cement, oil and gas, shipping, steel, power.

# outputs the expected snapshot

    Code
      prepare_for_timelineB(sda_target)
    Output
      # A tibble: 208 x 5
         year       line_name value extrapolated sector    
         <date>     <chr>     <dbl> <lgl>        <chr>     
       1 2002-01-01 projected 0.228 FALSE        automotive
       2 2003-01-01 projected 0.226 FALSE        automotive
       3 2004-01-01 projected 0.224 FALSE        automotive
       4 2005-01-01 projected 0.222 FALSE        automotive
       5 2006-01-01 projected 0.220 FALSE        automotive
       6 2007-01-01 projected 0.218 FALSE        automotive
       7 2008-01-01 projected 0.216 FALSE        automotive
       8 2009-01-01 projected 0.214 FALSE        automotive
       9 2010-01-01 projected 0.212 FALSE        automotive
      10 2011-01-01 projected 0.210 FALSE        automotive
      # ... with 198 more rows

