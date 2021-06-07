# outputs the expected snapshot

    Code
      prep_timelineY(data)
    Output
      # A tibble: 6 x 7
        sector    year       emission_factor_metr~ emission_factor_va~ line_name value
        <chr>     <date>     <chr>                               <dbl> <chr>     <dbl>
      1 automoti~ 2002-01-01 projected                           0.228 projected 0.228
      2 automoti~ 2003-01-01 projected                           0.226 projected 0.226
      3 automoti~ 2004-01-01 projected                           0.224 projected 0.224
      4 automoti~ 2005-01-01 projected                           0.222 projected 0.222
      5 automoti~ 2006-01-01 projected                           0.220 projected 0.220
      6 automoti~ 2007-01-01 projected                           0.218 projected 0.218
      # ... with 1 more variable: extrapolated <lgl>

# with bad `sector_filter` errors gracefully

    `sector_filter` must be of length 1, not 2.

---

    is.character(sector_filter) is not TRUE

