# outputs the expected date

    Code
      get_r2dii_technology_colours("power")
    Output
      # A tibble: 6 x 3
        technology    label               colour 
        <chr>         <chr>               <chr>  
      1 coalcap       Coal Capacity       #7A2701
      2 oilcap        Oil Capacity        #a63603
      3 gascap        Gas Capacity        #e6550d
      4 nuclearcap    Nuclear Capacity    #fd8d3c
      5 hydrocap      Hydro Capacity      #fdbe85
      6 renewablescap Renewables Capacity #ffd4ad

---

    Code
      r2dii_palette_colours()
    Output
      # A tibble: 9 x 2
        label       colour_hex
        <chr>       <chr>     
      1 dark_blue   #1b324f   
      2 green       #00c082   
      3 orange      #ff9623   
      4 grey        #d0d7e1   
      5 dark_purple #574099   
      6 yellow      #f2e06e   
      7 soft_blue   #78c4d6   
      8 ruby_red    #a63d57   
      9 moss_green  #4a5e54   

---

    Code
      r2dii_sector_colours()
    Output
      # A tibble: 8 x 2
        label      colour_hex
        <chr>      <chr>     
      1 automotive #78C4D6   
      2 aviation   #00c082   
      3 cement     #c1b358   
      4 coal       #4e3b37   
      5 oil&gas    #181716   
      6 power      #a63603   
      7 shipping   #574099   
      8 steel      #a63d57   

