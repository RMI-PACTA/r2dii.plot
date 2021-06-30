# outputs the expected snapshot

    Code
      palette_colours
    Output
      # A tibble: 9 x 2
        label       hex    
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
    Code
      scenario_colours
    Output
      # A tibble: 5 x 2
        label        hex    
        <chr>        <chr>  
      1 dark_green   #768555
      2 light_green  #9CAB7C
      3 dark_yellow  #FFFFCC
      4 light_yellow #FDE291
      5 red          #E07B73
    Code
      sector_colours
    Output
      # A tibble: 8 x 2
        label      hex    
        <chr>      <chr>  
      1 automotive #78C4D6
      2 aviation   #00c082
      3 cement     #c1b358
      4 coal       #4e3b37
      5 oil&gas    #181716
      6 power      #a63603
      7 shipping   #574099
      8 steel      #a63d57
    Code
      technology_colours
    Output
      # A tibble: 18 x 3
         sector       technology    hex    
         <chr>        <chr>         <chr>  
       1 power        coalcap       #7A2701
       2 power        oilcap        #a63603
       3 power        gascap        #e6550d
       4 power        nuclearcap    #fd8d3c
       5 power        hydrocap      #fdbe85
       6 power        renewablescap #ffd4ad
       7 automotive   ice_hdv       #548995
       8 automotive   ice           #609cab
       9 automotive   hybrid_hdv    #6cb0c0
      10 automotive   hybrid        #78c4d6
      11 automotive   fuelcell      #93cfde
      12 automotive   electric_hdv  #aedbe6
      13 automotive   electric      #c9e7ee
      14 oil&gas      gas           #b9b5b0
      15 oil&gas      oil           #181716
      16 fossil fuels gas           #b9b5b0
      17 fossil fuels oil           #181716
      18 fossil fuels coal          #4e3b37

