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
      # A tibble: 18 x 4
         sector       technology    label                        hex    
         <chr>        <chr>         <chr>                        <chr>  
       1 power        coalcap       Coal Capacity                #7A2701
       2 power        oilcap        Oil Capacity                 #a63603
       3 power        gascap        Gas Capacity                 #e6550d
       4 power        nuclearcap    Nuclear Capacity             #fd8d3c
       5 power        hydrocap      Hydro Capacity               #fdbe85
       6 power        renewablescap Renewables Capacity          #ffd4ad
       7 automotive   electric      Electric                     #548995
       8 automotive   electric_hdv  Electric Heavy Duty Vehicles #609cab
       9 automotive   fuelcell      FuelCell                     #6cb0c0
      10 automotive   hybrid        Hybrid                       #78c4d6
      11 automotive   hybrid_hdv    Hybrid Heavy Duty Vehicles   #93cfde
      12 automotive   ice           ICE                          #aedbe6
      13 automotive   ice_hdv       ICE Heavy Duty Vehicles      #c9e7ee
      14 oil&gas      gas           Gas                          #b9b5b0
      15 oil&gas      oil           Oil                          #181716
      16 fossil fuels gas           Gas                          #b9b5b0
      17 fossil fuels oil           Oil                          #181716
      18 fossil fuels coal          Coal                         #4e3b37

