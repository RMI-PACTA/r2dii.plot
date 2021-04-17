# with the simplest call outputs the expected plot object

    Code
      out
    Output
      $data
      list()
      attr(,"class")
      [1] "waiver"
      
      $layers
      $layers[[1]]
      mapping: x = ~factor(.data$metric_type, levels = rev(df_bar_specs$metric_type)), y = ~.data$value, fill = ~factor(.data$technology, levels = data_colours$technology) 
      geom_bar: width = 0.5, na.rm = FALSE, orientation = NA
      stat_identity: na.rm = FALSE
      position_fill 
      
      
      $scales
      <ggproto object: Class ScalesList, gg>
          add: function
          clone: function
          find: function
          get_scales: function
          has_scale: function
          input: function
          n: function
          non_position_scales: function
          scales: list
          super:  <ggproto object: Class ScalesList, gg>
      
      $mapping
      Aesthetic mapping: 
      <empty>
      
      $theme
      List of 93
       $ line                      :List of 6
        ..$ colour       : chr "black"
        ..$ size         : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                      :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ size         : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                      :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                     : NULL
       $ aspect.ratio              : NULL
       $ axis.title                :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 12
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top          :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom       : NULL
       $ axis.title.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left         : NULL
       $ axis.title.y.right        :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                 :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 10
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top           :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom        : NULL
       $ axis.text.y               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left          : NULL
       $ axis.text.y.right         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                :List of 6
        ..$ colour       : chr "#C0C0C0"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.ticks.x              : NULL
       $ axis.ticks.x.top          : NULL
       $ axis.ticks.x.bottom       : NULL
       $ axis.ticks.y              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.ticks.y.left         : NULL
       $ axis.ticks.y.right        : NULL
       $ axis.ticks.length         : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ axis.ticks.length.x       : NULL
       $ axis.ticks.length.x.top   : NULL
       $ axis.ticks.length.x.bottom: NULL
       $ axis.ticks.length.y       : NULL
       $ axis.ticks.length.y.left  : NULL
       $ axis.ticks.length.y.right : NULL
       $ axis.line                 :List of 6
        ..$ colour       : chr "#C0C0C0"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.line.x               : NULL
       $ axis.line.x.top           : NULL
       $ axis.line.x.bottom        : NULL
       $ axis.line.y               : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.y.left          : NULL
       $ axis.line.y.right         : NULL
       $ legend.background         :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin             : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing            : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x          : NULL
       $ legend.spacing.y          : NULL
       $ legend.key                : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.key.size           : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height         : NULL
       $ legend.key.width          : NULL
       $ legend.text               :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 9
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.align         : NULL
       $ legend.title              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.title.align        : NULL
       $ legend.position           : chr "bottom"
       $ legend.direction          : NULL
       $ legend.justification      : chr "center"
       $ legend.box                : NULL
       $ legend.box.just           : NULL
       $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing        : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ panel.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.border              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.spacing             : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ panel.spacing.x           : NULL
       $ panel.spacing.y           : NULL
       $ panel.grid                :List of 6
        ..$ colour       : chr "grey92"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.minor          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.major.x        : NULL
       $ panel.grid.major.y        : NULL
       $ panel.grid.minor.x        : NULL
       $ panel.grid.minor.y        : NULL
       $ panel.ontop               : logi FALSE
       $ plot.background           :List of 5
        ..$ fill         : NULL
        ..$ colour       : chr "white"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ plot.title                :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : chr "bold"
        ..$ colour       : NULL
        ..$ size         : num 14
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 20points 2points 12points 2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.title.position       : chr "panel"
       $ plot.subtitle             :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : num 1
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5.5points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption.position     : chr "panel"
       $ plot.tag                  :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.tag.position         : chr "topleft"
       $ plot.margin               : 'simpleUnit' num [1:4] 0.5cm 1cm 0.5cm 0.5cm
        ..- attr(*, "unit")= int 1
       $ strip.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ size         : 'rel' num 2
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ strip.background.x        : NULL
       $ strip.background.y        : NULL
       $ strip.placement           : chr "inside"
       $ strip.text                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey10"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 4.4points 4.4points 4.4points 4.4points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x              : NULL
       $ strip.text.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.switch.pad.grid     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.switch.pad.wrap     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.text.y.left         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      
      $coordinates
      <ggproto object: Class CoordFlip, CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: FALSE
          distance: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordFlip, CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetNull, Facet, gg>
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panels: function
          finish_data: function
          init_scales: function
          map_data: function
          params: list
          setup_data: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetNull, Facet, gg>
      
      $labels
      $labels$title
      [1] ""
      
      $labels$y
      [1] ""
      
      $labels$x
      [1] ""
      
      $labels$fill
      [1] "factor(technology, levels = data_colours$technology)"
      
      
      $guides
      $guides$fill
      $title
      list()
      attr(,"class")
      [1] "waiver"
      
      $title.position
      NULL
      
      $title.theme
      NULL
      
      $title.hjust
      NULL
      
      $title.vjust
      NULL
      
      $label
      [1] TRUE
      
      $label.position
      NULL
      
      $label.theme
      NULL
      
      $label.hjust
      NULL
      
      $label.vjust
      NULL
      
      $keywidth
      NULL
      
      $keyheight
      NULL
      
      $direction
      NULL
      
      $override.aes
      named list()
      
      $nrow
      NULL
      
      $ncol
      [1] 3
      
      $byrow
      [1] TRUE
      
      $reverse
      [1] FALSE
      
      $order
      [1] 0
      
      $available_aes
      [1] "any"
      
      $name
      [1] "legend"
      
      attr(,"class")
      [1] "guide"  "legend"
      
      

# with show_legend = FALSE outputs the expected plot object

    Code
      out
    Output
      $data
      list()
      attr(,"class")
      [1] "waiver"
      
      $layers
      $layers[[1]]
      mapping: x = ~factor(.data$metric_type, levels = rev(df_bar_specs$metric_type)), y = ~.data$value, fill = ~factor(.data$technology, levels = data_colours$technology) 
      geom_bar: width = 0.5, na.rm = FALSE, orientation = NA
      stat_identity: na.rm = FALSE
      position_fill 
      
      
      $scales
      <ggproto object: Class ScalesList, gg>
          add: function
          clone: function
          find: function
          get_scales: function
          has_scale: function
          input: function
          n: function
          non_position_scales: function
          scales: list
          super:  <ggproto object: Class ScalesList, gg>
      
      $mapping
      Aesthetic mapping: 
      <empty>
      
      $theme
      List of 93
       $ line                      :List of 6
        ..$ colour       : chr "black"
        ..$ size         : num 0.5
        ..$ linetype     : num 1
        ..$ lineend      : chr "butt"
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ rect                      :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ size         : num 0.5
        ..$ linetype     : num 1
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ text                      :List of 11
        ..$ family       : chr ""
        ..$ face         : chr "plain"
        ..$ colour       : chr "black"
        ..$ size         : num 11
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : num 0
        ..$ lineheight   : num 0.9
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ title                     : NULL
       $ aspect.ratio              : NULL
       $ axis.title                :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 12
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.75points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.top          :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.75points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.x.bottom       : NULL
       $ axis.title.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.75points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.title.y.left         : NULL
       $ axis.title.y.right        :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.75points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text                 :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 10
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 2.2points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.top           :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : num 0
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 2.2points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.x.bottom        : NULL
       $ axis.text.y               :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 1
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 2.2points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.text.y.left          : NULL
       $ axis.text.y.right         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 0points 2.2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ axis.ticks                :List of 6
        ..$ colour       : chr "#C0C0C0"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.ticks.x              : NULL
       $ axis.ticks.x.top          : NULL
       $ axis.ticks.x.bottom       : NULL
       $ axis.ticks.y              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.ticks.y.left         : NULL
       $ axis.ticks.y.right        : NULL
       $ axis.ticks.length         : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ axis.ticks.length.x       : NULL
       $ axis.ticks.length.x.top   : NULL
       $ axis.ticks.length.x.bottom: NULL
       $ axis.ticks.length.y       : NULL
       $ axis.ticks.length.y.left  : NULL
       $ axis.ticks.length.y.right : NULL
       $ axis.line                 :List of 6
        ..$ colour       : chr "#C0C0C0"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ axis.line.x               : NULL
       $ axis.line.x.top           : NULL
       $ axis.line.x.bottom        : NULL
       $ axis.line.y               : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ axis.line.y.left          : NULL
       $ axis.line.y.right         : NULL
       $ legend.background         :List of 5
        ..$ fill         : NULL
        ..$ colour       : logi NA
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ legend.margin             : 'margin' num [1:4] 5.5points 5.5points 5.5points 5.5points
        ..- attr(*, "unit")= int 8
       $ legend.spacing            : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ legend.spacing.x          : NULL
       $ legend.spacing.y          : NULL
       $ legend.key                : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.key.size           : 'simpleUnit' num 1.2lines
        ..- attr(*, "unit")= int 3
       $ legend.key.height         : NULL
       $ legend.key.width          : NULL
       $ legend.text               :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : num 9
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5points 5points 5points 5points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ legend.text.align         : NULL
       $ legend.title              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.title.align        : NULL
       $ legend.position           : chr "none"
       $ legend.direction          : NULL
       $ legend.justification      : chr "center"
       $ legend.box                : NULL
       $ legend.box.just           : NULL
       $ legend.box.margin         : 'margin' num [1:4] 0cm 0cm 0cm 0cm
        ..- attr(*, "unit")= int 1
       $ legend.box.background     : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ legend.box.spacing        : 'simpleUnit' num 11points
        ..- attr(*, "unit")= int 8
       $ panel.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : logi NA
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ panel.border              : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.spacing             : 'simpleUnit' num 5.5points
        ..- attr(*, "unit")= int 8
       $ panel.spacing.x           : NULL
       $ panel.spacing.y           : NULL
       $ panel.grid                :List of 6
        ..$ colour       : chr "grey92"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ lineend      : NULL
        ..$ arrow        : logi FALSE
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_line" "element"
       $ panel.grid.major          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.minor          : list()
        ..- attr(*, "class")= chr [1:2] "element_blank" "element"
       $ panel.grid.major.x        : NULL
       $ panel.grid.major.y        : NULL
       $ panel.grid.minor.x        : NULL
       $ panel.grid.minor.y        : NULL
       $ panel.ontop               : logi FALSE
       $ plot.background           :List of 5
        ..$ fill         : NULL
        ..$ colour       : chr "white"
        ..$ size         : NULL
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ plot.title                :List of 11
        ..$ family       : chr "Helvetica"
        ..$ face         : chr "bold"
        ..$ colour       : NULL
        ..$ size         : num 14
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 20points 2points 12points 2points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi FALSE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.title.position       : chr "panel"
       $ plot.subtitle             :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : num 0
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 0points 0points 5.5points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : num 1
        ..$ vjust        : num 1
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 5.5points 0points 0points 0points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.caption.position     : chr "panel"
       $ plot.tag                  :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : 'rel' num 1.2
        ..$ hjust        : num 0.5
        ..$ vjust        : num 0.5
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ plot.tag.position         : chr "topleft"
       $ plot.margin               : 'simpleUnit' num [1:4] 0.5cm 1cm 0.5cm 0.5cm
        ..- attr(*, "unit")= int 1
       $ strip.background          :List of 5
        ..$ fill         : chr "white"
        ..$ colour       : chr "black"
        ..$ size         : 'rel' num 2
        ..$ linetype     : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_rect" "element"
       $ strip.background.x        : NULL
       $ strip.background.y        : NULL
       $ strip.placement           : chr "inside"
       $ strip.text                :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : chr "grey10"
        ..$ size         : 'rel' num 0.8
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : NULL
        ..$ lineheight   : NULL
        ..$ margin       : 'margin' num [1:4] 4.4points 4.4points 4.4points 4.4points
        .. ..- attr(*, "unit")= int 8
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.text.x              : NULL
       $ strip.text.y              :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num -90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       $ strip.switch.pad.grid     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.switch.pad.wrap     : 'simpleUnit' num 2.75points
        ..- attr(*, "unit")= int 8
       $ strip.text.y.left         :List of 11
        ..$ family       : NULL
        ..$ face         : NULL
        ..$ colour       : NULL
        ..$ size         : NULL
        ..$ hjust        : NULL
        ..$ vjust        : NULL
        ..$ angle        : num 90
        ..$ lineheight   : NULL
        ..$ margin       : NULL
        ..$ debug        : NULL
        ..$ inherit.blank: logi TRUE
        ..- attr(*, "class")= chr [1:2] "element_text" "element"
       - attr(*, "class")= chr [1:2] "theme" "gg"
       - attr(*, "complete")= logi TRUE
       - attr(*, "validate")= logi TRUE
      
      $coordinates
      <ggproto object: Class CoordFlip, CoordCartesian, Coord, gg>
          aspect: function
          backtransform_range: function
          clip: on
          default: FALSE
          distance: function
          expand: TRUE
          is_free: function
          is_linear: function
          labels: function
          limits: list
          modify_scales: function
          range: function
          render_axis_h: function
          render_axis_v: function
          render_bg: function
          render_fg: function
          setup_data: function
          setup_layout: function
          setup_panel_guides: function
          setup_panel_params: function
          setup_params: function
          train_panel_guides: function
          transform: function
          super:  <ggproto object: Class CoordFlip, CoordCartesian, Coord, gg>
      
      $facet
      <ggproto object: Class FacetNull, Facet, gg>
          compute_layout: function
          draw_back: function
          draw_front: function
          draw_labels: function
          draw_panels: function
          finish_data: function
          init_scales: function
          map_data: function
          params: list
          setup_data: function
          setup_params: function
          shrink: TRUE
          train_scales: function
          vars: function
          super:  <ggproto object: Class FacetNull, Facet, gg>
      
      $labels
      $labels$title
      [1] ""
      
      $labels$y
      [1] ""
      
      $labels$x
      [1] ""
      
      $labels$fill
      [1] "factor(technology, levels = data_colours$technology)"
      
      

