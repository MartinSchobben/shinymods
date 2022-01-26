# gui can be generated

    Code
      purrr::map2(xc[col_spec(xc) != "logical"], names(col_spec(xc))[col_spec(xc) !=
        "logical"], ~col_vals(.x, id = "filter", label = .y))
    Output
      $speed
      $speed$inputId
      [1] "filter"
      
      $speed$label
      [1] "speed"
      
      $speed$min
      [1] 4
      
      $speed$max
      [1] 25
      
      $speed$value
      [1]  4 25
      
      
      $dist
      $dist$inputId
      [1] "filter"
      
      $dist$label
      [1] "dist"
      
      $dist$min
      [1] 2
      
      $dist$max
      [1] 120
      
      $dist$value
      [1]   2 120
      
      

---

    Code
      filter_ui("id", xc, c("Speed", "Distribution"))
    Output
      [[1]]
      shiny::sliderInput(inputId = "filter", label = "Speed", min = 4, 
          max = 25, value = c(4, 25))
      
      [[2]]
      shiny::sliderInput(inputId = "filter", label = "Distribution", 
          min = 2, max = 120, value = c(2, 120))
      
      [[3]]
      shiny::selectInput(inputID = "id", label = "Various", choices = c("lg1", 
      "lg2"))
      

