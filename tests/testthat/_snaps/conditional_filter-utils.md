# col_vals works

    Code
      col_vals(cars$speed)
    Output
      [[1]]
      [1] "sliderInput"
      
      [[2]]
      [[2]]$min
      min(cars$speed, na.rm = TRUE)
      
      [[2]]$max
      max(cars$speed, na.rm = TRUE)
      
      [[2]]$value
      range(cars$speed, na.rm = TRUE)
      
      

---

    Code
      col_vals(iris$Species)
    Output
      [[1]]
      [1] "selectInput"
      
      [[2]]
      [[2]]$choices
      levels(as.factor(iris$Species))
      
      [[2]]$selected
      levels(as.factor(iris$Species))
      
      [[2]]$multiple
      [1] TRUE
      
      

# logical_controller works

    Code
      logical_controller(cars2, "testthat")
    Output
      selectizeInput(inputId = "testthat-logi", label = "Various", 
          choices = detect_lgl(cars2, ignore = "", external = ""), 
          multiple = TRUE, options = list(placeholder = "select", onInitialize = "function() { this.setValue(null); }"))

---

    Code
      logical_controller(cars2, "testthat", labels = "a")
    Output
      selectizeInput(inputId = "testthat-logi", label = "a", choices = detect_lgl(cars2, 
          ignore = "", external = ""), multiple = TRUE, options = list(
          placeholder = "select", onInitialize = "function() { this.setValue(null); }"))

---

    Code
      logical_controller(cars2, "testthat", labels = "a", update = TRUE)
    Output
      updateSelectizeInput(session = "testthat", inputId = "logi", 
          label = "a", choices = detect_lgl(cars2, ignore = "", external = ""), 
          selected = shiny::isolate(input$logi))

# helpers work

    Code
      col_spec(cars2, "dist", "speed")
    Output
      high_speed  high_dist 
       "logical"  "logical" 

---

    Code
      col_spec(cars2, "dist")
    Output
      high_speed      speed  high_dist 
       "logical"  "numeric"  "logical" 

---

    Code
      col_spec(cars2)
    Output
      high_speed      speed       dist  high_dist 
       "logical"  "numeric"  "numeric"  "logical" 

