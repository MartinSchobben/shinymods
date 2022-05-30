# col_vals works

    Code
      col_vals(cars$speed, rlang::expr(cars$speed), .GlobalEnv)
    Output
      [[1]]
      [1] "sliderInput"
      
      [[2]]
      [[2]]$min
      [1] 4
      
      [[2]]$max
      [1] 25
      
      [[2]]$value
      [1]  4 25
      
      

---

    Code
      col_vals(iris$Species, rlang::expr(iris$Species), .GlobalEnv)
    Output
      [[1]]
      [1] "selectInput"
      
      [[2]]
      [[2]]$choices
      [1] "setosa"     "versicolor" "virginica" 
      
      [[2]]$selected
      [1] "setosa"     "versicolor" "virginica" 
      
      [[2]]$multiple
      [1] TRUE
      
      

# detect_controller works

    Code
      detect_controller(cars, "testthat")
    Output
      $speed
      sliderInput(inputId = "testthat-speed", label = "speed", min = 4, 
          max = 25, value = c(4, 25))
      
      $dist
      sliderInput(inputId = "testthat-dist", label = "dist", min = 2, 
          max = 120, value = c(2, 120))
      

---

    Code
      detect_controller(`testdata()`, "testthat")
    Output
      $speed
      sliderInput(inputId = "testthat-speed", label = "speed", min = 4, 
          max = 25, value = c(4, 25))
      
      $dist
      sliderInput(inputId = "testthat-dist", label = "dist", min = 2, 
          max = 120, value = c(2, 120))
      

---

    Code
      detect_controller(iris, "testthat")
    Output
      $Sepal.Length
      sliderInput(inputId = "testthat-Sepal.Length", label = "Sepal.Length", 
          min = 4.3, max = 7.9, value = c(4.3, 7.9))
      
      $Sepal.Width
      sliderInput(inputId = "testthat-Sepal.Width", label = "Sepal.Width", 
          min = 2, max = 4.4, value = c(2, 4.4))
      
      $Petal.Length
      sliderInput(inputId = "testthat-Petal.Length", label = "Petal.Length", 
          min = 1, max = 6.9, value = c(1, 6.9))
      
      $Petal.Width
      sliderInput(inputId = "testthat-Petal.Width", label = "Petal.Width", 
          min = 0.1, max = 2.5, value = c(0.1, 2.5))
      
      $Species
      selectInput(inputId = "testthat-Species", label = "Species", 
          choices = c("setosa", "versicolor", "virginica"), selected = c("setosa", 
          "versicolor", "virginica"), multiple = TRUE)
      

---

    Code
      detect_controller(cars, NULL, update = T)
    Output
      $speed
      updateSliderInput(inputId = "speed", min = min(cars$speed, na.rm = TRUE), 
          max = max(cars$speed, na.rm = TRUE), value = range(cars$speed, 
              na.rm = TRUE))
      
      $dist
      updateSliderInput(inputId = "dist", min = min(cars$dist, na.rm = TRUE), 
          max = max(cars$dist, na.rm = TRUE), value = range(cars$dist, 
              na.rm = TRUE))
      

---

    Code
      detect_controller(iris, NULL, update = T)
    Output
      $Sepal.Length
      updateSliderInput(inputId = "Sepal.Length", min = min(iris$Sepal.Length, 
          na.rm = TRUE), max = max(iris$Sepal.Length, na.rm = TRUE), 
          value = range(iris$Sepal.Length, na.rm = TRUE))
      
      $Sepal.Width
      updateSliderInput(inputId = "Sepal.Width", min = min(iris$Sepal.Width, 
          na.rm = TRUE), max = max(iris$Sepal.Width, na.rm = TRUE), 
          value = range(iris$Sepal.Width, na.rm = TRUE))
      
      $Petal.Length
      updateSliderInput(inputId = "Petal.Length", min = min(iris$Petal.Length, 
          na.rm = TRUE), max = max(iris$Petal.Length, na.rm = TRUE), 
          value = range(iris$Petal.Length, na.rm = TRUE))
      
      $Petal.Width
      updateSliderInput(inputId = "Petal.Width", min = min(iris$Petal.Width, 
          na.rm = TRUE), max = max(iris$Petal.Width, na.rm = TRUE), 
          value = range(iris$Petal.Width, na.rm = TRUE))
      
      $Species
      updateSelectInput(inputId = "Species", choices = levels(as.factor(iris$Species)), 
          selected = levels(as.factor(iris$Species)), multiple = TRUE)
      

---

    Code
      detect_controller(cars2, "testthat", c(speed = "a", dist = "b"))
    Output
      $speed
      sliderInput(inputId = "testthat-speed", label = "b", min = 4, 
          max = 25, value = c(4, 25))
      
      $dist
      sliderInput(inputId = "testthat-dist", label = "a", min = 2, 
          max = 120, value = c(2, 120))
      

---

    Code
      detect_controller(iris, "testthat", external = "Species")
    Output
      $Sepal.Length
      sliderInput(inputId = "testthat-Sepal.Length", label = "Sepal.Length", 
          min = 4.3, max = 7.9, value = c(4.3, 7.9))
      
      $Sepal.Width
      sliderInput(inputId = "testthat-Sepal.Width", label = "Sepal.Width", 
          min = 2, max = 4.4, value = c(2, 4.4))
      
      $Petal.Length
      sliderInput(inputId = "testthat-Petal.Length", label = "Petal.Length", 
          min = 1, max = 6.9, value = c(1, 6.9))
      
      $Petal.Width
      sliderInput(inputId = "testthat-Petal.Width", label = "Petal.Width", 
          min = 0.1, max = 2.5, value = c(0.1, 2.5))
      

# logical_controller works

    Code
      logical_controller(cars2, "testthat")
    Output
      selectizeInput(inputId = "testthat-logi", label = "Various", 
          choices = c("high_speed", "high_dist"), multiple = TRUE, 
          options = list(placeholder = "select", onInitialize = "function() { this.setValue(null); }"))

---

    Code
      logical_controller(cars2, "testthat", labels = "a")
    Output
      selectizeInput(inputId = "testthat-logi", label = "a", choices = c("high_speed", 
      "high_dist"), multiple = TRUE, options = list(placeholder = "select", 
          onInitialize = "function() { this.setValue(null); }"))

---

    Code
      logical_controller(cars2, "testthat", labels = "a", update = TRUE)
    Output
      updateSelectizeInput(session = "testthat", inputId = "logi", 
          label = "a", choices = detect_lgl(cars2, ignore = "", external = ""), 
          selected = shiny::isolate(input$logi))

# switch_controller works

    Code
      switch_controller(cars)
    Output
      $speed
      shinyjs::toggleState("speed", length(unique(cars$speed)) > 1)
      
      $dist
      shinyjs::toggleState("dist", length(unique(cars$dist)) > 1)
      

---

    Code
      switch_controller(iris)
    Output
      $Sepal.Length
      shinyjs::toggleState("Sepal.Length", length(unique(iris$Sepal.Length)) > 
          1)
      
      $Sepal.Width
      shinyjs::toggleState("Sepal.Width", length(unique(iris$Sepal.Width)) > 
          1)
      
      $Petal.Length
      shinyjs::toggleState("Petal.Length", length(unique(iris$Petal.Length)) > 
          1)
      
      $Petal.Width
      shinyjs::toggleState("Petal.Width", length(unique(iris$Petal.Width)) > 
          1)
      
      $Species
      shinyjs::toggleState("Species", length(levels(as.factor(iris$Species))) > 
          1)
      

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

