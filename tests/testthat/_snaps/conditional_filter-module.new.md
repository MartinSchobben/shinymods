# gui and update gui can be generated

    Code
      col_vals("numeric", "cars2$speed", "test", "speed", NULL, FALSE)
    Output
      $min
      min(cars2$speed, na.rm = TRUE)
      
      $max
      max(cars2$speed, na.rm = TRUE)
      
      $value
      range(cars2$speed, na.rm = TRUE)
      
      $inputId
      [1] "test"
      
      $label
      [1] "speed"
      

---

    Code
      col_vals("numeric", "cars2$speed", "test", "speed", NULL, TRUE)
    Output
      $min
      min(cars2$speed, na.rm = TRUE)
      
      $max
      max(cars2$speed, na.rm = TRUE)
      
      $value
      range(cars2$speed, na.rm = TRUE)
      
      $inputId
      [1] "test"
      

---

    Code
      col_vals("character", "iris$Species", "test", "species", NULL, FALSE)
    Output
      $choices
      unique(iris$Species)
      
      $selected
      unique(iris$Species)
      
      $inputId
      [1] "test"
      
      $label
      [1] "species"
      
      $multiple
      [1] TRUE
      

---

    Code
      col_vals("character", "iris$Species", "species", update = TRUE)
    Output
      $choices
      unique(iris$Species)
      
      $selected
      unique(iris$Species)
      
      $inputId
      [1] "species"
      

---

    Code
      filter_ui("test", cars2, labels = c("Speed", "Distribution"), logi = "other")
    Output
      list()

---

    Code
      filter_ui("test", cars2, labels = c("Speed", "Distribution"))
    Output
      list()

---

    Code
      filter_ui(dat = cars2, update = TRUE)
    Output
      named list()

---

    Code
      filter_ui(dat = iris, update = TRUE)
    Output
      named list()

---

    Code
      filter_ui(dat = iris, update = TRUE, shinyjs = TRUE)
    Output
      named list()

# that server update functions work

    Code
      hdl
    Output
      named list()

---

    Code
      observe_builder("speed", hdl, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input$speed, handlerExpr = {
          req(any(filter_var(cars2$speed, input$speed, remove_na = FALSE)), 
              cancelOutput = TRUE)
      })

