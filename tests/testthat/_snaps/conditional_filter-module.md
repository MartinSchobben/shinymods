# gui and update gui can be generated

    Code
      filter_controllers(cars, session)
    Output
      $speed
      sliderInput(inputId = "testthat-speed", label = "speed", min = 4, 
          max = 25, value = c(4, 25))
      
      $dist
      sliderInput(inputId = "testthat-dist", label = "dist", min = 2, 
          max = 120, value = c(2, 120))
      

---

    Code
      filter_controllers(cars, session, update = T)
    Output
      $speed
      updateSliderInput(session = <environment>, inputId = "speed", 
          value = range(cars$speed, na.rm = TRUE))
      
      $dist
      updateSliderInput(session = <environment>, inputId = "dist", 
          value = range(cars$dist, na.rm = TRUE))
      

---

    Code
      filter_controllers(cars, session, update = F, shinyjs = T)
    Output
      [[1]]
      shinyjs::useShinyjs()
      
      $speed
      sliderInput(inputId = "testthat-speed", label = "speed", min = 4, 
          max = 25, value = c(4, 25))
      
      $dist
      sliderInput(inputId = "testthat-dist", label = "dist", min = 2, 
          max = 120, value = c(2, 120))
      

---

    Code
      filter_controllers(cars, session, update = T, shinyjs = T)
    Output
      $speed
      $speed[[1]]
      shinyjs::toggleState("speed", length(unique(cars$speed)) > 1)
      
      $speed[[2]]
      updateSliderInput(session = <environment>, inputId = "speed", 
          value = range(cars$speed, na.rm = TRUE))
      
      
      $dist
      $dist[[1]]
      shinyjs::toggleState("dist", length(unique(cars$dist)) > 1)
      
      $dist[[2]]
      updateSliderInput(session = <environment>, inputId = "dist", 
          value = range(cars$dist, na.rm = TRUE))
      
      

---

    Code
      filter_controllers(cars, session, external = character(0), update = TRUE)
    Output
      $speed
      updateSliderInput(session = <environment>, inputId = "speed", 
          value = range(cars$speed, na.rm = TRUE))
      
      $dist
      updateSliderInput(session = <environment>, inputId = "dist", 
          value = range(cars$dist, na.rm = TRUE))
      

# helpers work

    Code
      variable_names(cars)
    Output
      [1] "speed" "dist" 

---

    Code
      variable_names(cars, "speed")
    Output
      [1] "dist"

# that server update functions work

    Code
      hdl1
    Output
      $speed
      $speed[[1]]
      shinyjs::toggleState("speed", length(unique(cars2$speed)) > 1)
      
      $speed[[2]]
      updateSliderInput(session = <environment>, inputId = "speed", 
          value = range(cars2$speed, na.rm = TRUE))
      
      
      $dist
      $dist[[1]]
      shinyjs::toggleState("dist", length(unique(cars2$dist)) > 1)
      
      $dist[[2]]
      updateSliderInput(session = <environment>, inputId = "dist", 
          value = range(cars2$dist, na.rm = TRUE))
      
      
      $logi
      $logi[[1]]
      shinyjs::toggleState("logi", length(detect_lgl(cars2, ignore = "", 
          external = "")) > 1)
      
      $logi[[2]]
      updateSelectizeInput(session = <environment>, inputId = "logi", 
          label = "", choices = detect_lgl(cars2, ignore = "", external = ""), 
          selected = shiny::isolate(input$logi))
      
      

---

    Code
      observe_builder("speed", hdl1, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$speed, handlerExpr = {
          req(any(filter_var(cars2$speed, input2$speed, remove_na = FALSE)), 
              cancelOutput = TRUE)
          updateSelectizeInput(session = <environment>, inputId = "logi", 
              label = "", choices = detect_lgl(cars2, ignore = "", 
                  external = ""), selected = shiny::isolate(input$logi))
          shinyjs::toggleState("logi", length(detect_lgl(cars2, ignore = "", 
              external = "")) > 1)
          updateSliderInput(session = <environment>, inputId = "dist", 
              value = range(cars2$dist, na.rm = TRUE))
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
      })

---

    Code
      observe_builder("logi", hdl1, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$logi, handlerExpr = {
          req(detect_lgl(cars2, external = external, ignore = ignore), 
              cancelOutput = TRUE)
          updateSliderInput(session = <environment>, inputId = "dist", 
              value = range(cars2$dist, na.rm = TRUE))
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
          updateSliderInput(session = <environment>, inputId = "speed", 
              value = range(cars2$speed, na.rm = TRUE))
          shinyjs::toggleState("speed", length(unique(cars2$speed)) > 
              1)
      })

---

    Code
      hdl2
    Output
      $speed
      shinyjs::toggleState("speed", length(unique(cars2$speed)) > 1)
      
      $dist
      $dist[[1]]
      shinyjs::toggleState("dist", length(unique(cars2$dist)) > 1)
      
      $dist[[2]]
      updateSliderInput(session = <environment>, inputId = "dist", 
          value = range(cars2$dist, na.rm = TRUE))
      
      
      $logi
      $logi[[1]]
      shinyjs::toggleState("logi", length(detect_lgl(cars2, ignore = "", 
          external = "speed")) > 1)
      
      $logi[[2]]
      updateSelectizeInput(session = <environment>, inputId = "logi", 
          label = "", choices = detect_lgl(cars2, ignore = "", external = "speed"), 
          selected = shiny::isolate(input$logi))
      
      

---

    Code
      observe_builder("speed", hdl2, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$speed, handlerExpr = {
          req(any(filter_var(cars2$speed, input2$speed, remove_na = FALSE)), 
              cancelOutput = TRUE)
          updateSelectizeInput(session = <environment>, inputId = "logi", 
              label = "", choices = detect_lgl(cars2, ignore = "", 
                  external = "speed"), selected = shiny::isolate(input$logi))
          shinyjs::toggleState("logi", length(detect_lgl(cars2, ignore = "", 
              external = "speed")) > 1)
          updateSliderInput(session = <environment>, inputId = "dist", 
              value = range(cars2$dist, na.rm = TRUE))
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
      })

