# gui and update gui can be generated

    Code
      col_vals("numeric", "cars2$speed", "test", "speed", NULL, FALSE)
    Output
      $inputId
      [1] "test"
      
      $min
      min(cars2$speed, na.rm = TRUE)
      
      $max
      max(cars2$speed, na.rm = TRUE)
      
      $value
      range(cars2$speed, na.rm = TRUE)
      
      $label
      [1] "speed"
      

---

    Code
      col_vals("numeric", "cars2$speed", "test", "speed", NULL, TRUE)
    Output
      $inputId
      [1] "test"
      
      $value
      range(cars2$speed, na.rm = TRUE)
      

---

    Code
      col_vals("character", "iris$Species", "test", "species", NULL, FALSE)
    Output
      $inputId
      [1] "test"
      
      $choices
      unique(na.omit(iris$Species))
      
      $selected
      unique(na.omit(iris$Species))
      
      $label
      [1] "species"
      
      $multiple
      [1] TRUE
      

---

    Code
      col_vals("character", "iris$Species", "species", update = TRUE)
    Output
      $inputId
      [1] "species"
      
      $choices
      unique(na.omit(iris$Species))
      
      $selected
      unique(na.omit(iris$Species))
      

---

    Code
      variable_names(cars2)
    Output
      [1] "speed" "dist" 

---

    Code
      variable_names(cars2, ignore = "speed")
    Output
      [1] "dist"

---

    Code
      filter_ui("test", dat = cars2, labels = c("Speed", "Distribution"), logi = "other")
    Output
      [[1]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-speed-label" for="test-speed">Speed</label>
        <input class="js-range-slider" id="test-speed" data-skin="shiny" data-type="double" data-min="4" data-max="25" data-from="4" data-to="25" data-step="1" data-grid="true" data-grid-num="7" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
      </div>
      
      [[2]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-dist-label" for="test-dist">Distribution</label>
        <input class="js-range-slider" id="test-dist" data-skin="shiny" data-type="double" data-min="2" data-max="120" data-from="2" data-to="120" data-step="1" data-grid="true" data-grid-num="9.83333333333333" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
      </div>
      
      [[3]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-logi-label" for="test-logi">other</label>
        <div>
          <select id="test-logi" class="form-control" multiple="multiple"><option value="high_speed">high_speed</option>
      <option value="high_dist">high_dist</option></select>
          <script type="application/json" data-for="test-logi" data-eval="[&quot;onInitialize&quot;]">{"placeholder":"select","onInitialize":"function() { this.setValue(null); }","plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      filter_ui("test", dat = cars2, labels = c("Speed", "Distribution"))
    Output
      [[1]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-speed-label" for="test-speed">Speed</label>
        <input class="js-range-slider" id="test-speed" data-skin="shiny" data-type="double" data-min="4" data-max="25" data-from="4" data-to="25" data-step="1" data-grid="true" data-grid-num="7" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
      </div>
      
      [[2]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-dist-label" for="test-dist">Distribution</label>
        <input class="js-range-slider" id="test-dist" data-skin="shiny" data-type="double" data-min="2" data-max="120" data-from="2" data-to="120" data-step="1" data-grid="true" data-grid-num="9.83333333333333" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
      </div>
      
      [[3]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-logi-label" for="test-logi">Various</label>
        <div>
          <select id="test-logi" class="form-control" multiple="multiple"><option value="high_speed">high_speed</option>
      <option value="high_dist">high_dist</option></select>
          <script type="application/json" data-for="test-logi" data-eval="[&quot;onInitialize&quot;]">{"placeholder":"select","onInitialize":"function() { this.setValue(null); }","plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      detect_control(cars2, external = "dist")
    Output
      [1] "sliderInput"    "selectizeInput"

---

    Code
      detect_control(cars2, external = "dist", ignore = "speed")
    Output
      [1] "selectizeInput"

---

    Code
      filter_ui("test", dat = cars2, external = "speed", labels = "Distribution",
        logi = "other")
    Output
      [[1]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-dist-label" for="test-dist">Distribution</label>
        <input class="js-range-slider" id="test-dist" data-skin="shiny" data-type="double" data-min="2" data-max="120" data-from="2" data-to="120" data-step="1" data-grid="true" data-grid-num="9.83333333333333" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-drag-interval="true" data-data-type="number"/>
      </div>
      
      [[2]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-logi-label" for="test-logi">other</label>
        <div>
          <select id="test-logi" class="form-control" multiple="multiple"><option value="high_speed">high_speed</option>
      <option value="high_dist">high_dist</option></select>
          <script type="application/json" data-for="test-logi" data-eval="[&quot;onInitialize&quot;]">{"placeholder":"select","onInitialize":"function() { this.setValue(null); }","plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      filter_ui("test", dat = cars2, external = "speed", labels = "Distribution",
        logi = "other", ignore = "dist")
    Output
      [[1]]
      <div class="form-group shiny-input-container">
        <label class="control-label" id="test-logi-label" for="test-logi">other</label>
        <div>
          <select id="test-logi" class="form-control" multiple="multiple"><option value="high_speed">high_speed</option>
      <option value="high_dist">high_dist</option></select>
          <script type="application/json" data-for="test-logi" data-eval="[&quot;onInitialize&quot;]">{"placeholder":"select","onInitialize":"function() { this.setValue(null); }","plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      filter_ui(dat = cars2, external = "speed", update = TRUE, shinyjs = FALSE)
    Output
      $dist
      shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
          na.rm = TRUE))
      
      $logi
      shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
          ignore = NULL, external = "speed"), selected = shiny::isolate(input$logi))
      

---

    Code
      filter_ui(dat = cars2, external = "speed", update = TRUE, shinyjs = TRUE)
    Output
      $dist
      $dist[[1]]
      shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
          na.rm = TRUE))
      
      $dist[[2]]
      shinyjs::toggleState("dist", length(unique(cars2$dist)) > 1)
      
      
      $logi
      $logi[[1]]
      shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
          ignore = NULL, external = "speed"), selected = shiny::isolate(input$logi))
      
      $logi[[2]]
      shinyjs::toggleState("logi", length(logi_cols(cars2, ignore = NULL, 
          external = "speed")) > 0)
      
      

---

    Code
      filter_ui(dat = cars2, update = TRUE)
    Output
      $speed
      shiny::updateSliderInput(inputId = "speed", value = range(cars2$speed, 
          na.rm = TRUE))
      
      $dist
      shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
          na.rm = TRUE))
      
      $logi
      shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
          ignore = NULL, external = NULL), selected = shiny::isolate(input$logi))
      

---

    Code
      filter_ui(dat = iris, update = TRUE)
    Output
      $Sepal.Length
      shiny::updateSliderInput(inputId = "Sepal.Length", value = range(iris$Sepal.Length, 
          na.rm = TRUE))
      
      $Sepal.Width
      shiny::updateSliderInput(inputId = "Sepal.Width", value = range(iris$Sepal.Width, 
          na.rm = TRUE))
      
      $Petal.Length
      shiny::updateSliderInput(inputId = "Petal.Length", value = range(iris$Petal.Length, 
          na.rm = TRUE))
      
      $Petal.Width
      shiny::updateSliderInput(inputId = "Petal.Width", value = range(iris$Petal.Width, 
          na.rm = TRUE))
      
      $Species
      shiny::updateSelectInput(inputId = "Species", choices = unique(na.omit(iris$Species)), 
          selected = unique(na.omit(iris$Species)))
      

---

    Code
      filter_ui(dat = iris, update = TRUE, shinyjs = TRUE)
    Output
      $Sepal.Length
      $Sepal.Length[[1]]
      shiny::updateSliderInput(inputId = "Sepal.Length", value = range(iris$Sepal.Length, 
          na.rm = TRUE))
      
      $Sepal.Length[[2]]
      shinyjs::toggleState("Sepal.Length", length(unique(iris$Sepal.Length)) > 
          1)
      
      
      $Sepal.Width
      $Sepal.Width[[1]]
      shiny::updateSliderInput(inputId = "Sepal.Width", value = range(iris$Sepal.Width, 
          na.rm = TRUE))
      
      $Sepal.Width[[2]]
      shinyjs::toggleState("Sepal.Width", length(unique(iris$Sepal.Width)) > 
          1)
      
      
      $Petal.Length
      $Petal.Length[[1]]
      shiny::updateSliderInput(inputId = "Petal.Length", value = range(iris$Petal.Length, 
          na.rm = TRUE))
      
      $Petal.Length[[2]]
      shinyjs::toggleState("Petal.Length", length(unique(iris$Petal.Length)) > 
          1)
      
      
      $Petal.Width
      $Petal.Width[[1]]
      shiny::updateSliderInput(inputId = "Petal.Width", value = range(iris$Petal.Width, 
          na.rm = TRUE))
      
      $Petal.Width[[2]]
      shinyjs::toggleState("Petal.Width", length(unique(iris$Petal.Width)) > 
          1)
      
      
      $Species
      $Species[[1]]
      shiny::updateSelectInput(inputId = "Species", choices = unique(na.omit(iris$Species)), 
          selected = unique(na.omit(iris$Species)))
      
      $Species[[2]]
      shinyjs::toggleState("Species", length(unique(na.omit(iris$Species))) > 
          1)
      
      

# that server update functions work

    Code
      hdl1
    Output
      $speed
      $speed[[1]]
      shiny::updateSliderInput(inputId = "speed", value = range(cars2$speed, 
          na.rm = TRUE))
      
      $speed[[2]]
      shinyjs::toggleState("speed", length(unique(cars2$speed)) > 1)
      
      
      $dist
      $dist[[1]]
      shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
          na.rm = TRUE))
      
      $dist[[2]]
      shinyjs::toggleState("dist", length(unique(cars2$dist)) > 1)
      
      
      $logi
      $logi[[1]]
      shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
          ignore = NULL, external = NULL), selected = shiny::isolate(input$logi))
      
      $logi[[2]]
      shinyjs::toggleState("logi", length(logi_cols(cars2, ignore = NULL, 
          external = NULL)) > 0)
      
      

---

    Code
      observe_builder("speed", hdl1, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$speed, handlerExpr = {
          req(any(filter_var(cars2$speed, input2$speed, remove_na = FALSE)), 
              cancelOutput = TRUE)
          shinyjs::toggleState("logi", length(logi_cols(cars2, ignore = NULL, 
              external = NULL)) > 0)
          shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
              ignore = NULL, external = NULL), selected = shiny::isolate(input$logi))
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
          shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
              na.rm = TRUE))
      })

---

    Code
      observe_builder("logi", hdl1, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$logi, handlerExpr = {
          req(length(logi_cols(cars2(), external = external, ignore = ignore)) > 
              0, cancelOutput = TRUE)
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
          shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
              na.rm = TRUE))
          shinyjs::toggleState("speed", length(unique(cars2$speed)) > 
              1)
          shiny::updateSliderInput(inputId = "speed", value = range(cars2$speed, 
              na.rm = TRUE))
      })

---

    Code
      hdl2
    Output
      $dist
      $dist[[1]]
      shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
          na.rm = TRUE))
      
      $dist[[2]]
      shinyjs::toggleState("dist", length(unique(cars2$dist)) > 1)
      
      
      $logi
      $logi[[1]]
      shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
          ignore = NULL, external = "speed"), selected = shiny::isolate(input$logi))
      
      $logi[[2]]
      shinyjs::toggleState("logi", length(logi_cols(cars2, ignore = NULL, 
          external = "speed")) > 0)
      
      

---

    Code
      observe_builder("speed", hdl2, dat = cars2, show = TRUE)
    Output
      shiny::observeEvent(eventExpr = input2$speed, handlerExpr = {
          req(any(filter_var(cars2$speed, input2$speed, remove_na = FALSE)), 
              cancelOutput = TRUE)
          shinyjs::toggleState("logi", length(logi_cols(cars2, ignore = NULL, 
              external = "speed")) > 0)
          shiny::updateSelectizeInput(inputId = "logi", choices = logi_cols(cars2, 
              ignore = NULL, external = "speed"), selected = shiny::isolate(input$logi))
          shinyjs::toggleState("dist", length(unique(cars2$dist)) > 
              1)
          shiny::updateSliderInput(inputId = "dist", value = range(cars2$dist, 
              na.rm = TRUE))
      })

