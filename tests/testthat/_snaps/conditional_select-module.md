# gui and update gui can be generated

    Code
      col_vals("numeric", "xc$speed", "test", "speed", NULL, FALSE)
    Output
      $min
      min(xc$speed, na.rm = TRUE)
      
      $max
      max(xc$speed, na.rm = TRUE)
      
      $value
      range(xc$speed, na.rm = TRUE)
      
      $inputId
      [1] "test"
      
      $label
      [1] "speed"
      

---

    Code
      col_vals("numeric", "xc$speed", "test", "speed", NULL, TRUE)
    Output
      $min
      min(xc$speed, na.rm = TRUE)
      
      $max
      max(xc$speed, na.rm = TRUE)
      
      $value
      range(xc$speed, na.rm = TRUE)
      
      $inputId
      [1] "test"
      

---

    Code
      col_vals("character", "iris$Species", "test", "species", NULL, FALSE)
    Output
      $choices
      levels(as.factor(iris$Species))
      
      $selected
      levels(as.factor(iris$Species))
      
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
      levels(as.factor(iris$Species))
      
      $selected
      levels(as.factor(iris$Species))
      
      $inputId
      [1] "species"
      

---

    Code
      filter_ui("test", xc, c("Speed", "Distribution"), "other")
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
          <select id="test-logi"><option value="lg1" selected>lg1</option>
      <option value="lg2">lg2</option></select>
          <script type="application/json" data-for="test-logi" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      filter_ui("test", xc, c("Speed", "Distribution"))
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
          <select id="test-logi"><option value="lg1" selected>lg1</option>
      <option value="lg2">lg2</option></select>
          <script type="application/json" data-for="test-logi" data-nonempty="">{"plugins":["selectize-plugin-a11y"]}</script>
        </div>
      </div>
      

---

    Code
      filter_ui(dat = xc, update = TRUE)
    Output
      $speed
      shiny::updateSliderInput(min = min(xc$speed, na.rm = TRUE), max = max(xc$speed, 
          na.rm = TRUE), value = range(xc$speed, na.rm = TRUE), inputId = "speed")
      
      $dist
      shiny::updateSliderInput(min = min(xc$dist, na.rm = TRUE), max = max(xc$dist, 
          na.rm = TRUE), value = range(xc$dist, na.rm = TRUE), inputId = "dist")
      
      $logical
      shiny::updateSelectInput(inputId = "logi", choices = c("lg1", 
      "lg2"))
      

---

    Code
      filter_ui(dat = iris, update = TRUE)
    Output
      $Sepal.Length
      shiny::updateSliderInput(min = min(iris$Sepal.Length, na.rm = TRUE), 
          max = max(iris$Sepal.Length, na.rm = TRUE), value = range(iris$Sepal.Length, 
              na.rm = TRUE), inputId = "Sepal.Length")
      
      $Sepal.Width
      shiny::updateSliderInput(min = min(iris$Sepal.Width, na.rm = TRUE), 
          max = max(iris$Sepal.Width, na.rm = TRUE), value = range(iris$Sepal.Width, 
              na.rm = TRUE), inputId = "Sepal.Width")
      
      $Petal.Length
      shiny::updateSliderInput(min = min(iris$Petal.Length, na.rm = TRUE), 
          max = max(iris$Petal.Length, na.rm = TRUE), value = range(iris$Petal.Length, 
              na.rm = TRUE), inputId = "Petal.Length")
      
      $Petal.Width
      shiny::updateSliderInput(min = min(iris$Petal.Width, na.rm = TRUE), 
          max = max(iris$Petal.Width, na.rm = TRUE), value = range(iris$Petal.Width, 
              na.rm = TRUE), inputId = "Petal.Width")
      
      $Species
      shiny::updateSelectInput(choices = levels(as.factor(iris$Species)), 
          selected = levels(as.factor(iris$Species)), inputId = "Species")
      

# that server update functions work

    Code
      hdl
    Output
      $speed
      shiny::updateSliderInput(min = min(xc$speed, na.rm = TRUE), max = max(xc$speed, 
          na.rm = TRUE), value = range(xc$speed, na.rm = TRUE), inputId = "speed")
      
      $dist
      shiny::updateSliderInput(min = min(xc$dist, na.rm = TRUE), max = max(xc$dist, 
          na.rm = TRUE), value = range(xc$dist, na.rm = TRUE), inputId = "dist")
      
      $logical
      shiny::updateSelectInput(inputId = "logi", choices = c("lg1", 
      "lg2"))
      

