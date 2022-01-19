test_that("download module server works", {
  ui <- fluidPage(output_ui("data", n = 1))
  server <- function(input, output, session) {
    output_server("data", reactive(cars), "cars", 1, ',')
  }
  app <- shinytest::ShinyDriver$new(shinyApp(ui, server))
  app$click("download1")
  app$getValue("download1")
})
