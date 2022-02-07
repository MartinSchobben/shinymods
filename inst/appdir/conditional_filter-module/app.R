library(shiny)
library(shinyjs)
library(shinymods)

ui <- fluidPage(
  filter_ui("test", cars2, c("Speed", "Distribution"), "other", shinyjs = TRUE),
  mainPanel(tableOutput("table"))
)

server <- function(input, output, session) {
  ft <- filter_server("test", reactive(cars2), shinyjs = TRUE)
  output$table <- renderTable({req(ft()); ft()})
}

shinyApp(ui, server)

ui <- fluidPage(
  filter_ui("test", dat = iris, external = "Sepal.Width", shinyjs = TRUE),
  mainPanel(tableOutput("table"))
)

server <- function(input, output, session) {
  ft <- filter_server(
    "test",
    dat = reactive(iris),
    external = reactiveValues(Sepal.Width = c(2.3, 3.6)),
    shinyjs = TRUE
    )
  output$table <- renderTable({req(ft()); ft()})
}

shinyApp(ui, server)
