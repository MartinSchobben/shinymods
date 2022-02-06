library(shiny)

ui <- fluidPage(
  filter_ui("test", iris, c("Speed", "Distribution"), "other", shinyjs = TRUE),
  mainPanel(tableOutput("table"))
)

server <- function(input, output, session) {
  ft <- filter_server("test", reactive(iris), shinyjs = TRUE)
  output$table <- renderTable({req(ft()); ft()})
}


shinyApp(ui, server)
