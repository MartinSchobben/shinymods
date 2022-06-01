library(shiny)
library(shinyjs)
library(shinymods)

ui <- fluidPage(
        sidebarLayout(
          sidebarPanel(
            dataset_ui("test", filter = is.data.frame),
            filter_ui("test")
          ),
          mainPanel(
            tableOutput("table")
          )
        )
      )


server <- function(input, output, session) {
  dt <- dataset_server("test")
  ft <- filter_server(dt, "test", shinyjs = FALSE)
  output$table <- renderTable({req(ft()); ft()})
}

shinyApp(ui, server)
