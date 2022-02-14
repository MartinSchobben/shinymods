library(shiny)
library(shinyjs)
library(shinymods)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  sidebarLayout(
    sidebarPanel(
      filter_ui(
        "test",
        dat = cars2,
        labels = c("Speed", "Distribution"),
        logi = "Other",
        shinyjs = TRUE
      )
  ),
  mainPanel(tableOutput("table"))
  )
)

server <- function(input, output, session) {
  ft <- filter_server("test", reactive(cars2), shinyjs = TRUE)
  output$table <- renderTable({req(ft()); ft()})
}

shinyApp(ui, server)
#
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   sidebarLayout(
#     sidebarPanel(
#       filter_ui(
#         "test",
#         dat = iris,
#         external = "Sepal.Width",
#         ignore = "Sepal.Length",
#         shinyjs = TRUE
#         )
#       ),
#     mainPanel(tableOutput("table"))
#   )
# )
#
# server <- function(input, output, session) {
#   ft <- filter_server(
#     "test",
#     dat = reactive(iris),
#     external = reactiveValues(Sepal.Width = c(2.3, 3.6)),
#     ignore =  "Sepal.Length",
#     shinyjs = TRUE
#     )
#   output$table <- renderTable({req(ft()); ft()})
# }
#
# shinyApp(ui, server)
#
#
# RGS <- RGS::get_standard_business_reporting("Nederland")
# ignore_vars <- c("referentie_omslagcode", "sortering", "referentienummer",
#                  "omschrijving_verkort", "omschrijving", "terminal")
#
# ui <- fluidPage(
#   shinyjs::useShinyjs(),
#   sidebarLayout(
#     sidebarPanel(
#       filter_ui(
#         "test",
#         dat = RGS,
#         external = "referentiecode",
#         ignore = ignore_vars,
#         shinyjs = TRUE
#       )
#     ),
#     mainPanel(tableOutput("table"))
#   )
# )
#
# server <- function(input, output, session) {
#   ft <- filter_server(
#     "test",
#     dat = reactive(RGS),
#     external = reactiveValues(referentiecode = RGS:::find_children(RGS, "")),
#     ignore =  ignore_vars,
#     shinyjs = TRUE
#   )
#   output$table <- renderTable({req(ft()); ft()})
# }
#
# shinyApp(ui, server)
