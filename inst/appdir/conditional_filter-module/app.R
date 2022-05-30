library(shiny)
library(shinyjs)
library(shinymods)

ui <- fluidPage(
        sidebarLayout(
          sidebarPanel(
            # dataset_ui("test", filter = is.data.frame),
            filter_ui("test")
          ),
          mainPanel(
            tableOutput("table")
          )
        )
      )


server <- function(input, output, session) {
  # dt <- dataset_server("test")
  ft <- filter_server(reactive(cars), "test", shinyjs = F)
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
