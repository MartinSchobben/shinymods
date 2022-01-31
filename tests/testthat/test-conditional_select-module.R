test_that("gui and update gui can be generated", {
  # dataset
  xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))
  expect_equal(
    detect_control(xc),
    c("sliderInput", "sliderInput", "selectInput")
    )
  expect_snapshot(
    col_vals("numeric", "xc$speed", "test", "speed", NULL, FALSE)
  )
  expect_snapshot(
    col_vals("numeric", "xc$speed", "test", "speed", NULL, TRUE)
  )
  expect_snapshot(
    col_vals("character", "iris$Species", "test", "species", NULL, FALSE)
  )
  expect_snapshot(
    col_vals("character", "iris$Species", "species", update = TRUE)
  )
  # custom labels
  expect_snapshot(
    filter_ui("test", xc, c("Speed", "Distribution"), "other")
  )
  # default label for logical
  expect_snapshot(
    filter_ui("test", xc, c("Speed", "Distribution"))
  )
  # no labels for update
  expect_snapshot(
    filter_ui(dat = xc, update = TRUE)
  )
  expect_snapshot(
    filter_ui(dat = iris, update = TRUE)
  )
  expect_snapshot(
    filter_ui(dat = iris, update = TRUE, shinyjs = TRUE)
  )
})


test_that("that server update functions work",{
  # dataset
  xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))
  input <- list()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  vars <- c("speed", "dist", "logi")

  # the controllers
  hdl <- filter_ui(dat = xc, update = TRUE, shinyjs = TRUE)
  expect_snapshot(hdl)
  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl, dat = xc, show = TRUE)
    )

})

test_that("that server update functions work",{
  # dataset
  xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))
  reactiveConsole(TRUE)
  input <- reactiveValues()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  vars <- reactive(c("speed", "dist", "logi"))
  dat <- reactive(xc)

  # the controllers
  hdl <- filter_ui(dat = dat(), update = TRUE, shinyjs = TRUE)
  expect_snapshot(hdl)
  # the observer event
  expect_invisible(
    observe_builder("speed", hdl, dat = dat())
  )
  reactiveConsole(FALSE)
})

# test_that("server can filter", {
#   x <- reactiveVal(xc)
#   testServer(filter_server, args = list(dat = x), {
#     # set
#     # x(xc)
#     session$flushReact()
#     # get
#     dataset <- session$getReturned()
#     # input
#     # session$setInputs(speed = c(4, 13))
#     print(dataset())
#     # session$setInputs(dist = c(60, 120))
#
#   })
#   dplyr::filter(xc, .data$speed < 13) %>% dplyr::pull(dist) %>% range()
# })

xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))
#
ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(filter_ui("test", xc, shinyjs = T)),
    mainPanel(tableOutput("table"))
  )
)
server <- function(input, output, session) {
  ft <- filter_server("test", reactive(xc), shinyjs =T)
  output$table <- renderTable({req(ft()); ft()})

}

shinyApp(ui, server)
