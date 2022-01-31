xc <- cbind(lg1 = rep(TRUE, nrow(cars)), cars, lg2 = rep(TRUE, nrow(cars)))

test_that("gui and update gui can be generated", {
  expect_equal(
    detect_control(xc),
    c("sliderInput", "sliderInput", "selectInput")
    )
  expect_snapshot(
    col_vals(xc$speed, "id", "speed", NULL, FALSE)
  )
  expect_snapshot(
    col_vals(xc$speed, "id", "speed", NULL, TRUE)
  )
  expect_snapshot(
    col_vals("character", "iris$Species", "id", "species", NULL, FALSE)
  )
  expect_snapshot(
    col_vals(iris$Species, "id", "species", NULL, FALSE)
  )
  expect_snapshot(
    filter_ui(NS("test", "id"), xc, c("Speed", "Distribution"), "other")
  )
  # default label for logical
  expect_snapshot(
    filter_ui("test", xc, c("Speed", "Distribution"))
  )
  # no labels for update
  expect_snapshot(
    filter_ui(dat = xc, update = TRUE)
  )
})


test_that("that server works" {
  reactiveConsole(TRUE)
  input <- reactiveValues()
  input <- list()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  vars <- reactive(c("speed", "dist", "logi"))
  dat <- reactive(xc)
  vars <- c("speed", "dist", "logi")

  #vars <- c("speed", "dist", "logi")
  hdl <- reactive(filter_ui(id = "id", data = xc, update = TRUE, session = getDefaultReactiveDomain()))

  hdl <- filter_ui(id = "id", data = xc, update = TRUE, session = getDefaultReactiveDomain())
  purrr::map(vars(), ~observe_builder(.x,  hdl))

  reactiveConsole(FALSE)
})


test_that("server can filter", {
  x <- reactiveVal(xc)
  testServer(filter_server, args = list(data = x), {
    # set
    # x(xc)
    # session$flushReact()
    # get
    # dataset <- session$getReturned()
    # input
    session$setInputs(speed = c(15, 25))
    session$setInputs(dist = c(60, 120))
    print(hdl())
  })
})

test_that("server can update controls", {
  x <- reactiveVal(xc[1:20,])
  y <- reactiveVal( c("speed", "dist", "logi"))
  testServer(cond_server, args = list(data = x, vars = y), {
    # set
    # x(xc)
    # session$flushReact()
    # get
    # dataset <- session$getReturned()
    # input

    print(hdl())
  })
})

ui <- fluidPage(
  filter_ui("test", xc, c("Speed", "Distribution"), "other")
)
server <- function(input, output, session) {
  filter_server("test", reactive(xc))
}

shinyApp(ui, server)
