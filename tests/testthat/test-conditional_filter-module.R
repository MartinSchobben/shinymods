test_that("gui and update gui can be generated", {
  expect_equal(
    detect_control(cars2),
    c("sliderInput", "sliderInput", "selectizeInput")
    )
  expect_equal(
    detect_control(cars2),
    c("updateSliderInput", "updateSliderInput", "updateSelectizeInput")
  )
  expect_snapshot(
    col_vals("numeric", "cars2$speed", "test", "speed", NULL, FALSE)
  )
  expect_snapshot(
    col_vals("numeric", "cars2$speed", "test", "speed", NULL, TRUE)
  )
  expect_snapshot(
    col_vals("character", "iris$Species", "test", "species", NULL, FALSE)
  )
  expect_snapshot(
    col_vals("character", "iris$Species", "species", update = TRUE)
  )
  # custom labels
  expect_snapshot(
    filter_ui("test", dat = cars2, labels  = c("Speed", "Distribution"), logi = "other")
  )
  # default label for logical
  expect_snapshot(
    filter_ui("test", dat = cars2, labels  = c("Speed", "Distribution"))
  )
  # external filter (e.g. from a different module with e.g. plot selection)
  expect_snapshot(
    detect_control(cars2, external = "dist")
  )
  expect_snapshot(
    filter_ui("test", dat = cars2, external = "speed", labels  = "Distribution", logi = "other")
  )
  expect_snapshot(
    filter_ui(dat = cars2, external = reactiveValues(speed = 4:14), update = TRUE)
  )
  # no labels for update
  expect_snapshot(
    filter_ui(dat = cars2, update = TRUE)
  )
  expect_snapshot(
    filter_ui(dat = iris, update = TRUE)
  )
  expect_snapshot(
    filter_ui(dat = iris, update = TRUE, shinyjs = TRUE)
  )
})

test_that("that server update functions work", {
  # prepare
  input <- list()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  vars <- c("speed", "dist", "logi")

  # the controllers
  hdl1 <- filter_ui(dat = cars2, update = TRUE, shinyjs = TRUE)
  expect_snapshot(hdl1)
  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl1, dat = cars2, show = TRUE)
  )
  # the controllers
  hdl2 <- filter_ui(dat = cars2, external = reactiveValues(speed = 4:14), update = TRUE, shinyjs = TRUE)
  expect_snapshot(hdl2)
  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl2, dat = cars2, show = TRUE)
  )


})

test_that("server can filter", {
  x <- reactiveVal()
  testServer(filter_server, args = list(dat = x), {
    # set
    x(cars2)
    session$flushReact()
    # get
    dataset <- session$getReturned()
    # input
    session$setInputs(speed = range(cars2$speed), dist = range(cars2$dist), logi = "")
    # compare
    expect_equal(dataset(), cars)
  })
})


test_that("module for conditional filtering works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "shinymods", "appdir", "conditional_filter-module")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
