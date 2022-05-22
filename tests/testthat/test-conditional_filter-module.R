test_that("gui and update gui can be generated", {

  # generate gui
   expect_snapshot(
     filter_ui(cars2, "testthat")
   )
  # update gui
  expect_snapshot(
    filter_ui(cars2, "testthat", update = T)
  )

  # use shinyjs
  expect_snapshot(
    filter_ui(cars2, "testthat", update = T, shinyjs = T)
  )
})

test_that("that server update functions work", {

  # load shinjs if needed
  if (isFALSE(requireNamespace("shinyjs"))) library(shinyjs)

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
  # the observer event (logical)
  expect_snapshot(
    observe_builder("logi", hdl1, dat = cars2, show = TRUE)
  )
  # the controllers
  hdl2 <- filter_ui(dat = cars2, external = "speed",
                    update = TRUE, shinyjs = TRUE)
  expect_snapshot(hdl2)
  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl2, dat = cars2, show = TRUE)
  )
})

test_that("server can filter", {

  # load shinjs if needed
  if (isFALSE(requireNamespace("shinyjs"))) library(shinyjs)

  x <- reactiveVal()
  testServer(filter_server, args = list(dat = x, ignore = ""), {
    # set
    x(cars2)
    session$flushReact()
    # get
    dataset <- session$getReturned()
    # input
    session$setInputs(speed = range(cars2$speed), dist = range(cars2$dist))
    # compare
    #print(dataset())
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
