test_that("gui and update gui can be generated", {

  session <- environment()
  session$ns <- NS("testthat")

  # generate gui
  expect_snapshot(
    filter_controllers(cars, session)
  )
  # update gui
  expect_snapshot(
    filter_controllers(cars, session, update = T)
  )
  # use shinyjs
  expect_snapshot(
    filter_controllers(cars, session, update = F, shinyjs = T)
  )
  expect_snapshot(
    filter_controllers(cars, session, update = T, shinyjs = T)
  )
  # external
  expect_snapshot(
    filter_controllers(cars, session, external =  character(0),  update = TRUE)
  )
  # ignore
  expect_snapshot(
    filter_controllers(iris, session, ignore = c("Petal.Width", "Petal.Length"))
  )
  expect_snapshot(
    filter_controllers(iris, session, ignore = c("Petal.Width", "Petal.Length"),
                       update = TRUE)
  )
})

test_that("helpers work", {
  expect_snapshot(
    variable_names(cars)
  )
  expect_snapshot(
    variable_names(cars, "speed")
  )
})

test_that("that server update functions work", {

  # prepare
  input <- list()
  input$speed <- 1:10
  input$dist <- 21:50
  input$logi <- rep(TRUE, 10)
  vars <- c("speed", "dist", "logi")
  session <- environment()
  session$ns <- NS("testthat")

  # the controllers
  hdl1 <- filter_controllers(dat = cars2, session,
                             update = TRUE, shinyjs = TRUE)

  # update gui
  expect_snapshot(
    hdl1
  )

  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl1, dat = cars2, show = TRUE)
  )

  # the observer event (logical)
  expect_snapshot(
    observe_builder("logi", hdl1, dat = cars2, show = TRUE)
  )

  # the controllers
  hdl2 <- filter_controllers(dat = cars2, session, external = "speed",
                             update = TRUE, shinyjs = TRUE)

  # update gui
  expect_snapshot(
    hdl2
  )

  # the observer event
  expect_snapshot(
    observe_builder("speed", hdl2, dat = cars2, show = TRUE)
  )
})

test_that("module for conditional filtering works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip(TRUE)

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "shinymods", "appdir", "conditional_filter-module")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
