test_that("col_vals works", {

  # numeric
  expect_snapshot(
    col_vals(cars$speed, rlang::expr(cars$speed), .GlobalEnv)
  )
  # character
  expect_snapshot(
    col_vals(iris$Species, rlang::expr(iris$Species), .GlobalEnv)
  )
  # logical
  expect_equal(
    col_vals(cars2$high_speed),
    NULL
  )
})

test_that("detect_controller works", {

  session <- environment()
  session$ns <- NS("testthat")

  # standard
  expect_snapshot(
    detect_controller(cars, session)
  )
  # standard (reactive like)
  `testdata()` <- cars
  expect_snapshot(
    detect_controller(`testdata()`, session)
  )
  expect_snapshot(
    detect_controller(iris, session)
  )
  # update
  expect_snapshot(
    detect_controller(cars, session, update = T)
  )
  expect_snapshot(
    detect_controller(iris, session, update = T)
  )
  # custom labels
  expect_snapshot(
    detect_controller(cars2, session, c(speed = "a", dist = "b"))
  )
  # remove external controllers
  expect_snapshot(
    detect_controller(iris, session, external = "Species")
  )

})

test_that("logical_controller works", {

  session <- environment()
  session$ns <- NS("testthat")

  # standard
  expect_snapshot(
    logical_controller(cars2, session)
  )
  # custom label
  expect_snapshot(
    logical_controller(cars2, session, labels = "a")
  )
  # update
  expect_snapshot(
    logical_controller(cars2, session, labels = "a", update = TRUE)
  )
})

test_that("switch_controller works", {

  # standard
  expect_snapshot(
    switch_controller(cars)
  )
  expect_snapshot(
    switch_controller(iris)
  )
})


test_that("helpers work", {

  # col specs
  expect_snapshot(
    col_spec(cars2, "dist", "speed")
  )
  expect_snapshot(
    col_spec(cars2, "dist")
  )
  expect_snapshot(
    col_spec(cars2)
  )
  expect_snapshot(
    col_spec(DNase)
  )
})
