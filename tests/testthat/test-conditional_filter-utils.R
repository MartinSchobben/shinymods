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

  # standard
  expect_snapshot(
    detect_controller(cars, "testthat")
  )
  # standard (reactive like)
  `testdata()` <- cars
  expect_snapshot(
    detect_controller(`testdata()`, "testthat")
  )
  expect_snapshot(
    detect_controller(iris, "testthat")
  )
  # update
  expect_snapshot(
    detect_controller(cars, NULL, update = T)
  )
  expect_snapshot(
    detect_controller(iris, NULL, update = T)
  )
  # custom labels
  expect_snapshot(
    detect_controller(cars2, "testthat", c(speed = "a", dist = "b"))
  )
  # remove external controllers
  expect_snapshot(
    detect_controller(iris, "testthat", external = "Species")
  )

})

test_that("logical_controller works", {

  # standard
  expect_snapshot(
    logical_controller(cars2, "testthat")
  )
  # custom label
  expect_snapshot(
    logical_controller(cars2, "testthat", labels = "a")
  )
  # update
  expect_snapshot(
    logical_controller(cars2, "testthat", labels = "a", update = TRUE)
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
