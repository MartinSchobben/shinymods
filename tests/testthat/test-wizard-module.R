test_that("module for conditional filtering works", {
  # Don't run these tests on the CRAN build servers
  skip_on_cran()
  skip_on_ci()
  skip_if_offline()
  skip(TRUE)

  # Use compareImages=FALSE because the expected image screenshots were created
  # on a Mac, and they will differ from screenshots taken on the CI platform,
  # which runs on Linux.
  appdir <- system.file(package = "shinymods", "appdir", "wizard-module")
  shinytest::expect_pass(shinytest::testApp(appdir, compareImages = FALSE))
})
