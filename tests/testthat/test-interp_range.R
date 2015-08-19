library(fluxr)
context("Interpolation")

test_that("interp_range() returns one or more values without NAs", {
  expect_equal(interp_range(x=1, y=1, max_interval=1, fill=0), 1)
  expect_equal(interp_range(x=c(1, 2), y=c(1, 2), max_interval=1, fill=0), c(1, 2))
  expect_equal(interp_range(x=seq(1, 10), y=seq(1, 10), max_interval=1, fill=0), seq(1, 10))
})

test_that("interp_range() interpolates one run of missing values", {
  expect_equal(interp_range(x=c(1, 2, 3), y=c(1, NA, 2),
                            max_interval=1, fill=0),
               c(1, 1.5, 2))
  expect_equal(interp_range(x=seq(1, 4), y=c(1, NA, NA, 2),
                            max_interval=2, fill=0),
               c(1, 1.33333, 1.66666, 2),
               tolerance=0.0001)
})

test_that("interp_range() does not interpolate over long intervals", {
  expect_equal(interp_range(x=seq(1, 4), y=c(1, NA, NA, 2),
                            max_interval=1, fill=0),
               c(1, 1, 2, 2))

  # takes mean between inner points
  expect_equal(interp_range(x=seq(1, 5), y=c(1, NA, NA, NA, 2),
                            max_interval=1, fill=0),
               c(1, 1, 1.5, 2, 2))

  expect_equal(interp_range(x=seq(1, 6), y=c(1, NA, NA, NA, NA, 2),
                            max_interval=1, fill=0),
               c(1, 1, 1.4, 1.6, 2, 2),
               tolarance=0.0001)
})

test_that("interp_range() fills ends", {
  expect_equal(interp_range(x=seq(1, 4), y=c(NA, NA, 1, 2),
                            max_interval=1, fill=0),
               c(0, 1, 1, 2))
  expect_equal(interp_range(x=seq(1, 5), y=c(NA, NA, NA, 1, 2),
                            max_interval=1, fill=0),
               c(0, 0, 1, 1, 2))
  expect_equal(interp_range(x=seq(1, 5), y=c(NA, NA, NA, 1, 2),
                            max_interval=2, fill=0),
               c(0, 1, 1, 1, 2))
  expect_equal(interp_range(x=seq(1, 5), y=c(NA, NA, NA, 1, 2),
                            max_interval=2, fill=1),
               c(1, 1, 1, 1, 2))

  expect_equal(interp_range(x=seq(1, 4), y=c(1, 2, NA, NA),
                            max_interval=1, fill=0),
               c(1, 2, 2, 0))
  expect_equal(interp_range(x=seq(1, 5), y=c(1, 2, NA, NA, NA),
                            max_interval=1, fill=0),
               c(1, 2, 2, 0, 0))
  expect_equal(interp_range(x=seq(1, 5), y=c(1, 2, NA, NA, NA),
                            max_interval=2, fill=0),
               c(1, 2, 2, 2, 0))
  expect_equal(interp_range(x=seq(1, 5), y=c(1, 2, NA, NA, NA),
                            max_interval=2, fill=1),
               c(1, 2, 2, 2, 1))

  expect_equal(interp_range(x=seq(1, 6), y=c(NA, NA, 1, 2, NA, NA),
                            max_interval=1, fill=0),
               c(0, 1, 1, 2, 2, 0))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, 2, NA, NA, NA),
                            max_interval=1, fill=0),
               c(0, 1, 1, 2, 2, 0, 0))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, 2, NA, NA, NA),
                            max_interval=2, fill=0),
               c(1, 1, 1, 2, 2, 2, 0))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, 2, NA, NA, NA),
                            max_interval=2, fill=1),
               c(1, 1, 1, 2, 2, 2, 1))
})

test_that("interp_range() fills ends and interpolates middle", {
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, NA, NA, NA, 2),
                            max_interval=1, fill=0),
               c(0, 1, 1, 1, 1.5, 2, 2))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, NA, NA, NA, 2),
                            max_interval=2, fill=0),
               c(1, 1, 1, 1, 1.5, 2, 2))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, NA, NA, NA, 2),
                            max_interval=3, fill=0),
               c(1, 1, 1, 1.25, 1.5, 1.75, 2))
  expect_equal(interp_range(x=seq(1, 7), y=c(NA, NA, 1, NA, NA, NA, 2),
                            max_interval=4, fill=0),
               c(1, 1, 1, 1.25, 1.5, 1.75, 2))
  expect_equal(interp_range(x=seq(1, 9), y=c(NA, NA, 1, NA, NA, NA, NA, NA, 2),
                            max_interval=4, fill=0),
               c(1, 1, 1, 1, 1.3333, 1.5, 1.6666, 2, 2),
               tolerance=0.0001)
})



test_that("interp_range() does not interpolate over long intervals", {
  expect_equal(interp_range(x=seq(1, 5), y=c(1, NA, NA, NA, 2),
                            max_interval=1, fill=0),
               c(1, 1, 1.5, 2, 2))
})

test_that("interp_range() throws errors", {
  expect_error(interp_range(x=1, y=c(1, 2), max_interval=1, fill=0),
               'input vectors x and y must have the same length')
  expect_error(interp_range(x=1, y=1, max_interval=0, fill=0),
               'max_interval must be greater than or equal to 0')
  expect_error(interp_range(x=1, y=1, max_interval=-1, fill=0),
               'max_interval must be greater than or equal to 0')
})

