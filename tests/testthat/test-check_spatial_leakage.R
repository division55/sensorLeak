test_that("distant observations return no spatial risk", {

  data <- data.frame(
    latitude = c(13.0827, 13.5000),
    longitude = c(80.2707, 80.7000),
    split = c("train", "test")
  )

  result <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 1
  )

  expect_equal(nrow(result), 0)
})


test_that("nearby observations are detected", {

  data <- data.frame(
    latitude = c(13.0827, 13.0830),
    longitude = c(80.2707, 80.2710),
    split = c("train", "test")
  )

  result <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 1
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$train_row, 1)
  expect_equal(result$test_row, 2)
  expect_true(result$distance_km <= 1)
})


test_that("original row numbers are returned", {

  data <- data.frame(
    latitude = c(13.0827, 15.0000, 13.0830),
    longitude = c(80.2707, 80.0000, 80.2710),
    split = c("train", "train", "test")
  )

  result <- check_spatial_leakage(
    data,
    lat = "latitude",
    lon = "longitude",
    split = "split",
    threshold = 1
  )

  expect_equal(result$train_row, 1)
  expect_equal(result$test_row, 3)
})


test_that("missing columns produce an error", {

  data <- data.frame(
    latitude = 13.0827,
    longitude = 80.2707,
    split = "train"
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "wrong_column",
      lon = "longitude",
      split = "split"
    )
  )
})


test_that("non-numeric coordinates produce an error", {

  data <- data.frame(
    latitude = "13.0827",
    longitude = 80.2707,
    split = "train"
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )
  )
})


test_that("invalid latitude produces an error", {

  data <- data.frame(
    latitude = 100,
    longitude = 80.2707,
    split = "train"
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )
  )
})


test_that("invalid longitude produces an error", {

  data <- data.frame(
    latitude = 13.0827,
    longitude = 200,
    split = "train"
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )
  )
})


test_that("invalid threshold produces an error", {

  data <- data.frame(
    latitude = 13.0827,
    longitude = 80.2707,
    split = "train"
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split",
      threshold = -1
    )
  )
})


test_that("missing coordinates produce an error", {

  data <- data.frame(
    latitude = c(13.0827, NA),
    longitude = c(80.2707, 80.2710),
    split = c("train", "test")
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )
  )
})


test_that("invalid split values produce an error", {

  data <- data.frame(
    latitude = c(13.0827, 13.0830),
    longitude = c(80.2707, 80.2710),
    split = c("train", "validation")
  )

  expect_error(
    check_spatial_leakage(
      data,
      lat = "latitude",
      lon = "longitude",
      split = "split"
    )
  )
})
