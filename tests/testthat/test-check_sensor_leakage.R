test_that("sensors only in one split return no leakage", {

  data <- data.frame(
    sensor_id = c("S1", "S2", "S3", "S4"),
    split = c("train", "train", "test", "test")
  )

  result <- check_sensor_leakage(
    data,
    sensor = "sensor_id",
    split = "split"
  )

  expect_equal(nrow(result), 0)
})


test_that("shared sensors are detected", {

  data <- data.frame(
    sensor_id = c("S1", "S2", "S1", "S3"),
    split = c("train", "train", "test", "test")
  )

  result <- check_sensor_leakage(
    data,
    sensor = "sensor_id",
    split = "split"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$sensor, "S1")
})


test_that("observation counts are correct", {

  data <- data.frame(
    sensor_id = c("S1", "S1", "S1", "S2", "S2"),
    split = c("train", "train", "test", "train", "test")
  )

  result <- check_sensor_leakage(
    data,
    sensor = "sensor_id",
    split = "split"
  )

  s1 <- result[result$sensor == "S1", ]

  expect_equal(s1$train_observations, 2)
  expect_equal(s1$test_observations, 1)

  s2 <- result[result$sensor == "S2", ]

  expect_equal(s2$train_observations, 1)
  expect_equal(s2$test_observations, 1)
})


test_that("missing columns produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S2"),
    split = c("train", "test")
  )

  expect_error(
    check_sensor_leakage(
      data,
      sensor = "wrong_column",
      split = "split"
    )
  )
})


test_that("invalid split values produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S2"),
    split = c("train", "validation")
  )

  expect_error(
    check_sensor_leakage(
      data,
      sensor = "sensor_id",
      split = "split"
    )
  )
})


test_that("missing sensor values produce an error", {

  data <- data.frame(
    sensor_id = c("S1", NA),
    split = c("train", "test")
  )

  expect_error(
    check_sensor_leakage(
      data,
      sensor = "sensor_id",
      split = "split"
    )
  )
})
