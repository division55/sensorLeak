test_that("sensor_audit returns a sensor_audit object", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 14:00:00",
      "2026-01-01 16:00:00"
    )),
    sensor_id = c("S1", "S1", "S2", "S2"),
    latitude = c(13.0827, 13.0830, 13.2000, 13.3000),
    longitude = c(80.2707, 80.2710, 80.3000, 80.4000),
    split = c("train", "train", "test", "test")
  )

  result <- sensor_audit(
    data,
    time = "timestamp",
    sensor = "sensor_id",
    split = "split",
    lat = "latitude",
    lon = "longitude"
  )

  expect_s3_class(result, "sensor_audit")
})


test_that("sensor_audit contains all leakage checks", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 14:00:00",
      "2026-01-01 16:00:00"
    )),
    sensor_id = c("S1", "S1", "S1", "S2"),
    latitude = c(13.0827, 13.0830, 13.0832, 13.3000),
    longitude = c(80.2707, 80.2710, 80.2712, 80.4000),
    split = c("train", "train", "test", "test")
  )

  result <- sensor_audit(
    data,
    time = "timestamp",
    sensor = "sensor_id",
    split = "split",
    lat = "latitude",
    lon = "longitude"
  )

  expect_true("temporal" %in% names(result))
  expect_true("sensor" %in% names(result))
  expect_true("spatial" %in% names(result))
})


test_that("sensor_audit accepts a spatial threshold", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 14:00:00"
    )),
    sensor_id = c("S1", "S2"),
    latitude = c(13.0827, 13.0830),
    longitude = c(80.2707, 80.2710),
    split = c("train", "test")
  )

  result <- sensor_audit(
    data,
    time = "timestamp",
    sensor = "sensor_id",
    split = "split",
    lat = "latitude",
    lon = "longitude",
    spatial_threshold = 1
  )

  expect_true(is.data.frame(result$spatial))
})
