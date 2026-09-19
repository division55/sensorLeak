test_that("summary.sensor_audit returns a summary object", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 14:00:00",
      "2026-01-01 16:00:00"
    )),
    sensor_id = c("S1", "S1", "S2", "S2"),
    latitude = c(
      13.0827,
      13.0830,
      13.2000,
      13.3000
    ),
    longitude = c(
      80.2707,
      80.2710,
      80.3000,
      80.4000
    ),
    split = c("train", "train", "test", "test")
  )

  audit <- sensor_audit(
    data,
    time = "timestamp",
    sensor = "sensor_id",
    split = "split",
    lat = "latitude",
    lon = "longitude"
  )

  result <- summary(audit)

  expect_s3_class(
    result,
    "sensor_audit_summary"
  )

  expect_true(
    is.numeric(result$temporal_findings)
  )

  expect_true(
    is.numeric(result$shared_sensors)
  )

  expect_true(
    is.numeric(result$spatial_pairs)
  )
})


test_that("summary contains detailed audit results", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 14:00:00",
      "2026-01-01 16:00:00"
    )),
    sensor_id = c("S1", "S1", "S1", "S2"),
    latitude = c(
      13.0827,
      13.0830,
      13.0832,
      13.3000
    ),
    longitude = c(
      80.2707,
      80.2710,
      80.2712,
      80.4000
    ),
    split = c("train", "train", "test", "test")
  )

  audit <- sensor_audit(
    data,
    time = "timestamp",
    sensor = "sensor_id",
    split = "split",
    lat = "latitude",
    lon = "longitude"
  )

  result <- summary(audit)

  expect_true(is.data.frame(result$temporal))
  expect_true(is.data.frame(result$sensor))
  expect_true(is.data.frame(result$spatial))
})
