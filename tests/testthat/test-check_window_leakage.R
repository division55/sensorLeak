test_that("non-overlapping windows return no leakage", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 15:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  result <- check_window_leakage(
    data,
    start = "window_start",
    end = "window_end",
    split = "split",
    sensor = "sensor_id"
  )

  expect_equal(nrow(result), 0)
})


test_that("overlapping windows are detected", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  result <- check_window_leakage(
    data,
    start = "window_start",
    end = "window_end",
    split = "split",
    sensor = "sensor_id"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$overlap_duration, 3600)
})


test_that("overlapping windows from different sensors are ignored", {

  data <- data.frame(
    sensor_id = c("S1", "S2"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  result <- check_window_leakage(
    data,
    start = "window_start",
    end = "window_end",
    split = "split",
    sensor = "sensor_id"
  )

  expect_equal(nrow(result), 0)
})


test_that("missing columns produce an error", {

  data <- data.frame(
    sensor_id = "S1",
    window_start = as.POSIXct("2026-01-01 10:00:00"),
    window_end = as.POSIXct("2026-01-01 14:00:00")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})


test_that("invalid split values produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("training", "test")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})


test_that("detected overlap contains the correct sensor", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  result <- check_window_leakage(
    data,
    start = "window_start",
    end = "window_end",
    split = "split",
    sensor = "sensor_id"
  )

  expect_equal(result$sensor, "S1")
  expect_equal(result$overlap_duration, 3600)
})
test_that("non-POSIXct time columns produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = c(
      "2026-01-01 10:00:00",
      "2026-01-01 13:00:00"
    ),
    window_end = c(
      "2026-01-01 14:00:00",
      "2026-01-01 18:00:00"
    ),
    split = c("train", "test")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})


test_that("missing time values produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        NA
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})


test_that("invalid window boundaries produce an error", {

  data <- data.frame(
    sensor_id = c("S1", "S1"),
    window_start = as.POSIXct(
      c(
        "2026-01-01 15:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})


test_that("missing sensor values produce an error", {

  data <- data.frame(
    sensor_id = c("S1", NA),
    window_start = as.POSIXct(
      c(
        "2026-01-01 10:00:00",
        "2026-01-01 13:00:00"
      )
    ),
    window_end = as.POSIXct(
      c(
        "2026-01-01 14:00:00",
        "2026-01-01 18:00:00"
      )
    ),
    split = c("train", "test")
  )

  expect_error(
    check_window_leakage(
      data,
      start = "window_start",
      end = "window_end",
      split = "split",
      sensor = "sensor_id"
    )
  )
})
