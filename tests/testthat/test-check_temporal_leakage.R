test_that("valid temporal split returns no leakage", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 14:00:00",
      "2026-01-01 16:00:00"
    )),
    split = c("train", "train", "test", "test")
  )

  result <- check_temporal_leakage(
    data,
    time = "timestamp",
    split = "split"
  )

  expect_equal(nrow(result), 0)
})


test_that("test observation before training end is detected", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 11:00:00",
      "2026-01-01 16:00:00"
    )),
    split = c("train", "train", "test", "test")
  )

  result <- check_temporal_leakage(
    data,
    time = "timestamp",
    split = "split"
  )

  expect_equal(nrow(result), 1)
  expect_equal(result$test_row, 3)
})


test_that("test observation at training boundary is detected", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00",
      "2026-01-01 12:00:00"
    )),
    split = c("train", "train", "test")
  )

  result <- check_temporal_leakage(
    data,
    time = "timestamp",
    split = "split"
  )

  expect_equal(nrow(result), 1)
})


test_that("missing columns produce an error", {

  data <- data.frame(
    timestamp = as.POSIXct("2026-01-01 10:00:00"),
    split = "train"
  )

  expect_error(
    check_temporal_leakage(
      data,
      time = "wrong_column",
      split = "split"
    )
  )
})


test_that("non-POSIXct time produces an error", {

  data <- data.frame(
    timestamp = c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00"
    ),
    split = c("train", "test")
  )

  expect_error(
    check_temporal_leakage(
      data,
      time = "timestamp",
      split = "split"
    )
  )
})


test_that("missing time values produce an error", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      NA
    )),
    split = c("train", "test")
  )

  expect_error(
    check_temporal_leakage(
      data,
      time = "timestamp",
      split = "split"
    )
  )
})


test_that("invalid split values produce an error", {

  data <- data.frame(
    timestamp = as.POSIXct(c(
      "2026-01-01 10:00:00",
      "2026-01-01 12:00:00"
    )),
    split = c("train", "validation")
  )

  expect_error(
    check_temporal_leakage(
      data,
      time = "timestamp",
      split = "split"
    )
  )
})
