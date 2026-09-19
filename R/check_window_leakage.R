#' Check for overlapping temporal windows
#'
#' Detects temporal overlap between training and testing windows
#' within environmental sensor data.
#'
#' @param data A data frame containing temporal windows.
#' @param start A character string giving the column containing
#'   window start times.
#' @param end A character string giving the column containing
#'   window end times.
#' @param split A character string giving the column identifying
#'   training and testing observations.
#' @param sensor A character string giving the column identifying
#'   the environmental sensor.
#'
#' @return A data frame containing detected overlapping windows.
#'
#' @examples
#' data <- data.frame(
#'   sensor_id = c("S1", "S1"),
#'   window_start = as.POSIXct(
#'     c("2026-01-01 10:00:00", "2026-01-01 13:00:00")
#'   ),
#'   window_end = as.POSIXct(
#'     c("2026-01-01 14:00:00", "2026-01-01 18:00:00")
#'   ),
#'   split = c("train", "test")
#' )
#'
#' check_window_leakage(
#'   data,
#'   start = "window_start",
#'   end = "window_end",
#'   split = "split",
#'   sensor = "sensor_id"
#' )
#'
#' @export
check_window_leakage <- function(
    data,
    start,
    end,
    split,
    sensor) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required_columns <- c(start, end, split, sensor)

  if (!all(required_columns %in% names(data))) {
    missing_columns <- required_columns[
      !required_columns %in% names(data)
    ]

    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!inherits(data[[start]], "POSIXct") ||
      !inherits(data[[end]], "POSIXct")) {
    stop("start and end columns must be POSIXct.")
  }

  if (anyNA(data[[start]]) || anyNA(data[[end]])) {
    stop("start and end columns cannot contain missing values.")
  }

  if (any(data[[start]] >= data[[end]])) {
    stop("Each window start time must be before its end time.")
  }

  if (anyNA(data[[sensor]])) {
    stop("sensor column cannot contain missing values.")
  }

  if (!all(data[[split]] %in% c("train", "test"))) {
    stop("split must contain only 'train' and 'test'.")
  }

  train_rows <- which(data[[split]] == "train")
  test_rows <- which(data[[split]] == "test")

  train <- data[train_rows, , drop = FALSE]
  test <- data[test_rows, , drop = FALSE]

  if (nrow(train) == 0 || nrow(test) == 0) {
    return(data.frame(
      sensor = character(),
      train_row = integer(),
      test_row = integer(),
      overlap_start = as.POSIXct(character()),
      overlap_end = as.POSIXct(character()),
      overlap_duration = numeric()
    ))
  }

  overlaps <- list()

  for (i in seq_len(nrow(train))) {

    for (j in seq_len(nrow(test))) {

      if (train[[sensor]][i] != test[[sensor]][j]) {
        next
      }

      overlap_start <- max(
        train[[start]][i],
        test[[start]][j]
      )

      overlap_end <- min(
        train[[end]][i],
        test[[end]][j]
      )

      if (overlap_start < overlap_end) {

        overlaps[[length(overlaps) + 1]] <- data.frame(
          sensor = train[[sensor]][i],
          train_row = train_rows[i],
          test_row = test_rows[j],
          overlap_start = overlap_start,
          overlap_end = overlap_end,
          overlap_duration = as.numeric(
            difftime(
              overlap_end,
              overlap_start,
              units = "secs"
            )
          )
        )
      }
    }
  }

  if (length(overlaps) == 0) {
    return(data.frame(
      sensor = character(),
      train_row = integer(),
      test_row = integer(),
      overlap_start = as.POSIXct(character()),
      overlap_end = as.POSIXct(character()),
      overlap_duration = numeric()
    ))
  }

  do.call(rbind, overlaps)
}
