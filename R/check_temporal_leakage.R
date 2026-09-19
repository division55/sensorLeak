#' Check for temporal leakage
#'
#' Identifies test observations that occur before the end of the
#' training period. Such observations may indicate an invalid
#' temporal validation setup.
#'
#' @param data A data frame containing timestamped observations.
#' @param time A character string giving the timestamp column.
#' @param split A character string giving the column identifying
#'   training and testing observations.
#'
#' @return A data frame containing test observations that occur
#'   before the end of the training period.
#'
#' @examples
#' data <- data.frame(
#'   timestamp = as.POSIXct(
#'     c(
#'       "2026-01-01 10:00:00",
#'       "2026-01-01 12:00:00",
#'       "2026-01-01 11:00:00",
#'       "2026-01-01 15:00:00"
#'     )
#'   ),
#'   split = c("train", "train", "test", "test")
#' )
#'
#' check_temporal_leakage(
#'   data,
#'   time = "timestamp",
#'   split = "split"
#' )
#'
#' @export
check_temporal_leakage <- function(data, time, split) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required_columns <- c(time, split)

  if (!all(required_columns %in% names(data))) {
    missing_columns <- required_columns[
      !required_columns %in% names(data)
    ]

    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!inherits(data[[time]], "POSIXct")) {
    stop("time column must be POSIXct.")
  }

  if (anyNA(data[[time]])) {
    stop("time column cannot contain missing values.")
  }

  if (!all(data[[split]] %in% c("train", "test"))) {
    stop("split must contain only 'train' and 'test'.")
  }

  train_times <- data[[time]][data[[split]] == "train"]
  test_times <- data[[time]][data[[split]] == "test"]

  if (length(train_times) == 0 || length(test_times) == 0) {
    return(data.frame(
      test_row = integer(),
      test_time = as.POSIXct(character()),
      training_end = as.POSIXct(character())
    ))
  }

  training_end <- max(train_times)

  problematic <- which(
    data[[split]] == "test" &
      data[[time]] <= training_end
  )

  if (length(problematic) == 0) {
    return(data.frame(
      test_row = integer(),
      test_time = as.POSIXct(character()),
      training_end = as.POSIXct(character())
    ))
  }

  data.frame(
    test_row = problematic,
    test_time = data[[time]][problematic],
    training_end = training_end
  )
}
