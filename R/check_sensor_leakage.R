#' Check for sensors shared between training and testing data
#'
#' Identifies environmental sensors that occur in both training
#' and testing observations. Shared sensors may represent a
#' validation risk when the intended model should generalize
#' to unseen sensors.
#'
#' @param data A data frame containing sensor observations.
#' @param sensor A character string giving the column identifying
#'   the environmental sensor.
#' @param split A character string giving the column identifying
#'   training and testing observations.
#'
#' @return A data frame containing sensors present in both splits,
#'   together with their observation counts.
#'
#' @examples
#' data <- data.frame(
#'   sensor_id = c("S1", "S2", "S1", "S3"),
#'   split = c("train", "train", "test", "test")
#' )
#'
#' check_sensor_leakage(
#'   data,
#'   sensor = "sensor_id",
#'   split = "split"
#' )
#'
#' @export
check_sensor_leakage <- function(data, sensor, split) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required_columns <- c(sensor, split)

  if (!all(required_columns %in% names(data))) {
    missing_columns <- required_columns[
      !required_columns %in% names(data)
    ]

    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (anyNA(data[[sensor]])) {
    stop("sensor column cannot contain missing values.")
  }

  if (!all(data[[split]] %in% c("train", "test"))) {
    stop("split must contain only 'train' and 'test'.")
  }

  train_sensors <- unique(data[[sensor]][data[[split]] == "train"])
  test_sensors <- unique(data[[sensor]][data[[split]] == "test"])

  shared_sensors <- intersect(train_sensors, test_sensors)

  if (length(shared_sensors) == 0) {
    return(data.frame(
      sensor = character(),
      train_observations = integer(),
      test_observations = integer()
    ))
  }

  result <- lapply(shared_sensors, function(x) {

    data.frame(
      sensor = x,
      train_observations = sum(
        data[[sensor]] == x & data[[split]] == "train"
      ),
      test_observations = sum(
        data[[sensor]] == x & data[[split]] == "test"
      )
    )
  })

  do.call(rbind, result)
}
