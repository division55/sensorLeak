#' Check for spatial proximity between training and testing data
#'
#' Identifies training and testing observations that are within a
#' specified geographic distance of each other. Close observations
#' may represent a validation risk when a model is intended to
#' generalize to geographically independent locations.
#'
#' @param data A data frame containing geographic observations.
#' @param lat A character string giving the latitude column.
#' @param lon A character string giving the longitude column.
#' @param split A character string giving the column identifying
#'   training and testing observations.
#' @param threshold A numeric value giving the maximum distance
#'   in kilometres for observations to be considered spatially close.
#'
#' @return A data frame containing pairs of spatially close
#'   training and testing observations.
#'
#' @examples
#' data <- data.frame(
#'   latitude = c(13.0827, 13.0830, 13.2000, 13.3000),
#'   longitude = c(80.2707, 80.2710, 80.3000, 80.4000),
#'   split = c("train", "test", "train", "test")
#' )
#'
#' check_spatial_leakage(
#'   data,
#'   lat = "latitude",
#'   lon = "longitude",
#'   split = "split",
#'   threshold = 1
#' )
#'
#' @export
check_spatial_leakage <- function(
    data,
    lat,
    lon,
    split,
    threshold = 1) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  required_columns <- c(lat, lon, split)

  if (!all(required_columns %in% names(data))) {
    missing_columns <- required_columns[
      !required_columns %in% names(data)
    ]

    stop(
      "Missing required columns: ",
      paste(missing_columns, collapse = ", ")
    )
  }

  if (!is.numeric(data[[lat]]) || !is.numeric(data[[lon]])) {
    stop("lat and lon columns must be numeric.")
  }

  if (anyNA(data[[lat]]) || anyNA(data[[lon]])) {
    stop("lat and lon columns cannot contain missing values.")
  }

  if (any(data[[lat]] < -90 | data[[lat]] > 90)) {
    stop("latitude values must be between -90 and 90.")
  }

  if (any(data[[lon]] < -180 | data[[lon]] > 180)) {
    stop("longitude values must be between -180 and 180.")
  }

  if (!is.numeric(threshold) ||
      length(threshold) != 1 ||
      is.na(threshold) ||
      threshold < 0) {
    stop("threshold must be a single non-negative numeric value.")
  }

  if (!all(data[[split]] %in% c("train", "test"))) {
    stop("split must contain only 'train' and 'test'.")
  }

  train_rows <- which(data[[split]] == "train")
  test_rows <- which(data[[split]] == "test")

  if (length(train_rows) == 0 || length(test_rows) == 0) {
    return(data.frame(
      train_row = integer(),
      test_row = integer(),
      distance_km = numeric()
    ))
  }

  haversine_distance <- function(
    lat1,
    lon1,
    lat2,
    lon2) {

    radius <- 6371

    lat1 <- lat1 * pi / 180
    lat2 <- lat2 * pi / 180
    dlat <- lat2 - lat1
    dlon <- (lon2 - lon1) * pi / 180

    a <- sin(dlat / 2)^2 +
      cos(lat1) * cos(lat2) * sin(dlon / 2)^2

    2 * radius * asin(sqrt(a))
  }

  results <- list()

  for (i in seq_along(train_rows)) {

    train_row <- train_rows[i]

    for (j in seq_along(test_rows)) {

      test_row <- test_rows[j]

      distance <- haversine_distance(
        data[[lat]][train_row],
        data[[lon]][train_row],
        data[[lat]][test_row],
        data[[lon]][test_row]
      )

      if (distance <= threshold) {

        results[[length(results) + 1]] <- data.frame(
          train_row = train_row,
          test_row = test_row,
          distance_km = distance
        )
      }
    }
  }

  if (length(results) == 0) {
    return(data.frame(
      train_row = integer(),
      test_row = integer(),
      distance_km = numeric()
    ))
  }

  do.call(rbind, results)
}
