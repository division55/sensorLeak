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

  spatial_pairs_within(
    lat = data[[lat]],
    lon = data[[lon]],
    train_rows = train_rows,
    test_rows = test_rows,
    threshold = threshold
  )
}

#' Find train/test pairs within a distance threshold
#'
#' Computes haversine distances between every training and testing
#' observation in blocks, so memory use stays bounded, and returns the
#' pairs within `threshold` kilometres, ordered by `train_row` and then
#' `test_row`.
#'
#' @param lat,lon Numeric vectors of latitude and longitude in degrees,
#'   one element per row of the original data.
#' @param train_rows,test_rows Integer row indices of the training and
#'   testing observations.
#' @param threshold Maximum distance in kilometres.
#' @param max_cells Approximate maximum size of one distance matrix.
#'
#' @return A data frame with columns `train_row`, `test_row`, and
#'   `distance_km`.
#'
#' @noRd
spatial_pairs_within <- function(
    lat,
    lon,
    train_rows,
    test_rows,
    threshold,
    max_cells = 2e6) {

  radius <- 6371

  lat_train <- lat[train_rows] * pi / 180
  lon_train <- lon[train_rows]
  lat_test <- lat[test_rows] * pi / 180
  lon_test <- lon[test_rows]

  cos_test <- cos(lat_test)

  block_size <- max(1L, floor(max_cells / length(test_rows)))
  starts <- seq(1L, length(train_rows), by = block_size)

  blocks <- lapply(starts, function(start) {

    idx <- start:min(start + block_size - 1L, length(train_rows))

    dlat <- outer(lat_train[idx], lat_test, function(a, b) b - a)
    dlon <- outer(
      lon_train[idx],
      lon_test,
      function(a, b) (b - a) * pi / 180
    )

    a <- sin(dlat / 2)^2 +
      outer(cos(lat_train[idx]), cos_test) * sin(dlon / 2)^2

    distance <- 2 * radius * asin(sqrt(pmin(a, 1)))

    hits <- which(distance <= threshold, arr.ind = TRUE)

    if (nrow(hits) == 0) {
      return(NULL)
    }

    hits <- hits[order(hits[, 1], hits[, 2]), , drop = FALSE]

    data.frame(
      train_row = train_rows[idx[hits[, 1]]],
      test_row = test_rows[hits[, 2]],
      distance_km = distance[hits]
    )
  })

  blocks <- blocks[!vapply(blocks, is.null, logical(1))]

  if (length(blocks) == 0) {
    return(data.frame(
      train_row = integer(),
      test_row = integer(),
      distance_km = numeric()
    ))
  }

  result <- do.call(rbind, blocks)
  rownames(result) <- NULL

  result
}
