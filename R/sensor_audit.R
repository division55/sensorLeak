#' Audit environmental sensor data for leakage risks
#'
#' Runs multiple leakage checks on environmental sensor data
#' and combines their findings into a single audit result.
#'
#' @param data A data frame containing environmental sensor data.
#' @param time A character string giving the timestamp column.
#' @param sensor A character string giving the sensor identifier column.
#' @param split A character string giving the train/test split column.
#' @param lat A character string giving the latitude column.
#' @param lon A character string giving the longitude column.
#' @param spatial_threshold A numeric value giving the spatial
#'   proximity threshold in kilometres.
#'
#' @return An object of class `sensor_audit` containing the
#'   results of the leakage checks.
#'
#' @examples
#' data <- data.frame(
#'   timestamp = as.POSIXct(c(
#'     "2026-01-01 10:00:00",
#'     "2026-01-01 12:00:00",
#'     "2026-01-01 14:00:00",
#'     "2026-01-01 16:00:00"
#'   )),
#'   sensor_id = c("S1", "S1", "S2", "S2"),
#'   latitude = c(13.08, 13.08, 13.20, 13.30),
#'   longitude = c(80.27, 80.27, 80.30, 80.40),
#'   split = c("train", "train", "test", "test")
#' )
#'
#' audit <- sensor_audit(
#'   data,
#'   time = "timestamp",
#'   sensor = "sensor_id",
#'   split = "split",
#'   lat = "latitude",
#'   lon = "longitude"
#' )
#'
#' @export
sensor_audit <- function(
    data,
    time,
    sensor,
    split,
    lat,
    lon,
    spatial_threshold = 1) {

  if (!is.data.frame(data)) {
    stop("data must be a data frame.")
  }

  temporal <- check_temporal_leakage(
    data,
    time = time,
    split = split
  )

  sensor_result <- check_sensor_leakage(
    data,
    sensor = sensor,
    split = split
  )

  spatial <- check_spatial_leakage(
    data,
    lat = lat,
    lon = lon,
    split = split,
    threshold = spatial_threshold
  )

  result <- list(
    temporal = temporal,
    sensor = sensor_result,
    spatial = spatial
  )

  class(result) <- "sensor_audit"

  result
}
