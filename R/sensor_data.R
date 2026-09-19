#' Example Environmental Sensor Dataset
#'
#' A small synthetic dataset representing environmental sensor
#' observations with timestamps, sensor identifiers, geographic
#' coordinates, PM2.5 measurements, and train/test assignments.
#'
#' @format A data frame with 12 observations and 6 variables:
#' \describe{
#'   \item{timestamp}{Timestamp of the sensor observation.}
#'   \item{sensor_id}{Identifier of the environmental sensor.}
#'   \item{latitude}{Latitude of the sensor location in decimal degrees.}
#'   \item{longitude}{Longitude of the sensor location in decimal degrees.}
#'   \item{PM25}{PM2.5 concentration measurement.}
#'   \item{split}{Training or testing assignment.}
#' }
#'
#' @source Synthetic example data created for the sensorLeak package.
#'
#' @examples
#' data(sensor_data)
#' head(sensor_data)
#'
"sensor_data"
