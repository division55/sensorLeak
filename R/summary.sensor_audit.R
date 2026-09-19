#' Summarize a sensor leakage audit
#'
#' Provides detailed summary information for a `sensor_audit`
#' object.
#'
#' @param object An object of class `sensor_audit`.
#' @param ... Additional arguments passed to the method.
#'
#' @return An object of class `sensor_audit_summary`.
#'
#' @export
summary.sensor_audit <- function(object, ...) {

  temporal_count <- nrow(object$temporal)
  sensor_count <- nrow(object$sensor)
  spatial_count <- nrow(object$spatial)

  result <- list(
    temporal_findings = temporal_count,
    shared_sensors = sensor_count,
    spatial_pairs = spatial_count,
    temporal = object$temporal,
    sensor = object$sensor,
    spatial = object$spatial
  )

  class(result) <- "sensor_audit_summary"

  result
}
