#' Print a sensor leakage audit
#'
#' Prints a concise summary of the potential validation risks
#' identified by a `sensor_audit` object.
#'
#' @param x An object of class `sensor_audit`.
#' @param ... Additional arguments passed to the method.
#'
#' @return Invisibly returns the audit object.
#'
#' @export
print.sensor_audit <- function(x, ...) {

  cat("Environmental Sensor Leakage Audit\n")
  cat("==================================\n\n")

  temporal_count <- nrow(x$temporal)
  sensor_count <- nrow(x$sensor)
  spatial_count <- nrow(x$spatial)

  cat("Temporal risk: ", temporal_count, " finding(s)\n", sep = "")
  cat("Sensor risk:   ", sensor_count, " sensor(s) shared\n", sep = "")
  cat("Spatial risk:  ", spatial_count, " nearby pair(s)\n", sep = "")

  total_findings <- temporal_count +
    sensor_count +
    spatial_count

  cat("\n")

  if (total_findings == 0) {
    cat("Overall: no potential validation risks detected.\n")
  } else {
    cat("Overall: potential validation risks detected.\n")
  }

  cat("\nUse summary(x) for detailed results.\n")

  invisible(x)
}
